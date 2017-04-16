;   Copyright (c) Antony Omeri. All rights reserved.

;;;; -----------------------------------------------------------------------
;;;; Database Wrapper
;;;; -----------------------------------------------------------------------
(ns com.omexit.common.db
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [com.omexit.common.config :as config]
            [clojure.data.json])
  (:import [com.mchange.v2.c3p0 ComboPooledDataSource]))

;; Initialize function
(defn initialize-datasource []
  (def db-spec (let [cpds (doto (ComboPooledDataSource.)
                            (.setJdbcUrl config/jdbc-url)
                            (.setUser config/db-user)
                            (.setPassword config/db-password)
                            (.setDriverClass config/jdbc-driver))]
                 {:datasource cpds}))

  (def pooled-db (delay db-spec))

  (log/info "Datasource Initialized.."))

(defn db-connection [] @pooled-db)

(declare lispify-keys)

(defn execute-query
  "Executes a given SQL statement and returns the results as is to the caller function"
  [sql-query & params]
  (log/debugf "executeQuery(\"%s\", params=%s)" sql-query params)
  (jdbc/with-db-connection [conn (db-connection)]
                           (jdbc/execute! conn (if (empty? params)
                                                 [sql-query]
                                                 (into [] (conj params sql-query))))))

(defn get-customer-info
  "Returns a MIFOS Mobile customer info"
  [customer]
  (jdbc/with-db-connection [conn (db-connection)]
                           (jdbc/execute! conn ["call proc_get_customer_info(?, @client_info)" customer])
                           (if-let [result (jdbc/query conn
                                                    ["select @client_info client_info;"]
                                                    :result-set-fn first)]
                             (-> (String. (result :client_info))
                                 (clojure.data.json/read-str :key-fn keyword)
                                 lispify-keys)
                             {})))

(defn update-customer-info
  "Updated a customer's info"
  [customer info]
  (jdbc/with-db-connection [conn (db-connection)]
                           (if-let [updated (first (jdbc/update! conn :tbl_customers info ["customer_msisdn = ?" customer]))]
                             (= updated 1)
                             (do
                               (log/errorf "!updateCustomerInfo(%s,%s) -> 0 updated" customer info)
                               false))))

(defn get-loan-products
  "Returns a map of the loan products on offer"
  []
  (jdbc/with-db-connection [conn (db-connection)]
                           (if-let [result (jdbc/query conn ["select product_id, product_name from tbl_loan_products"])]
                             (into {}
                                   (map (fn [{:keys [product_name product_id]}]
                                          {product_name product_id})
                                        result))
                             (throw (Exception. "!noLoanProductsConfigured")))))

;; ----------------------------------------------------------------------------------------------------
;; Utility functions
;; ----------------------------------------------------------------------------------------------------
(defn lispify-keys
  [sql-map]
  (into {}
        (map (fn [[k v]]
               (let [k (clojure.string/replace (name k) "_" "-")
                     v (condp = (type v)
                         org.postgresql.jdbc4.Jdbc4Array (apply hash-set (seq (.getArray v)))
                         v)]
                 {(keyword k) v})) sql-map)))

