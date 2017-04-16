;
(ns ^{:doc "Utility functions"
      :author "Antony Omeri"}
  com.omexit.common.utils
  (:require [com.omexit.common.config :as config]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
    [clojure.math.numeric-tower :as math]
            [digest]))

(def status-codes {200 "Success"
                   201 "Failed"
                   202 "Accepted"
                   400 "Bad Request"
                   404 "Transaction Not Found"
                   100 "Processing"
                   500 "Internal Application Error"})

(defn fn-mk-response-map
  "fn shortcut to make http-kit response body"
  [status-code & ext-data]
  {:status 200 :content-type "application/json"
   :body (let [status {:status status-code
                       :status-msg (status-codes status-code)}]
           (if-not (empty? ext-data)
             (conj status {:ext-data (first ext-data)})
             status))})

(defn fn-mk-response
  "fn that given a http-kit response body transforms body to a json string"
  [resp]
  (into resp {:body (json/write-str (:body resp))}))

(defn with-request-params
  [request bindings to-do])

(declare ->subscriber-no ->msisdn)

(defn parse-params-string
  "Returns a map of request parameters"
  [request bindings]
  (or (let [request-string (java.net.URLDecoder/decode request)
            params-v (clojure.string/split request-string #"&")
            params (into {} (map (fn [param]
                                   (let [param-m (clojure.string/split param #"=")]
                                     (apply hash-map (if (= (count param-m) 2)
                                                       param-m
                                                       (conj param-m "")))))
                                 params-v))]
        (when params
          (into {} (map (fn [[var key type]]
                          (if-let [value (params key)]
                            {var (condp = type
                                   :subscriber (->subscriber-no value)
                                   :int32 (Integer/valueOf value)
                                   :int64 (Long/valueOf value)
                                   :string value
                                   value)}))
                        bindings))))
      {}))

(defn get-message
  "Returns a string with parameter keys substituted with actual values"
  [message parameters]
  (when (empty? parameters)
    (throw (Exception. "!get-message() - > empty parameters passed..")))
  (let [parameters (into {} (map (fn [[k v]] {(str k) (str v)}) parameters))
        regex (re-pattern (apply str (interpose "|" (map #(str %) (keys parameters)))))]
    (clojure.string/replace message regex parameters)))

;; ----------------------------------------------------------------------------
;; Parsers
;; ----------------------------------------------------------------------------
(defn ->subscriber-no
  "Ensure a number is mobile number"
  [msisdn]
  (let [subscriber (if (.startsWith msisdn config/country-prefix)
                     (subs msisdn (count config/country-prefix))
                     msisdn)]
    (when (> (count (str subscriber)) (+ config/subscriber-digits config/network-digits))
      (throw (Exception. (format "invalidMSISDN(%s)" msisdn))))
    (Long/valueOf subscriber)))

(defn ->msisdn
  ""
  [subscriber]
  (Long/valueOf (if (.startsWith subscriber config/country-prefix)
                  (subs subscriber (count config/country-prefix))
                  subscriber)))

(defn ->currency
  [cents]
  (let [decimals (math/expt 10 config/currency-decimals)]
    (/ cents decimals)))

(defn ->cents
  [amount]
  (let [decimals (math/expt 10 config/currency-decimals)]
    (* amount decimals)))


