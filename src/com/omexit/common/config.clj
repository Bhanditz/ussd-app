(System/setProperty "mifos.mobile.config" (str (or (System/getenv "DEV_HOME") "C:/Dev") "/Projects/mifos-mobile/test-env/mifos.config"))

(ns com.omexit.common.config
  (:require [clojure.tools.logging :as log]
            [clojure.data.zip.xml :as zip-xml :only [xml1]]
            [clojure.zip :as zip :only [xml-zip]])
  (:import  [java.util Properties]
            [java.io FileInputStream])
  (:use [clojure.walk]))

(def app-configs (atom {}))

(defn- config-file []
  (let [result (let [result (System/getProperty "mifos.mobile.config")]
                     (when result (java.io.File. result)))]
    (if (and result (.isFile result))
      result
      (do (log/fatal (format "Config(%s) = nil" result))
          (throw (Exception. (format "Configuration file [file://%s] not found." result)))))))

(defn- load-config [config]
  (let [properties (Properties.)
        fis (FileInputStream. config)]
    ; populate the properties hashmap with values from the output stream
    (.. properties (load fis))
    (keywordize-keys (into {} properties))))

(defn- config-value [name & args]
  (let [value (@app-configs name)]
    (if-not (empty? value)
      (let [args (when args (apply assoc {} args))
            {type :type} args
            args (dissoc args :type)
            value (if (vector? value)
                    (loop [x (first value)
                           xs (next value)]
                      (let [properties (dissoc x :value)]
                        (if (or (and (empty? args)
                                     (empty? properties))
                                (and (not (empty? args))
                                     (every? (fn [[k v]]
                                               (= (properties k) v))
                                             args)))
                          (x :value)
                          (when xs
                            (recur (first xs) (next xs))))))
                    value)]
        (when value
          (let [value #^String value]
            (cond (or (nil? type) (= type :string)) value
                  ;; ---
                  (= type :int) (Integer/valueOf value)
                  (= type :long) (Long/valueOf value)
                  (= type :boolean) (contains? #{"yes" "true" "y" "t" "1"}
                                            (.toLowerCase value))
                  (= type :keyword) (keyword value)
                  (= type :path) (java.io.File. value)
                  (= type :url) (java.net.URL. value))))))))

(defn initialize-config []
  (log/info "Initializing configurations..")
  (reset! app-configs (load-config (config-file)))
  (log/debugf "configs=%s" @app-configs)

  ;; Database settings
  (def jdbc-driver (config-value :db.jdbc.driver))
  (def jdbc-protocol (config-value :db.jdbc.protocol))
  (def jdbc-url (config-value :db.jdbc.url))
  (def db-host (config-value :db.server))
  (def db-port (config-value :db.port))
  (def db-name (config-value :db.database))
  (def db-user (config-value :db.user))
  (def db-password (config-value :db.password))

  ;; Subscriber number related settings
  (def subscriber-digits (config-value :msisdn.subscriber.digits :type :int))
  (def network-digits (config-value :msisdn.network.digits :type :int))
  (def country-prefix (config-value :msisdn.country.prefix))

  ;; Currency settings
  (def currency-decimals (or (config-value :currency.decimals :type :int) 0))

  ;; MIFOSX configurations
  (def mifos-api-url (config-value :mifos.api.url))
  (def mifos-api-user (config-value :mifos.api.user))
  (def mifos-api-pass (config-value :mifos.api.pass))
  (def mifos-api-tenant-id (config-value :mifos.api.tenant.id))
  (def mifos-api-connect-timeout (config-value :mifos.api.timeout.connect :type :int))
  (def mifos-api-read-timeout (config-value :mifos.api.timeout.read :type :int))
  (def mifos-api-headers (let [keys (keys @app-configs)
                               headers (remove #(nil? %) (map (fn [k] (when (.startsWith (str k) ":mifos.api.header") k)) keys))]
                           (if-not (empty? headers)
                             (into {}
                                   (map (fn [header]
                                          (let [hvalue (@app-configs header)
                                                hkey (last (re-find #":mifos.api.header\[\"(.*?)\"\]" (str header)))]
                                            {hkey hvalue})) headers))
                             {})))

  ;; Session
  (def session-max-lifetime (/ (or (config-value :ussd.session.max.lifetime :type :long) 60000) 1000))
  (def session-clear-interval (or (config-value :ussd.session.clear.interval :type :long) 1000))

  ;; USSD Menu Definition
  (def ussd-menu-definition (or (config-value :ussd.menu.definition)
                                (throw (Exception. "undefinedUSSDMenuDefinition()"))))
  (let [menu-def (slurp ussd-menu-definition)
        bis (-> (.getBytes menu-def) (java.io.ByteArrayInputStream.) (java.io.BufferedInputStream.))
        root (zip/xml-zip (clojure.xml/parse bis))]
    (def ussd-state-transitions ((first (zip-xml/xml1-> root :transition-table)) :content))
    (def ussd-state-renders ((first (zip-xml/xml1-> root :renderer)) :content))
    (def ussd-state-initializer ((first (zip-xml/xml1-> root :initializer)) :content)))

  (def ussd-internal-error-msg (config-value :ussd.internal.error.msg))

  ;; loan info
  (def loan-product-id (config-value :loan.product.id :type :int))
  (def loan-payment-type (config-value :loan.payment.type))
  (def loan-datatable-name (config-value :loan.datatable.name))

  ;; RabbitMQ
  (def rabbitmq-host (or (config-value :rabbitmq.host) "127.0.0.1"))
  (def rabbitmq-port (or (config-value :rabbitmq.port :type :int) 5672))
  (def rabbitmq-username (config-value :rabbitmq.username))
  (def rabbitmq-password (config-value :rabbitmq.password))
  (def rabbitmq-vhost (or (config-value :rabbitmq.vhost) "/"))
  (def rabbitmq-queue-name (config-value :rabbitmq.queue.name))
  (def rabbitmq-queue-exchange (config-value :rabbitmq.queue.exchange))
  (def rabbitmq-queue-routing-key (config-value :rabbitmq.queue.routing.key))
  (def rabbitmq-retry-queue-name (config-value :rabbitmq.retry.queue.name))
  (def rabbitmq-retry-exchange (config-value :rabbitmq.retry.exchange))
  (def rabbitmq-retry-routing-key (config-value :rabbitmq.retry.routing.key))
  (def rabbitmq-retry-delay (config-value :rabbitmq.retry.delay :type :int))
  (def rabbitmq-failed-queue-name (config-value :rabbitmq.failed.queue.name))
  (def rabbitmq-failed-exchange (config-value :rabbitmq.failed.exchange))
  (def rabbitmq-failed-routing-key (config-value :rabbitmq.failed.routing.key))

  ;; SMS Settings
  (def sms-notify (or (config-value :sms.notify :type :keyword) :off))
  (when (= sms-notify :on)
    (def sms-on-lend (or (config-value :sms.on.lend :type :boolean) false))
    (when sms-on-lend
      (def sms-lend-ok (config-value :sms.lend.ok))
      (def sms-lend-failed (config-value :sms.lend.failed))
      (def sms-disburse-failed (config-value :sms.disburse.failed))
      (def sms-approval-failed (config-value :sms.approval.failed)))))
