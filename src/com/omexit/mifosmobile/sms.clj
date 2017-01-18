(ns com.argent.mifosmobile.sms
  (:require [com.argent.common.config :as config]
            [com.argent.lib.rabbitmq :as rmq]
            [clojure.tools.logging :as log]
            [com.argent.common.utils :as utils]))

(def send-sms (fn [& args] nil))

;; Initialize Interfaces
(defn initialize-sms-interface
  "Initializes the available SMS interfaces. Currently the only available interface is RabbitMQ"
  []
    (when (= config/sms-notify :on)
        (def do-send-sms (rmq/initialize-rabbitmq {
                                                ; connection info
                                                :host     config/rabbitmq-host
                                                :port     config/rabbitmq-port
                                                :username config/rabbitmq-username
                                                :password config/rabbitmq-password
                                                :vhost    config/rabbitmq-vhost
                                                ; queue info
                                                :queue-name config/rabbitmq-queue-name
                                                :queue-exchange config/rabbitmq-queue-exchange
                                                :queue-routing-key config/rabbitmq-queue-routing-key
                                                ; retry queue info
                                                :queue-retry-name config/rabbitmq-retry-queue-name
                                                :queue-retry-exchange config/rabbitmq-retry-exchange
                                                :queue-retry-routing-key config/rabbitmq-retry-routing-key
                                                :queue-retry-delay config/rabbitmq-retry-delay
                                                ; failed queue info
                                                :queue-failed-name config/rabbitmq-failed-queue-name
                                                :queue-failed-exchange config/rabbitmq-failed-exchange
                                                :queue-failed-routing-key config/rabbitmq-failed-routing-key}))
        (def send-sms (fn [type args]
                        (let [{:keys [request-id subscriber]} args]
                          (try
                            (log/infof "sendSMS(%s,%s,%s)" request-id subscriber type)
                            (let [msg (utils/get-message (condp = type
                                                           :lend-ok config/sms-lend-ok
                                                           :lend-failed config/sms-lend-failed
                                                           :disburse-failed config/sms-disburse-failed
                                                           :approve-failed config/sms-approval-failed
                                                           (throw (Exception. (format "UndefinedSMSType(%s)" type))))
                                                         args)]
                              (do-send-sms {:subscriber subscriber
                                            :request-id request-id
                                            :msg msg}))
                            (catch Exception ex
                              (log/errorf ex "!sendSMS(%s,%s,%s) -> %s" request-id subscriber type (.getMessage ex)))))))))

