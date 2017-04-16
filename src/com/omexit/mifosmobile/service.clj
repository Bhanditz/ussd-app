;
(ns com.omexit.mifosmobile.service
  ^{:doc    "MIFOS Mobile - A service that provides access to MIFOS via a mobile channel (USSD, App etc)"
    :author "Antony Omeri"}
  (:gen-class)
  (:require [clojure.tools.logging :as log]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [com.omexit.common
             [config :as config]
             [db :as db]
             [utils :as utils]]
            [com.omexit.mifosmobile
             [sessions :as session]
             [mifos :as mifos]]
            [com.omexit.mifosmobile.ussd.ussd-menu :as menu]
            [com.omexit.mifosmobile.ussd.ussd-utils :as ussd-utils]
            [com.omexit.mifosmobile.sms :as sms]))

; --------------------------------------------------------------------------------------------------------
;   Gateway adapter functions
; --------------------------------------------------------------------------------------------------------
(declare handle-ussd-request)

;;
;; AfricaIsTalking USSD Gateway
;;
(let [do-respond (fn [text terminate?]
                   {:status 200
                    :headers {"Content-Type" "text/plain"}
                    :body (str (if terminate? "END " "CON ") text)})]
  (defn- handle-ussd-request-africaistalking
    "Africa Is Talking USSD Gateway request handler"
    [request]
    (try
      (let [{:keys [subscriber session-id service-code option]}
            (utils/parse-params-string
              (slurp (request :body))
              [[:subscriber "phoneNumber" :subscriber]
               [:session-id "sessionId" :uint63]
               [:service-code "serviceCode" :string]
               [:option "text" :string]])]
        (if (and subscriber session-id service-code)
          (let [is-new? (not (session/session-exists? session-id subscriber))
                inputs (into [] (let [input# (atom 0)]
                                  (map (fn [input]
                                         (swap! input# inc)
                                         {(keyword (format "input%s" @input#)) input})
                                       (clojure.string/split option #"\*"))))
                input (or (when inputs ((comp first vals last) inputs))
                          "")
                {:keys [text end?]} (handle-ussd-request session-id subscriber input is-new?)]
            (do-respond text end?))
          (do
            (log/errorf "malformedRequest(phoneNumber=%s,sessionId=%s,serviceCode=%s,text=%s)" subscriber session-id service-code option)
            (do-respond config/ussd-internal-error-msg true))))
      (catch Exception ex
        (do
          (log/errorf ex "!handleAfricaIsTalkingRequest() -> %s" (.getMessage ex))
          (do-respond config/ussd-internal-error-msg true))))))

(defn handle-ussd-request-comviva
  "Handler for COMVIVA's USSD gateway (Flares)"
  [request])

;; -----------------------------------------------------------------------------------------------------------
;; USSD handler functions
;; -----------------------------------------------------------------------------------------------------------

(declare new-loan-request update-loan-status get-subscriber-info)

(defn handle-ussd-request
  "Handles the internals of processing a USSD request. It accepts 4 parameters:
    session-id - unique ID for the current session
    subscriber - a subscriber/customers's MSISDN
    input      - The text submitted by the customer
    new?       - To show whether this is a new USSD session. This is important for session management between the USSD Gateway and this App.

    Returns:
    map::
    :text   - The text to be displayed on the customer's phone.
    :end?   - Signals the end of a session. The caller function uses this to render the appropriate session directive to the USSD Gateway"
  [session-id subscriber input new?]
  (log/infof "handleUSSDRequest(args=[%s %s %s %s])" session-id subscriber input new?)
  (try
    (let [session-data (if new?
                         (conj
                           (session/start-new-session session-id subscriber)
                           (mifos/get-client-info subscriber)
                           (db/get-customer-info subscriber))
                         (session/get-session-data session-id subscriber))
          new-session-data (atom (assoc session-data :ussd-input input))
          [state +session-data+] (if new?
                                 (let [start-state (let [{:keys [is-enabled? pin]} @new-session-data]
                                                     (if (and is-enabled? pin)
                                                       (if (menu/valid-state? :enter-pin) :enter-pin :menu)
                                                       :not-allowed))]
                                   (swap! new-session-data assoc :state start-state)
                                   [start-state new-session-data])
                                 (let [updated-session (menu/state-automaton new-session-data input)]
                                   [(updated-session :state) (atom updated-session)]))]
      (log/debugf "sessionData(%s)" @+session-data+)
      ;; update session data
      (session/update-session-data session-id subscriber +session-data+)
      ;; handle actions
      (condp = state
        :lend (try
                (let [{:keys [amount-qualified queue-count loan-balance loan-amount accountNo id loan-product-id loan-request-amount]
                       :or {queue-count 0 loan-balance 0 loan-amount 0}} @+session-data+]
                  ;; check business rules:
                  ;; 1. The customer is qualified to borrow
                  ;; 2. That the customer is not a debtor
                  ;; 3. That the customer does not have pending requests in the loan request queue
                  (if (and (> amount-qualified 0)
                           (= queue-count 0)
                           (= loan-balance 0)
                           (<= loan-request-amount amount-qualified))
                    (let [loan-req {:principal loan-request-amount
                                    :account-id accountNo
                                    :client-id id
                                    ;:loan-product-id config/loan-product-id
                                    :loan-product-id loan-product-id
                                    :amount loan-request-amount}
                          %failed (fn [request-id subscriber error type]
                                    (let [{:keys [error-msg to-reconcile?]} error]
                                      (log/errorf "!lend(%s,sub=%s,amt=%s) -> %s" session-id subscriber loan-request-amount error)
                                      (update-loan-status {:request-id request-id
                                                           :subscriber subscriber
                                                           :status (if to-reconcile? 2 1)
                                                           :error-msg error-msg})
                                      (sms/send-sms type @+session-data+)
                                      (swap! +session-data+ assoc :state :lend-failed)))
                          ;; Register the loan request for tracking purposes
                          request-id (new-loan-request (assoc loan-req :subscriber subscriber))
                          ret (mifos/submit-loan-request (assoc loan-req :request-id request-id))
                          {:keys [status error]} ret]
                      (condp = status
                        :ok (let [{:keys [loanId resourceId]} (ret :result)]
                              (log/infof "lend(%s,sub=%s,amt=%s) -> %s" session-id subscriber input ret)
                              (update-loan-status {:request-id request-id
                                                   :status 5
                                                   :loan-id loanId
                                                   :subscriber subscriber})

                              ;; send SMS notification
                              (sms/send-sms :lend-ok (assoc @+session-data+
                                                       :request-id request-id
                                                       :loan-amount loan-request-amount))

                              (future (let [{:keys [status error]}
                                            (mifos/approve-loan {:loan-id loanId
                                                                 :resource-id resourceId
                                                                 :loan-amount loan-request-amount
                                                                 :payment-type config/loan-payment-type
                                                                 :account-no accountNo})]
                                        (condp = status
                                          :ok (do
                                                (update-loan-status {:request-id request-id
                                                                     :status 4
                                                                     :subscriber subscriber
                                                                     :loan-id loanId})
                                                (let [{:keys [status error]}
                                                      (mifos/disburse-loan {:loan-id loanId
                                                                            :loan-amount loan-request-amount
                                                                            :payment-type config/loan-payment-type
                                                                            :account-no accountNo
                                                                            :routing-code 12})]
                                                  (condp = status
                                                    :ok (update-loan-status {:request-id request-id
                                                                             :status 3
                                                                             :subscriber subscriber
                                                                             :loan-id loanId})
                                                    :failed (%failed request-id subscriber error :disburse-failed)
                                                    (throw (Exception. (format "unknownStatus(%s)" status))))))
                                          :failed (%failed request-id subscriber error :approve-failed)
                                          (throw (Exception. (format "unknownStatus(%s)" status)))))))
                        :failed  (%failed request-id subscriber error :lend-failed)
                        (throw (Exception. (format "unknownStatus(%s)" status)))))

                    ;;
                    (cond
                      (> loan-balance 0) (swap! +session-data+ assoc :state :has-loan)
                      :else (swap! +session-data+ assoc :state :lend-failed))))
                (catch Exception ex
                  (do
                    (log/errorf ex "!lend(%s,sub=%s,amt=%s) -> %s" session-id subscriber input (.getMessage ex))
                    (swap! +session-data+ assoc :state :lend-failed))))

        :loan-account-balance
        (let [{:keys [loan-account-no accountNo id]} @+session-data+
              result (ussd-utils/get-loan-account-balance loan-account-no accountNo id)]
          (swap! +session-data+ conj result))
        :savings-account-balance
        (let [{:keys [savings-account-no accountNo id]} @+session-data+
              result (ussd-utils/get-savings-account-balance savings-account-no)]
          (swap! +session-data+ conj result))
        true)

      ;; Finally Render the state
      (let [{:keys [state]} @+session-data+
            final-state? (menu/final-state? state)
            [+session-data+ state-text] (menu/render-state @+session-data+)]
        (session/update-session-data session-id subscriber +session-data+)

        (when final-state?
          (session/end-session session-id subscriber))
        {:text (com.omexit.common.utils/get-message state-text @+session-data+)
         :end? (if final-state? true false)}))
    (catch Exception ex
      (log/errorf ex "handleUSSDRequest(args=[%s %s %s %s]) -> %s" session-id subscriber input new? (.getMessage ex))
      {:text config/ussd-internal-error-msg
       :end? true})))

(defn handle-ussd-loan-hook
  [request]
  (let [{:keys [body]} request]
    (log/info (slurp body))
    {:status 200
     :body "OK"}))

;; ------------------------------------------------------------------------------------------------------------
;;    Servlet Functions
;; ------------------------------------------------------------------------------------------------------------
(defroutes all-routes
           (GET "/" [] (fn [req] "ehlo"))
           (POST "/ussd/comviva" request handle-ussd-request-comviva)
           (GET "/ussd/comviva" request handle-ussd-request-comviva)
           (POST "/ussd/ait" request handle-ussd-request-africaistalking)
           (POST "/ussd/mifos" request handle-ussd-request-comviva)
           (GET "/ussd/mifos" request handle-ussd-request-comviva)
           (POST "/hooks/ussd_loan" request handle-ussd-loan-hook)
           ;(POST "/loans/repay" request handle-loan-repayment)
           (route/not-found "404"))

(defn initialize
  ""
  []
  (log/info "#---------------------------------------------------------------------------")
  (log/info "# MIFOS MOBILE Service")
  (log/info "#---------------------------------------------------------------------------")

  ;; -------------------------------------------------------------------------------------
  ;;  Initialize application
  ;; -------------------------------------------------------------------------------------
  ; Load and initialize configs
  (config/initialize-config)
  ; Initialize datasource
  (db/initialize-datasource)
  ; ussd menu
  (menu/initialize-ussd-menu)
  ; RMQ
  (sms/initialize-sms-interface)
  ; session management
  (session/initiate-session-manager))

(defn destroy
  "Gracefully shutdown"
  []
  (log/info "Service ShutDown Initiated.")

  (log/info "Service ShutDown Finalized."))

;; ------------------------------------------------------------------------------------------
;; Utility functions
;; ------------------------------------------------------------------------------------------
(def timestamp-txn-count (atom {}))
(def loan-products nil)

(defn generate-request-id
  "Generates a unique request-id: Supports upto a TPS of 99"
  [component-id]
  (let [timestamp (System/currentTimeMillis)
        counter (or (@timestamp-txn-count timestamp) 1)
        id (format "%s%s%02d" component-id timestamp counter)]
    (if (@timestamp-txn-count timestamp)
      (swap! timestamp-txn-count assoc timestamp (inc counter))
      (reset! timestamp-txn-count {}))
    (Long/valueOf id)))

(defn new-loan-request
  "Creates a new loan request and returns the request-id"
  [args]
  (log/debugf "newLoanRequest(args=%s)")
  (let [request-id (generate-request-id 1)
        {:keys [subscriber client-id loan-product-id amount]} args]
    (db/execute-query "INSERT INTO tbl_loan_req_new (request_id, subscriber_fk, client_fk, loan_product_id, request_amount)
                       VALUES (?,?,?,?,?)" request-id subscriber client-id loan-product-id amount)
    request-id))

(defn update-loan-status
  ""
  [args]
  (log/debugf "updateLoanStatus(args=%s)" args)
  (let [{:keys [request-id subscriber status error-msg loan-id]} args]
    (db/execute-query "call proc_update_loan_req_status(?,?,?,?,?)"
                      request-id subscriber status (or error-msg "") loan-id)))