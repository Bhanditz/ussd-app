(ns
  ^{:doc "MIFOS Helper functions"}
  com.argent.mifosmobile.mifos
  (:require [clj-http.client :as client]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [com.argent.common.config :as config]
            [com.argent.common.macros :as mac]
            [com.argent.common.date :as date]))

(declare get-loan-product)

(defn initialize
  ""
  [args]
  (def loan-product-info (into {}
                               (doall (map (fn [product-id]
                                             (let [product-info (or (get-loan-product product-id)
                                                                    (throw (Exception. (format "!getLoanProduct(id=%s) -> Loan Product not found" product-id))))]
                                               {product-id product-info}))
                                           (args :loan-product-ids))))))

(defn
  call-mifos
  "Handle Calls to MIFOS and return a MAP of:
    :status - call status :ok|:failed
    :result - the result of the call if :ok
    :error-msg - error message for failed calls
    :error-class - classification of the error for error handling purposes
    "
  [args]
  (mac/with-timed-try
    (format "callMIFOS(args=%s)" args)
    (fn [error]
      (log/errorf error "!doCallMIFOS(args=%s) -> %s" args (.getMessage error))
      {:status :failed
       :error-msg (.getMessage error)
       :error-class :mifos-call-error})
    (let [{:keys [scheme uri data query-params]} args
          api-url (str config/mifos-api-url "/" uri)
          request-params (conj {:headers (conj {"Content-Type" "application/json"}
                                               config/mifos-api-headers)
                                :basic-auth [config/mifos-api-user config/mifos-api-pass]
                                :insecure? true
                                :socket-timeout config/mifos-api-read-timeout
                                :conn-timeout config/mifos-api-connect-timeout}
                               (if query-params {:query-params query-params} {})
                               (if data {:body data} {}))
          {:keys [status request-time body error]}
          (condp = scheme
            :post (client/post api-url request-params)
            :get (client/get api-url request-params)
            :put (client/put api-url request-params)
            :delete (client/delete api-url request-params)
            (client/get api-url request-params))]
      (condp = status
        200 {:status :ok
             :result (json/read-str body :key-fn keyword)}
        (let [[error-class error-msg]
              (condp instance? error
                java.net.ConnectException [:timeout-on-connect "Connection Timeout"]
                java.net.SocketTimeoutException [:timeout-on-read "Timeout on read"]
                [:http-error (str "HTTP Status " status)])]
          {:status :failed
           :error {:error-class error-class
                   :error-msg error-msg}})))))

(declare get-customer-loan-info)

(defn get-client-info
  "Fetch Client Information by Mobile Number from MIFOS"
  [client-msisdn]
  (mac/with-timed-try
    (format "getClientInfo(mobile-no=%s)" client-msisdn)
    (fn [error]
      (log/errorf error "!doGetClientInfo(mobile-no=%s) -> %s" client-msisdn (.getMessage error))
      {})
    (let [args {:scheme :get
                :uri "clients"
                :query-params {"sqlSearch" (format "c.mobile_no=%s" client-msisdn)}}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret]
          (if-let [client-info (-> result :pageItems first)]
            (let [client-id (:id client-info)
                  loan-info (if client-id
                              (get-customer-loan-info (:id client-info))
                              {})]
              (conj (dissoc client-info :timeline)
                    loan-info))
            {}))
        :failed
        (let [error-msg (format "!doGetClientInfo(mobile-no=%s) -> %s" client-msisdn ret)]
          (log/error error-msg)
          {})))))

(defn get-client-accounts
  ""
  [client-id]
  (mac/with-timed-try
    (format "getClientAccounts(client-id=%s)" client-id)
    (fn [error]
      (log/errorf error "!doGetClientAccounts(client-id=%s) -> %s" client-id (.getMessage error))
      {:status :failed
       :error-msg (.getMessage error)
       :error-class :mifos-call-error})
    (let [args {:scheme :get
                :uri (format "clients/%s/accounts" client-id)
                :query-params {"associations" "all"}}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret
              %get-accounts-info (fn [accounts]
                                   (group-by :product-name
                                             (apply vector
                                                    (take-last 5
                                                               (remove nil? (map (fn [account]
                                                                                   (let [{:keys [shortProductName productName productId accountNo id]} account
                                                                                         active? (-> account :status :active)]
                                                                                     (when active?
                                                                                       {:product-name productName
                                                                                        :product-id productId
                                                                                        :account-no accountNo
                                                                                        :active? active?
                                                                                        :id id})))
                                                                                 accounts))))))]
          (into {}
                (map (fn [[account info]]
                       (let [info (%get-accounts-info info)]
                         {account info}))
                     result)))
        :failed
        (let [error-msg (format "!doGetClientAccounts(client-id=%s) -> %s" client-id ret)]
          (log/error error-msg)
          (throw (Exception. error-msg)))))))

(defn get-savings-account-info
  ""
  [account-id]
  (mac/with-timed-try
    (format "getSavingAccountInfo(account-id=%s)" account-id)
    (fn [error]
      (log/errorf error "!doGetSavingsAccountInfo(account-id=%s) -> %s" account-id (.getMessage error))
      {:status :failed
       :error-msg (.getMessage error)
       :error-class :mifos-call-error})
    (let [args {:scheme :get
                :uri (format "savingsaccounts/%s" account-id)}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret
              account-balance (-> result :summary :accountBalance)]
          {:status :ok :result {:savings-account-balance account-balance}})
        :failed
        (let [error-msg (format "!doGetSavingsAccountInfo(account-id=%s) -> %s" account-id ret)]
          (log/error error-msg)
          (throw (Exception. error-msg)))))))

(defn get-loan-info
  ""
  [loan-id]
  (mac/with-timed-try
    (format "getLoanInfo(loan-id=%s)" loan-id)
    (fn [error]
      (log/errorf error "!doGetLoanInfo(loan-id=%s) -> %s" loan-id (.getMessage error))
      {:status :failed
       :error {:msg (.getMessage error)
               :class :mifos-call-error}})
    (let [args {:scheme :get
                :uri (format "loans/%s" loan-id)}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret
              {:keys [principalDisbursed principalPaid principalOutstanding
                      interestCharged interestPaid interestOutstanding
                      feeChargesCharged feeChargesPaid feeChargesOutstanding
                      penaltyChargesCharged penaltyChargesPaid penaltyChargesOutstanding
                      totalExpectedRepayment totalRepayment totalOutstanding totalOverdue]} (result :summary)]
          {:status :ok
           :result {:loan-expected-repayment totalExpectedRepayment
                    :loan-repaid totalRepayment
                    :loan-outstanding totalOutstanding
                    :loan-overdue totalOverdue}})
        :failed
        (let [error-msg (format "!doGetLoanInfo(loan-id=%s) -> %s" loan-id ret)]
          (log/error error-msg)
          (throw (Exception. error-msg)))))))

(defn get-loan-product
  "Retrieve the Loan Product Info"
  [loan-product-id]
  (mac/with-timed-try
    (format "getProductLoans(loan-product=%s)" loan-product-id)
    (fn [error]
      (log/errorf error "!doGetProductLoans(loan-product=%s) -> %s" loan-product-id (.getMessage error))
      nil)
    (let [args {:scheme :get
                :uri (str "loanproducts/" loan-product-id)}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [id maxPrincipal maxNumberOfRepayments numberOfRepayments
                      interestCalculationPeriodType interestRatePerPeriod interestType
                      amortizationType repaymentFrequencyType transactionProcessingStrategyId
                      ]} (ret :result)]
          {:locale "en_US"
           :loanType "individual"
           :productId id
           ;;
           :interestCalculationPeriodType (interestCalculationPeriodType :id)
           :interestRatePerPeriod interestRatePerPeriod
           :interestType (interestType :id)
           :amortizationType (amortizationType :id)
           :repaymentFrequencyType (repaymentFrequencyType :id)
           :repaymentEvery 1
           :numberOfRepayments numberOfRepayments
           :loanTermFrequency 12
           :loanTermFrequencyType (repaymentFrequencyType :id)
           :maxOutstandingLoanBalance (str maxPrincipal)
           :transactionProcessingStrategyId transactionProcessingStrategyId
           :dateFormat "dd MMMM yyyy"})
        :failed
        (let [error-msg (format "!doGetClientLoans(loan-product=%s) -> %s" loan-product-id ret)]
          (log/error error-msg)
          nil)))))

(defn get-customer-loan-info
  "Returns the amount qualified and whether the customer is allowed to access the loan"
  [client]
  (mac/with-timed-try
    (format "getCustomerLoanInfo(client=%s)" client)
    (fn [error]
      (log/errorf error "!doGgetCustomerLoanInfo(client=%s) -> %s" client (.getMessage error))
      {:amount-qualified 0 :is-enabled? false})
    (let [args {:scheme :get
                :uri (format "datatables/%s/%s" config/loan-datatable-name client)}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [amount_qualified is_enabled]} (first (ret :result))]
          {:amount-qualified amount_qualified
           :is-enabled? (= is_enabled "true")})
        :failed
        (let [error-msg (format "!doGgetCustomerLoanInfo(client=%s) -> %s" client ret)]
          (log/error error-msg)
          {:amount-qualified 0 :is-enabled? false})))))

(defn submit-loan-request
  ""
  [request]
  (mac/with-timed-try
    (format "submitLoanRequest(args=%s)" request)
    (fn [error]
      (log/errorf error "!doSubmitLoanRequest(args=%s) -> %s" request (.getMessage error))
      {:status :failed
       :error-msg (.getMessage error)
       :error-class :mifos-call-error})
    (let [{:keys [loan-product-id account-id amount request-id ]} request
          args {:scheme :post
                :uri    "loans"
                :data   (json/write-str (conj (loan-product-info loan-product-id)
                                              {:clientId account-id,
                                               :submittedOnDate (date/now "dd MMMM yyyy"),
                                               :principal (str amount)
                                               :expectedDisbursementDate (date/now "dd MMMM yyyy")
                                               :externalId (str request-id)}))}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        ret
        :failed
        (let [{:keys [error-class error-msg]} (ret :error)]
          (log/errorf "!doSubmitLoanRequest(args=%s) -> %s" request error-msg)
          {:status :failed
           :error {:error-msg error-msg
                   :error-class error-class
                   :to-reconcile? (contains? #{:timeout-on-read} error-class)}})))))

(defn approve-loan
  ""
  [request]
  (mac/with-timed-try
    (format "approveLoan(args=%s)" request)
    (fn [error]
      (log/errorf error "!doAprroveLoan(args=%s) -> %s" request (.getMessage error))
      {:status :failed
       :error {:error-msg (.getMessage error)
               :error-class :mifos-call-error
               :to-reconcile? false}})
    (let [{:keys [loan-id account-no]} request
          args {:scheme :post
                :uri    (str "loans/" loan-id)
                :query-params {"command" "approve"}
                :data   (json/write-str {:dateFormat                      "dd MMMM yyyy",
                                         :locale                          "en_US",
                                         :approvedOnDate          (date/now "dd MMMM yyyy")
                                         :expectedDisbursementDate (date/now "dd MMMM yyyy")
                                         :note (str "Loan Approval for Client=" account-no ", LoanID=" loan-id)
                                         })}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret
              status (-> result :changes :status :value)]
          (if (= status "Approved")
            ret
            (do
              (log/errorf "!doApproveLoan(args=%s) -> %s" request status)
              {:status :failed
               :error {:error-msg result
                       :error-class :loan-approval}})))
        :failed
        (let [{:keys [error-class error-msg]} (ret :error)]
          (log/errorf "!doApproveLoan(args=%s) -> %s" request error-msg)
          {:status :failed
           :error {:error-msg error-msg
                   :error-class error-class
                   :to-reconcile? (contains? #{:timeout-on-read} error-class)}})))))

(defn disburse-loan
  ""
  [request]
  (mac/with-timed-try
    (format "disburseLoan(args=%s)" request)
    (fn [error]
      (log/errorf error "!doDisburseLoan(args=%s) -> %s" request (.getMessage error))
      {:status :failed
       :error-msg (.getMessage error)
       :error-class :mifos-call-error})
    (let [{:keys [loan-id loan-amount payment-type account-no]} request
          args {:scheme :post
                :uri    (str "loans/" loan-id)
                :query-params {"command" "disburse"}
                :data   (json/write-str {:dateFormat                      "dd MMMM yyyy"
                                         :locale                          "en_US"
                                         :transactionAmount               loan-amount
                                         :actualDisbursementDate          (date/now "dd MMMM yyyy")
                                         :paymentTypeId                   (str payment-type)
                                         :note (str "Loan Disbursment for Client=" account-no ", LoanID=" loan-id ", amount=" loan-amount)})}
          ret (call-mifos args)
          {:keys [status]} ret]
      (condp = status
        :ok
        (let [{:keys [result]} ret
              status (-> result :changes :status :value)]
          (if (= status "Active")
            ret
            (do
              (log/errorf "!doDisburseLoan(args=%s) -> %s" request status)
              {:status :failed
               :error {:error-msg result
                       :error-class :loan-disbursement}})))
        :failed
        (let [{:keys [error-class error-msg]} (ret :error)]
          (log/errorf "!doDisburseLoan(args=%s) -> %s" request error-msg)
          {:status :failed
           :error {:error-msg error-msg
                   :error-class error-class
                   :to-reconcile? (contains? #{:timeout-on-read} error-class)}})))))