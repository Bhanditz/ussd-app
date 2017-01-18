(ns
  ^{:doc    "USSD Utils - Utility functions that used by the USSD state machine"
    :author "Nahashon Kibet | mckibet@gmail.com"}
  com.argent.mifosmobile.ussd.ussd-utils
  (:require [com.argent.common.config :as config]
            [com.argent.common.db :as db]
            [com.argent.mifosmobile.mifos :as mifos]
            [digest]
            [clojure.tools.logging :as log]))

(declare initialize-loans initialize-savings initialize-transfers
         finalize-loans finalize-savings finalize-transfers)

(defn initialize-utils []
  (log/info "Initializing USSD Utils")
  (initialize-loans)
  (initialize-savings)
  (initialize-transfers))

(defn finalize-utils []
  (log/info "Finalizing USSD Utils")
  (finalize-loans)
  (finalize-savings)
  (finalize-transfers))

;; --------------------------------------------------------------------------------
;;  Loans
;; --------------------------------------------------------------------------------
(def loan-products-info (atom nil))
(def loan-product-names (atom []))

(defn- initialize-loans
  []
  (let [loan-products (db/get-loan-products)
        product-names (into [] (keys loan-products))]
    (reset! loan-products-info loan-products)
    (reset! loan-product-names product-names)

    ; mifos
    (mifos/initialize {:loan-product-ids (vals loan-products)})

    (def get-product
      (fn [index]
        (if-let [product-name (nth product-names (dec index))]
          (or [(loan-products product-name) product-name]
              (do
                (log/errorf "!loanProductIdNotFound(name=%s)" product-name)
                nil))
          (do
            (log/errorf "!loanProductNameNotFound(index=%s)" (dec index))
            nil))))

    (def set-loan-product
      (fn [session input]
        (if-let [input (try
                         (Integer/valueOf input)
                         (catch Exception ex#
                           (log/errorf ex# "invalidInput(%s) -> %s" input (.getMessage ex#))
                           nil))]
          (if-let [[product-id product-name] (get-product input)]
            (do
              (swap! session assoc :loan-product-id product-id :loan-product-name product-name)
              [true session])
            (do
              (log/errorf "!getProductId(%s)" input)
              [false session]))
          [false session])))

    (def get-loan-products-list
      (fn [session list-index-counter]
        (if (empty? product-names)
          [false (assoc session :state :no-loan-products) @list-index-counter nil]
          (let [products-list (into []
                                    (map (fn [loan-product]
                                           (swap! list-index-counter inc)
                                           (format "%s. %s" @list-index-counter loan-product))
                                         product-names))]
            [true session @list-index-counter products-list]))))))

(defn get-client-loan-products-list
  "Returns a list of a clients active loan products"
  [session list-index-counter]
  (if-let [client-accounts
           (or (session :client-accounts)
               (if-let [accounts (mifos/get-client-accounts (session :id))]
                 accounts nil))]
    (let [loan-accounts (client-accounts :loanAccounts)
          loan-product-names (apply vector (keys loan-accounts))]
      (if-not (empty? loan-product-names)
        (let [product-names (map (fn [loan-product]
                                   (swap! list-index-counter inc)
                                   (format "%s. %s" @list-index-counter loan-product))
                                 loan-product-names)]
          [true (assoc session :client-accounts client-accounts :loan-products loan-product-names) @list-index-counter product-names])
        [false (assoc session :state :no-client-loan-products) @list-index-counter nil]))
    (do
      (log/error "!noClientAccountsthFound()")
      [false session @list-index-counter nil])))

(defn set-client-loan-product
  "Set a selected client loan product in the provided session"
  [session input]
  (if-let [input (try
                   (Integer/valueOf input)
                   (catch Exception ex#
                     (log/errorf ex# "invalidLoanInput(%s) -> %s" input (.getMessage ex#))
                     nil))]
    ;; valid input
    (let [loan-products# (@session :loan-products)]
      (log/infof "session=%s" @session)
      (if-let [loan-product-name (nth loan-products# (dec input))]
        (let [loan-account-info (-> @session :client-accounts :loanAccounts)
              loan-product-info (first (loan-account-info loan-product-name))
              loan-product-id (loan-product-info :product-id)]
          (swap! session assoc :loan-product-name loan-product-name :loan-product-id loan-product-id)
          [true session])
        (do
          (log/errorf "!loanProductNotFound(input=%s)" input)
          [false session])))
    ;; invalid input - do nothing
    [false session]))

(defn set-loan-amount
  "Persists the amount requested by a customer in the current USSD session"
  [session input]
  (if-let [amount (try
                    (Integer/valueOf input)
                    (catch Exception ex#
                      (log/errorf ex# "invalidLoanAmountInput(%s) -> %s" input (.getMessage ex#))
                      nil))]
    (do
      (swap! session assoc :loan-request-amount amount)
      [true session])
    (do
      (log/errorf "!invalidAmount(%s)" input)
      [false session])))

(defn get-client-loan-accounts-list
  "Returns a clients list of loan accounts"
  [session list-index-counter]
  (if-let [loan-product-name (session :loan-product-name)]
    (let [loan-account-info (-> session :client-accounts :loanAccounts)
          loan-accounts (into [] (map (fn [{account-no :account-no}]
                                        (swap! list-index-counter inc)
                                        (format "%s. %s" @list-index-counter account-no))
                                      (loan-account-info loan-product-name)))]
      [true session @list-index-counter loan-accounts])
    [false (assoc session :state :no-client-loan-accounts) @list-index-counter nil]))

(defn set-client-loan-account
  "Sets a selected client loan account in the provided session"
  [session input]
  (if-let [input (try
                   (Integer/valueOf input)
                   (catch Exception ex#
                     (log/errorf ex# "invalidLoanAccountInput(%s) -> %s" input (.getMessage ex#))
                     nil))]
    ;; valid input
    (let [loan-product-name (@session :loan-product-name)
          loan-account-info (-> @session :client-accounts :loanAccounts)
          loan-product-info (nth (loan-account-info loan-product-name) (dec input))
          loan-account-no (loan-product-info :account-no)]
      (if loan-account-no
        (do
          (swap! session assoc :loan-account-no loan-account-no)
          [true session])
        (do
          (log/errorf "!loanAccountNotFound(accounts=%s)" loan-account-no)
          [false session])))
    ;; invalid input - do nothing
    [false session]))

(defn get-loan-account-balance [loan-account-no accountNo id]
  (try
    (let [ret (mifos/get-loan-info loan-account-no)
          {:keys [status error result]} ret]
      (if error
        (do
          (log/errorf "!getClientLoanBalances(acc-no=%s) -> %s"accountNo error)
          {:state :loan-account-balance-failed})
        (condp = status
          :ok
          result
          :failed
          (do
            (log/errorf "!getClientLoanBalances(acc-no=%s) -> failed" accountNo)
            {:state :loan-account-balance-failed})
          (do
            (log/errorf "!getClientLoanBalances(acc-no=%s) -> unexpected status [%s]" accountNo status)
            {:state :loan-account-balance-failed}))))
    (catch Exception ex
      (do
        (log/errorf ex "!loan-account-balance(acc-no=%s) -> %s" accountNo (.getMessage ex))
        {:state :loan-account-balance-failed}))))

(defn- finalize-loans []
  (log/info "finalize loans"))

;; --------------------------------------------------------------------------------
;;  Savings
;; --------------------------------------------------------------------------------
(defn- initialize-savings []
  (log/info "initializing savings"))

(defn get-client-savings-products-list
  "Generate a list of a clients savings products"
  [session list-index-counter]
  (if-let [client-accounts
           (or (session :client-accounts)
               (if-let [accounts (mifos/get-client-accounts (session :id))]
                 accounts nil))]
    (let [savings-accounts (client-accounts :savingsAccounts)
          savings-product-names (apply vector (keys savings-accounts))]
      (if-not (empty? savings-product-names)
        (let [product-names (map (fn [savings-product]
                                   (swap! list-index-counter inc)
                                   (format "%s. %s" @list-index-counter savings-product))
                                 savings-product-names)]
          [true (assoc session :client-accounts client-accounts :savings-products savings-product-names) @list-index-counter product-names])
        [false (assoc session :state :no-client-savings-products) @list-index-counter nil]))
    (do
      (log/error "!noClientAccountsthFound()")
      [false session @list-index-counter nil])))

(defn set-client-savings-product
  "Set a selected client savings product in the provided session"
  [session input]
  (if-let [input (try
                   (Integer/valueOf input)
                   (catch Exception ex#
                     (log/errorf ex# "invalidLoanInput(%s) -> %s" input (.getMessage ex#))
                     nil))]
    ;; valid input
    (let [savings-products# (@session :savings-products)]
      (log/infof "session=%s" @session)
      (if-let [savings-product-name (nth savings-products# (dec input))]
        (let [savings-account-info (-> @session :client-accounts :savingsAccounts)
              savings-product-info (first (savings-account-info savings-product-name))
              savings-product-id (savings-product-info :product-id)]
          (swap! session assoc :savings-product-name savings-product-name :savings-product-id savings-product-id)
          [true session])
        (do
          (log/errorf "!savingsProductNotFound(input=%s)" input)
          [false session])))
    ;; invalid input - do nothing
    [false session]))

(defn get-client-savings-accounts-list
  "Returns a clients list of savings accounts"
  [session list-index-counter]
  (if-let [savings-product-name (session :savings-product-name)]
    (let [savings-account-info (-> session :client-accounts :savingsAccounts)
          savings-accounts (into [] (map (fn [{account-no :account-no}]
                                        (swap! list-index-counter inc)
                                        (format "%s. %s" @list-index-counter account-no))
                                      (savings-account-info savings-product-name)))]
      [true session @list-index-counter savings-accounts])
    [false (assoc session :state :no-client-savings-accounts) @list-index-counter nil]))

(defn set-client-savings-account
  "Sets a selected client savings account in the provided session"
  [session input]
  (if-let [input (try
                   (Integer/valueOf input)
                   (catch Exception ex#
                     (log/errorf ex# "invalidLoanAccountInput(%s) -> %s" input (.getMessage ex#))
                     nil))]
    ;; valid input
    (let [savings-product-name (@session :savings-product-name)
          savings-account-info (-> @session :client-accounts :savingsAccounts)
          savings-product-info (nth (savings-account-info savings-product-name) (dec input))
          savings-account-no (savings-product-info :account-no)]
      (if savings-account-no
        (do
          (swap! session assoc :savings-account-no savings-account-no)
          [true session])
        (do
          (log/errorf "!savingsAccountNotFound(accounts=%s)" savings-account-no)
          [false session])))
    ;; invalid input - do nothing
    [false session]))

(defn get-savings-account-balance [savings-account-no]
  (try
    (let [ret (mifos/get-savings-account-info savings-account-no)
          {:keys [status error result]} ret]
      (if error
        (do
          (log/errorf "!getClientSavingsBalances(acc-no=%s) -> %s" savings-account-no error)
          {:state :savings-account-balance-failed})
        (condp = status
          :ok
          result
          :failed
          (do
            (log/errorf "!getClientSavingsBalances(acc-no=%s) -> failed" savings-account-no)
            {:state :savings-account-balance-failed})
          (do
            (log/errorf "!getClientSavingsBalances(acc-no=%s) -> unexpected status [%s]" savings-account-no status)
            {:state :savings-account-balance-failed}))))
    (catch Exception ex
      (do
        (log/errorf ex "!savingsAccountBalance(acc-no=%s) -> %s" savings-account-no (.getMessage ex))
        {:state :savings-account-balance-failed}))))

(defn- finalize-savings []
  (log/info "finalize savings"))

;; --------------------------------------------------------------------------------
;;  Transfers
;; --------------------------------------------------------------------------------
(defn- initialize-transfers []
  (log/info "initializing transfers"))

(defn- finalize-transfers []
  (log/info "finalize transfers"))

;; --------------------------------------------------------------------------------
;;  General utility functions
;; --------------------------------------------------------------------------------
(defn login [session input]
  (let [{:keys [first-login pin]
         :or {:first-login false}} @session
        {status :status} (if (and pin input)
                            (if (= (digest/md5 input) pin)
                              {:status :ok}
                              {:status :failed})
                            {:status :failed})]
    (condp = status
      :ok (if first-login
            (do
              (swap! session assoc :state :change-pin)
              [true session])
            [true session])
      [false session])))

(defn change-pin
  ""
  [session input]
  (if-let [new-pin (try
                     (Integer/valueOf input)
                     (catch Exception ex#
                       (log/errorf ex# "invalidPIN(%s) -> %s" input (.getMessage ex#))
                       nil))]
    (let [new-pin (digest/md5 (str new-pin))]
      (if (db/update-customer-info (:subscriber @session) {:pin new-pin :first_login 1})
        (do (swap! session assoc :pin new-pin)
            [true session])
        [false session]))
    [false session]))

(defn confirm-current-pin
  ""
  [session input]
  (let [{:keys [pin]} @session]
    (if (and pin input)
      (if (= (digest/md5 input) pin)
        [true session]
        [false session])
      [false session])))

(defn confirm-and-change-pin
  [session input]
  (let [new-pin (@session :new-pin)]
    (if (= new-pin input)
      (change-pin session new-pin)
      [false session])))

(defn update-session
  "Adds parameters and values to a ussd session"
  [session & args]
  (if args
    (let [%read-only-keys [:start-time :subscriber :client-accounts :id]
          args (apply hash-map args)
          ;; remove read-only keys from the parameters to prevent potential session data manipulation
          args (apply dissoc args %read-only-keys)
          ]
      (swap! session conj args)
      [true session])
    [false session]))

