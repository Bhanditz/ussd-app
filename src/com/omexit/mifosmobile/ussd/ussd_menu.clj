(ns com.omexit.mifosmobile.ussd.ussd-menu
  (:require [clojure.tools.logging :as log]
            [com.omexit.common.config :as config]
            [com.omexit.mifosmobile.sessions :as session]
            [com.omexit.mifosmobile.ussd.ussd-utils :as ussd-utils]
            [clojure.data.zip.xml :as zip-xml :only [xml1]]
            [clojure.zip :as zip :only [xml-zip]]))

(declare define-automaton define-render)
(def ^:dynamic *session-data*)
(def ^:dynamic *regex*)

(defn
  initialize-ussd-menu
  []
  (log/info "Initializing USSD Menu")
  (ussd-utils/initialize-utils)
  (define-automaton config/ussd-state-transitions)
  (define-render config/ussd-state-renders))

(declare automaton-def renderer-def)

(defn define-automaton
  [transition-table]
  (def state-automaton (eval (automaton-def 'session-data transition-table)))
  (def valid-states  (let [+valid-states+ (atom #{})]
                       (doseq [{state :tag transitions :content} transition-table]
                         (when state (swap! +valid-states+ conj state))
                         (doseq [{:keys [attrs]} transitions]
                           (let [{:keys [st sf]} attrs]
                             (when st (swap! +valid-states+ conj (keyword st)))
                             (when sf (swap! +valid-states+ conj (keyword sf))))))
                       @+valid-states+))
  (def final-states (let [+final-states+ (atom #{})]
                      (doseq [{state :tag transitions :content} transition-table]
                        (when-not transitions (swap! +final-states+ conj state)))
                      @+final-states+))
  (defn final-state? [state] (if (final-states state) true false))
  (defn valid-state? [state] (if (valid-states state) true false)))

(defn define-render
  [state-renders]
  (def render-state (eval (renderer-def 'session-data state-renders))))

;; ----------------------------------------------------------------------------
;;  Function generators
;; ----------------------------------------------------------------------------
(defn- automaton-def
  [session-data transition-table]
  (let [ussd-input (gensym "input")]
    `(fn [~session-data ~ussd-input]
       (let [~'input ~ussd-input
             ; actions function
             ~'login ussd-utils/login
             ~'confirm-current-pin ussd-utils/confirm-current-pin
             ~'change-pin ussd-utils/change-pin
             ~'confirm-and-change-pin ussd-utils/confirm-and-change-pin
             ~'lend? (fn [session#]
                           (let [{:keys [~'loan-balance] :or {~'loan-balance 0}} @session#]
                             (if (= ~'loan-balance 0)
                               [true session#]
                               (do
                                 (swap! session# assoc :state :has-loan)
                                 [false session#]))))
             ~'update-session ussd-utils/update-session
             ;; loans
             ~'set-client-loan-product ussd-utils/set-client-loan-product
             ~'set-client-loan-account ussd-utils/set-client-loan-account
             ~'set-loan-product ussd-utils/set-loan-product
             ~'set-loan-amount ussd-utils/set-loan-amount
             ;; savings
             ~'set-client-savings-product ussd-utils/set-client-savings-product
             ~'set-client-savings-account ussd-utils/set-client-savings-account]
         ;;
         (let [current-state# ((deref ~session-data) :state)]
           (condp = current-state#
             ~@(mapcat (fn [{state :tag transitions :content}]
                       (let [state-transitions
                             `(condp = ~ussd-input
                                ~@(mapcat (fn [{:keys [attrs]}]
                                            (let [{:keys [input action st sf]} attrs
                                                  action# (if (or (= action "") (nil? action))
                                                           nil
                                                           (let [action-str-fn
                                                                 (if (.startsWith action "update-session")
                                                                   (let [args (clojure.string/split action #" ")
                                                                         args (remove #(.isEmpty %) args)
                                                                         action (first args)
                                                                         session-args (rest args)
                                                                         args (apply str
                                                                                     (interpose " "
                                                                                                (conj session-args session-data)))]
                                                                     (format "(%s %s)" action args))
                                                                   (format "(%s %s %s)" action session-data ussd-input))]
                                                             (read-string action-str-fn)))
                                                  body `(do
                                                          ~(if action#
                                                             `(let [[ok# updated-session-data#] ~action#
                                                                   new-state# (@updated-session-data# :state)]
                                                               (log/debugf "session2=%s,input=%s" ~'session-data ~'input)
                                                               (if ok#
                                                                 (if (= new-state# :other)
                                                                   @updated-session-data#
                                                                   (swap! updated-session-data# assoc :state ~(keyword st)))
                                                                 (swap! ~session-data assoc :state ~(keyword sf))))
                                                             `(swap! ~session-data assoc :state ~(keyword st))))]
                                              ;;
                                              (log/debugf "transition(state=%s, input=%s, action=%s)" state input action)
                                              (if (= input "*")
                                                (list body)
                                                `(~input ~body))))
                                          transitions))]
                         (log/debugf "state=%s, transitions=%s" state state-transitions)
                         (list state state-transitions)))
                     transition-table)
             (throw (Exception. (format "unknownState(%s)" current-state#)))))))))

(defn- renderer-def
  [session-data state-renders]
  `(fn [~session-data]
     (let [~'list-index-counter#      (atom 0) ;; for tracking the index of a list
           ;; loans
           ~'get-loan-products        ussd-utils/get-loan-products-list
           ~'get-client-loan-products ussd-utils/get-client-loan-products-list
           ~'get-client-loan-accounts ussd-utils/get-client-loan-accounts-list
           ;; savings
           ~'get-client-savings-products ussd-utils/get-client-savings-products-list
           ~'get-client-savings-accounts ussd-utils/get-client-savings-accounts-list
           {:keys [~'subscriber]} ~session-data
           ~'session-data# (atom ~session-data)]
       (condp = (~session-data :state)
         ~@(mapcat (fn [{state# :tag automaton# :content}]
                     (let [list-actions# (into {} (map-indexed  (fn [indx {tag# :tag attrs# :attrs}]
                                                         (condp = tag#
                                                           :text nil
                                                           :list (if-let [action# (attrs# :action)]
                                                                   (let [action# (read-string (format "(%s %s %s)" action# session-data 'list-index-counter#))]
                                                                     {(keyword (format "list-%s" indx)) action#})
                                                                   (throw (Exception. (format "missingActionForTag(%s)" tag#))))
                                                           (throw (Exception. (format "unknownElement(%s)" tag#)))))
                                                       automaton#))
                           body `(let [state-texts# (apply concat (map-indexed (fn [indx# {tag# :tag content# :content attrs# :attrs}]
                                                                                 (condp = tag#
                                                                                   :text
                                                                                   (if content# content# "")
                                                                                   :list
                                                                                   (let [{text# :text no-list# :nolist} attrs#
                                                                                         list-no# (keyword (format "list-%s" indx#))
                                                                                         action# (list-no# ~list-actions#)
                                                                                         [ok# updated-session# list-counter# content#] action#]
                                                                                     (reset! ~'session-data# updated-session#)

                                                                                     (reset! ~'list-index-counter# list-counter#)
                                                                                     ;(session/update-session-data ~'session-id ~'subscriber (atom updated-session#))
                                                                                     (if ok#
                                                                                       (apply vector (conj (apply list content#) text#))
                                                                                       (if (nil? content#)
                                                                                         [(or no-list# "No items returned!")]
                                                                                         content#)))
                                                                                   (throw (Exception. (format "unknownElement(%s)" tag#)))))
                                                                               ~automaton#))
                                       count-of-lines# (- (count state-texts#) 1)
                                       state-texts# (doall (map-indexed
                                                            (fn [line# text#]
                                                              (if (< line# count-of-lines#)
                                                                (with-out-str (println text#))
                                                                (with-out-str (print text#))))
                                                            state-texts#))]
                                   (reset! ~'list-index-counter# 0)
                                   [~'session-data# (apply str state-texts#)])]
                       (list state# body)))
                   state-renders)))))
