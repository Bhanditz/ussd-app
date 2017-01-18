(ns
  ^{:doc "Session Management Functions"
    :author "Nahashon Kibet | mckibet@gmail.com"}
  com.argent.mifosmobile.sessions
  (:require [com.argent.common.date :as date]
            [clojure.tools.logging :as log]
            [com.argent.common.config :as config]))

(def session-data (atom {}))
(def session-timer (atom nil))
(def read-only-keys [:start-time :subscriber])

(declare session-exists? clear-sessions)

(defn initiate-session-manager
  "Initializes the session store management functions"
  []
  (log/info "Initialize session manager")
  (reset! session-timer (doto (java.util.Timer. "session-cleaner" false)
                          (.scheduleAtFixedRate (proxy [java.util.TimerTask] []
                                                  (run []
                                                    (clear-sessions)))
                                                (java.util.Date.)
                                                config/session-clear-interval))))

(defn start-new-session
  "Initiates a new session if non exists and returns the session's data"
  [session-id subscriber]
  (log/debugf "startNewSession(%s,%s)"  session-id subscriber)
  ;; make sure we dont already have a session
  (if (session-exists? session-id subscriber)
    (throw (Exception. (format "duplicateSession(sid=%s, %s)" session-id subscriber)))
    (let [session {:start-time (date/now)
                   :subscriber subscriber}]
      (swap! session-data assoc session-id session)
      session)))

(defn session-exists?
  "Checks if a session assocated to a given ID and subscriber exists"
  [session-id subscriber]
  (if-let [session (@session-data session-id)]
    (let [session-subscriber (session :subscriber)]
      (if (= subscriber session-subscriber)
        true
        (throw (Exception. (format "!sessionMismatch(sid=%s, expected=%s,found=%s" session-id session-subscriber subscriber)))))
    false))

(defn get-session-data
  "Returns data for an existing session"
  [session-id subscriber]
  (log/debugf "getSessionData(%s,%s)" session-id subscriber)
  (if (session-exists? session-id subscriber)
    (@session-data session-id)
    (throw (Exception. (format "!sessionNotFound(sid=%s,%s)" session-id subscriber)))))

(defn update-session-data
  "Updates session data and returns the updated session"
  [session-id subscriber data]
  (log/debugf "updateSession(%s, sub=%s, data=%s)" session-id subscriber @data)
  (if (session-exists? session-id subscriber)
    ;; remove read-only keys in the data
    (let [data (apply dissoc @data read-only-keys)
          session (@session-data session-id)
          new-session (conj session (or data {}) {:last-updated (date/now)})]
      (swap! session-data assoc session-id new-session)
      new-session)
    (throw (Exception. (format "!sessionNotFound(sid=%s,%s)" session-id subscriber)))))

(defn end-session
  "Ends a session by clearing associated data from the session store"
  [session-id subscriber]
  (log/debugf "clearSession(%s,%s)" session-id subscriber)
  (if (session-exists? session-id subscriber)
    (swap! session-data dissoc session-id)
    (throw (Exception. (format "!sessionNotFound(sid=%s,%s)" session-id subscriber)))))

(defn clear-sessions
  "Clears expired sessions from the session store "
  []
  (log/info "startSessionCleanUp()")
  (let [expired-sessions (atom 0)]
    (doall (map (fn [[id session]]
                  (let [{:keys [start-time last-updated]} session
                        life-time (date/interval (or last-updated start-time) (date/now))]
                    ;;
                    (when (> life-time config/session-max-lifetime)
                      (swap! expired-sessions inc)
                      (log/debugf "sessionExpired(sid=%s, started=%s, last-updated=%s, lifetime=%s)" id start-time last-updated life-time)
                      (swap! session-data dissoc id))))
                @session-data))
    (log/infof "doneSessionCleanUp(expiredSessionsCount=%s)" @expired-sessions)))

;; To do:
;; 1. Collect session statistics:
;;    a) complete sessions
;;    b) incomplete sessions
;;