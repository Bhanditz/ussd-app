;

(ns com.argent.common.macros
  (:require [clojure.tools.logging :as log]))

(defmacro with-timed-try
  "macro to execute blocks that need to be timed and may throw exceptions"
  [tag fn-on-failure & body]
  `(let
     [fn-name# (str "do" (.toUpperCase (subs ~tag 0 1)) (subs ~tag 1))
      start-time# (System/currentTimeMillis)
      e-handler# (fn [err# fname#] (log/errorf err# "!%s -> %s" fname# (.getMessage err#)) :error)
      return-val# (try
                    ~@body
                    (catch Exception se#
                      (if (fn? ~fn-on-failure)
                        (~fn-on-failure se#)
                        (e-handler# se# fn-name#))))]
     (log/infof "%s -> %s" fn-name# return-val#)
     (log/infof "callProf|%s -> %sms" fn-name# (- (System/currentTimeMillis) start-time#))
     return-val#))
