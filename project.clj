(defproject ussd-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/tools.logging "0.3.0"]
                 [log4j "1.2.17"]
                 [clj-http "1.0.1"]
                 [http-kit "2.1.18"]
                 [ring "1.3.2"]
                 [compojure "1.3.1"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [com.mchange/c3p0 "0.9.5"]
                 [com.mchange/mchange-commons-java "0.2.9"]
                 [org.postgresql/postgresql "9.3-1100-jdbc41"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [clj-time "0.9.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [digest "1.4.4"]
                 [com.rabbitmq/amqp-client "3.5.1"]
                 [com.novemberain/langohr "3.2.0"]]
  ;:main ^:skip-aot mifos-mobile.core
  :plugins [[lein-ring "0.9.1"]]
  :ring {:handler com.argent.mifosmobile.service/all-routes
         :uberwar-name mifosmobile.war
         ;:web-xml "resources/web.xml"
         :init com.argent.mifosmobile.service/initialize
         :destroy com.argent.mifosmobile.service/destroy}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
