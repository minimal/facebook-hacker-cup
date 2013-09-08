(defproject fb-lines "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/core.typed "0.2.4"]]
  :core.typed {:check [fb-lines.core]}
  :main fb-lines.core)
