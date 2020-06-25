(defproject com.guaranteedrate/lazy-map "0.1.7-SNAPSHOT"

  :description
  "Lazy maps for Clojure"

  :url
  "https://github.com/Guaranteed-Rate/lazy-map"

  :license
  {:name "Eclipse Public License"
   :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.8.0"]]

  :plugins
  [[lein-cloverage "1.1.2"]]

  :deploy-repositories
  [["releases" :clojars]
   ["snapshots" :clojars]])
