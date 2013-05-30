(defproject core-cl2 "0.7.2"
  :description "Chlorine's bootstrap"
  :url "http://github.com/chlorinejs/core-cl2"
  :dependencies []
  :resource-paths ["src/cl2"]
  :profiles {:provided
             {:dependencies
              [[org.clojure/clojure "1.5.1"]
               [chlorine "1.5.3.1"]
               ]}})
