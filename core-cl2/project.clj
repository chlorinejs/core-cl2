(defproject core-cl2 "0.7.0"
  :description "Chlorine's bootstrap"
  :url "http://github.com/chlorinejs/core-cl2"
  :dependencies []
  :resource-paths ["src/cl2"]
  :test-paths ["test/clj"]
  :profiles {:provided
             {:dependencies
              [[org.clojure/clojure "1.5.1"]
               [chlorine "1.5.2.1"]
               ]}})
