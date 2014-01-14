(defproject core-cl2 "0.9.0-SNAPSHOT"
  :description "Chlorine's bootstrap"
  :url "http://github.com/chlorinejs/core-cl2"
  :dependencies []
  :resource-paths ["src/"]
  :profiles {:provided
             {:dependencies
              [[org.clojure/clojure "1.5.1"]
               [chlorine "1.6.4-SNAPSHOT"]
               ]}}
  :nodejs {:keywords ["chlorinejs", "clojure", "core" "macro"]
           :devDependencies {:mocha ">= 1.7.4"}
           :scripts {:mocha "mocha -u qunit test/test_runner.js"
                     :mocha-auto "mocha -w -u qunit test/test_runner.js"}}
  :plugins [[lein-cl2c "0.0.1-SNAPSHOT"]
            [lein-npm "0.2.0-SNAPSHOT"]]
  :cl2c {:compile
         {:watch ["src", "test"]
          :filter "src/"
          :path-map ["src/" => "lib/"]
          :paths ["node_modules/"]
          :strategy "prod"
          ;; some files may take too long to compile. We need a limit
          :timeout 2000
          }
         :dev
         {:watch ["src", "test"]
          :filter "test/test_runner.cl2"
          :paths ["node_modules/" "src/"]
          :strategy "dev"
          ;; some files may take too long to compile. We need a limit
          :timeout 2000
          }})
