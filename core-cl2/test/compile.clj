(ns compile
  (:require [chlorine.js :refer [tojs' *temp-sym-count* *macros*]]
            [chlorine.util :refer [replace-map]]))

(defn -main [& files]
  (doseq [file files]
    (let [output
          (-> file
              clojure.java.io/file
              .getAbsolutePath
              (replace-map
               (array-map #".cl2$" ".js",
                          "/core-cl2/test/" "/test_runners/"))
              clojure.java.io/file)]
      (when-not (.isDirectory (.getParentFile output))
        (.mkdirs (.getParentFile output)))
      (spit output (binding [*temp-sym-count* (ref 999)
                             *macros*         (ref {})]
                     (tojs' file))))))
