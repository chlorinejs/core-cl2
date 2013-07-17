(ns compile
  (:require [chlorine.js :refer [tojs]]
            [chlorine.util :refer [replace-map]]))

(defn -main [& files]
  (doseq [file files
          :let [file (-> file
                         clojure.java.io/file
                         .getAbsolutePath)]]
    (let [output
          (-> file
              (replace-map
               (array-map #".cl2$" ".js",
                          "/core-cl2/test/" "/test_runners/"))
              clojure.java.io/file)]
      (when-not (.isDirectory (.getParentFile output))
        (.mkdirs (.getParentFile output)))
      (spit output (tojs file)))))
