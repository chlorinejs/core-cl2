(ns chlorine.inlines
  (:use [clojure.test :only [deftest is]])
  (:require [chlorine.js :as cl2]))

(defmacro js [& body]
  `(binding [cl2/*temp-sym-count* (ref 999)
             cl2/*last-sexpr*     (ref nil)
             cl2/*macros*         (ref {})]
     (cl2/tojs' "r:/dev.cl2")
     (cl2/js ~@body)
     ))

(deftest types
  (is (= (js (isa? "foobar" "String"))
         "(\"foobar\" instanceof String)")))
