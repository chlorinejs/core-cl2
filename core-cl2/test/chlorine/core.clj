(ns chlorine.core
  (:use [clojure.test :only [deftest is]])
  (:require [chlorine.js]))

(defmacro js [& body]
  `(do
     (dosync (ref-set chlorine.js/*macros* {}))
     (chlorine.js/tojs [:resource "/dev.cl2"])
     (chlorine.js/js ~@body)))

(deftest types
  (is (= (js (isa? "foobar" "String"))
         "(\"foobar\" instanceof String)")))

(deftest combo
  (is (= (js
          (defn test [a] (if (! (or (boolean? a) (string? a))) (first a))))
         (str "test = function (a) {"
              " if (!((\"boolean\" === typeof(a)) ||"
              " (\"string\" === typeof(a))))"
              " { return a[0]; }; }")))
  (is (= (js
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no"
             :else "don't know")))
         (str "test = function (a) {"
              " if (symbol$QUEST$(a)) {"
              " return \"yes\"; }"
              " else {"
              " if ((\"number\" === typeof(a))) {"
              " return \"no\"; }"
              " else { if ('else') {"
              " return \"don't know\"; }"
              " else { return null; }; }; }; }")))

  (is (= (js
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no")))
         (str "test = function (a) {"
              " if (symbol$QUEST$(a)) {"
              " return \"yes\"; }"
              " else {"
              " if ((\"number\" === typeof(a))) {"
              " return \"no\"; }"
              " else {"
              " return null; }; }; }"))))

(deftest variables
  (is (= (js
           (lvar x 0)
           (set! x (+ x 1)))
         " var x = 0; x = (x + 1);")))

(deftest data-structures
    (is (= (js (contains? {:a 1} :a))
           "('a' in {'a' : 1})")))

(deftest function-tests
  (is (= (js (fn has-foo? [] (contains? {:foo 1 :bar 2} :foo)))
         (str "function has_foo$QUEST$ ()"
              " { return ('foo' in {'foo' : 1,'bar' : 2}); }")))
  (is (= (js (defn has-foo? [] (contains? {:foo 1 :bar 2} :foo)))
         (str "has_foo$QUEST$ = function ()"
              " { return ('foo' in {'foo' : 1,'bar' : 2}); }")))
  (is (= (js (defn fact [n]
               (loop [n n x (dec n)]
                 (if (= 1 x)
                   n
                   (recur (* n x) (dec x))))))
         (str "fact = function (n) {"
              " for (var n = n, x = (n - 1); true;) {"
              " if ($EQ$$STAR$(1, x)) { return n;"
              " } else {"
              " var _temp_1000 = [(n * x),(x - 1)];\n"
              " n = _temp_1000[0];"
              " x = _temp_1000[1];"
              " continue;"
              " }; break; }; }")
         )))

(deftest core-macros
  (is (= (js
           (. foo (bar 1 2 3)))
         "foo.bar(1, 2, 3)"))

  (is (= (js
          (.. foo (bar 1 2) (baz 3 4)))
         "foo.bar(1, 2).baz(3, 4)"))

  (is (= (js
          (-> foo bar baz))
         "baz(bar(foo))"))

  (is (= (js
          (-> foo (bar 1 2) (baz 3 4)))
         "baz(bar(foo, 1, 2), 3, 4)"))

  (is (= (js
          (->> foo (bar 1 2) (baz 3 4)))
         "baz(3, 4, bar(1, 2, foo))"))
  (is (= (js (isa? [ 1 2 3] "Array"))
         "([1,2,3] instanceof Array)"))
  (is (= (js (re-test #"/e/" "e"))
         "/e/.test(\"e\")"))
  (is (= (js (contains? [] 10))
         "(10 in [])")))

(deftest do-while-test
  (is (= (js
           (do-while (< x 10)
             (inc! x)))
         "do { x = (x + 1); } while ((x < 10))"
         ))
  (is (= (js
           (do-while (and (< x 10) (> x 5))
             (inc! x)))
         "do { x = (x + 1); } while (((x < 10) && (x > 5)))"
         )))

(deftest dofor-test
  (is (= (js
           (dofor [(lvar i 0
                        j 1)
                  (< i 5)
                  (inc! i)]
                 1))
         "for (var i = 0,j = 1;(i < 5);i = (i + 1)) { 1; }"
         ))
  (is (= (js
           (dofor [(def i 0)
                  (< i 5)
                  (inc! i)]
                 1))
         "for (var i = 0;(i < 5);i = (i + 1)) { 1; }"
         ))
  (is (= (js
          (dofor [[i 0 j 1]
                  (< i 5)
                  (inc! i)]
                 1))
         "for (var i = 0,j = 1;(i < 5);i = (i + 1)) { 1; }")))
