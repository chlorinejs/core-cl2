;; "Inline" is a optimization technique where a function call inside an other
;; form is emitted as smaller pieces of code while the outer form should still
;; return the same result.
;; ChlorineJS hasn't implemented :inline syntax for `defn` macro as seen
;; in Clojure.
;; To make the magic happen, ChlorineJS currently use macros to
;; ensure the specified forms are emitted as their expanded forms instead.

(defmacro not [expr] `(! ~expr))

(defmacro get
  ([m k]
     `(get* ~m ~k))
  ([m k not-found]
     `(get' ~m ~k ~not-found)))

(defmacro contains? [m k]
  `(in ~k ~m))
(defmacro true? [expr] `(=== true ~expr))
(defmacro false? [expr] `(=== false ~expr))
(defmacro nil? [expr] `(=== nil ~expr))

(defmacro first [x] `(get* ~x 0))
(defmacro second [x] `(get* ~x 1))

(defmacro next [x] `(if (< 1 (count ~x)) (.slice ~x 1)))
(defmacro nnext [x] `(next (next ~x)))
(defmacro rest [x] `(.slice ~x 1))

(defmacro set? [x] `(isa? ~x "Cl2Set"))

(defmacro vector? [a] `(isa? ~a "Array"))

(defmacro string?
  [x]
  `(or (=== "string" (typeof ~x))
       (isa? ~x "String")))

(defmacro number? [n] `(=== "number" (typeof ~n)))

(defmacro fn? [f] `(=== "function" (typeof ~f)))

(defmacro +
  ([] `0)
  ([x] x)
  ([x & more] `(+* ~x ~@more)))

(defmacro -
  ([] `0)
  ([x] x)
  ([x & more] `(-* ~x ~@more)))

(defmacro *
  ([] `1)
  ([x] x)
  ([x & more] `(** ~x ~@more)))

(defmacro /
  ([x] (/ 1 x))
  ([x & more] `(js-divide ~x ~@more)))

(defmacro inc [arg] `(+ 1 ~arg))

(defmacro dec [arg] `(- ~arg 1))
