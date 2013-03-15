(defmacro not [expr] `(! ~expr))
(defmacro contains? [m k]
  `(in ~k ~m))
(defmacro true? [expr] `(=== true ~expr))
(defmacro false? [expr] `(=== false ~expr))
(defmacro nil? [expr] `(=== nil ~expr))

(defmacro first [x] `(get* ~x 0))
(defmacro second [x] `(get* ~x 1))
(defmacro last [x] `(get* ~x (- (count ~x) 1)))
(defmacro next [x] `(if (< 1 (count ~x)) (.slice ~x 1)))
(defmacro nnext [x] `(next (next ~x)))
(defmacro rest [x] `(.slice ~x 1))

(defmacro array? [a] `(isa? ~a "Array"))
(defmacro string? [s] `(=== "string" (typeof ~s)))
(defmacro number? [n] `(=== "number" (typeof ~n)))
(defmacro boolean? [b] `(=== "boolean" (typeof ~b)))
(defmacro fn? [f] `(=== "function" (typeof ~f)))
(defmacro regexp? [re] `(isa? ~re "RegExp"))

(defmacro inc [arg] `(+ 1 ~arg))
(defmacro dec [arg] `(- ~arg 1))

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

(defmacro =
  ([] `true)
  ([x] `true)
  ([x y] `(=* ~x ~y))
  ([x y & more] `(=* ~x ~y ~@more)))
