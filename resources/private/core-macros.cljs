(defmacro true? [expr] `(=== true ~expr))
(defmacro false? [expr] `(=== false ~expr))
(defmacro undefined? [expr] `(=== undefined ~expr))
(defmacro nil? [expr] `(=== nil ~expr))

(defmacro first [x] `(get ~x 0))
(defmacro second [x] `(get ~x 1))
(defmacro third [x] `(get ~x 2))
(defmacro last [x] `(get ~x (- (count ~x) 1)))

(defmacro array? [a] `(isa? ~a "Array"))
(defmacro string? [s] `(=== "string" (typeof ~s)))
(defmacro number? [n] `(=== "number" (typeof ~n)))
(defmacro boolean? [b] `(=== "boolean" (typeof ~b)))
(defmacro fn? [f] `(== "function" (typeof ~f)))

(defmacro str [& args] `(+ "" ~@args))
(defmacro inc [arg] `(+ 1 ~arg))
(defmacro dec [arg] `(- ~arg 1))