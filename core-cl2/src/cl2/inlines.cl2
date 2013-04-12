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

(defmacro inc [arg] `(+ 1 ~arg))

(defmacro dec [arg] `(- ~arg 1))

(defmacro =
  ([] `true)
  ([x] `true)
  ([x y]
     ;; things to be compared using `===`
     (if (or (some #{'null}
                   [x y])
             (some #(or (number? %)
                        (keyword? %)
                        (string? %)
                        (true? %)
                        (false? %)
                        (nil? %))
                   [x y]))
       `(=== ~x ~y)
       `(=* ~x ~y)))
  ([a b & c]
     (let [args `(~a ~b ~@c)
           truthy-primitive-member
           (or (some #{'null}
                     args)
               (some #(if (or (number? %)
                              (keyword? %)
                              (string? %)
                              (true? %))
                        %)
                     args))
           falsey-primitive-member?
           (some #(cond (= false %) :false (= nil %) :nil) args)
           falsey-primitive-member
           (when falsey-primitive-member?
             (case falsey-primitive-member?
               :false false
               :nil nil))
           primitive-member?
           (or truthy-primitive-member
               falsey-primitive-member?)
           primitive-member
           (or truthy-primitive-member
               falsey-primitive-member)]
       (if primitive-member?
         (let [args (remove #(= primitive-member %) args)]
           (case (count args)
             0 `true
             1 `(=== ~primitive-member ~@args)
             2 `(and (=== ~primitive-member ~(first  args))
                     (=== ~primitive-member ~(second args)))
             ;; else
             `(=** ~primitive-member ~@args)))
         `(=' ~@args)))))

(defmacro reduce
  ([f val coll]
     `(reduce* ~f ~val ~coll))
  ([f coll]
     (if (vector? coll)
       `(reduce* ~f ~(first coll) ~coll))
     `(let [coll ~coll
            init (first coll)]
        (reduce* ~f init coll))))

(defmacro reductions
  ([f val coll]
    `(reductions* ~f ~val ~coll))
  ([f coll]
     (if (vector? coll)
       `(reductions* ~f ~(first coll) ~coll))
     `(let [coll ~coll
            init (first coll)]
        (reductions* ~f init coll))))
