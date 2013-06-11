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
     `(reduce' ~f ~val ~coll))
  ([f coll]
     (if (vector? coll)
       `(reduce' ~f ~(first coll) ~(rest coll)))
     `(let [coll ~coll
            init (first coll)
            tail (rest coll)]
        (reduce' ~f init tail))))

(defmacro reductions
  ([f val coll]
    `(reductions* ~f ~val ~coll))
  ([f coll]
     (if (vector? coll)
       `(reductions* ~f ~(first coll) ~(rest coll)))
     `(let [coll ~coll
            init (first coll)
            tail (rest coll)]
        (reductions* ~f init tail))))

(defmacro partition
  ([n coll]
     `(partion3 ~n ~n ~coll))
  ([n step coll]
     `(partition3 ~n ~step ~coll))
  ([n step pad coll]
     `(partition4 ~n ~step ~pad ~coll)))
