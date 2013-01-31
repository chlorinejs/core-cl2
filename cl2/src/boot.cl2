(defmacro borrow-macros [& syms] (apply chlorine.js/borrow-macros syms))
(defmacro apply [fun & args] `(.apply ~fun 0 ~@args))
(borrow-macros when when-not unless if-not if-let when-let cond .. -> ->>)

(defmacro fn [& fdeclrs]
  (let [valid-fn-declrs? (fn [fdeclrs]
                           (and (list? fdeclrs)
                                (vector? (first fdeclrs))))
        original-fdeclrs fdeclrs
        [fname fdeclrs] (if (symbol? (first fdeclrs))
                          [(first fdeclrs) (rest fdeclrs)]
                          [nil fdeclrs])
        [docstring fdeclrs] (if (string? (first fdeclrs))
                              [(first fdeclrs) (rest fdeclrs)]
                              [nil fdeclrs])
        ]
    ;; Cases:
    ;; * (fn _ _ | [args] body)
    ;; * (fn _ _ | ([args] body))
    ;; * (fn _ _ | ([args-1] body-1) ([args-n] body-n))
    (cond
     (vector? (first fdeclrs))
     `(fn* ~@original-fdeclrs)

     (and (= 1 (count fdeclrs))
          (apply valid-fn-declrs? fdeclrs))
     `(fn* ~@(concat (remove nil? [fname docstring]) (first fdeclrs)))

     (and (< 1 (count fdeclrs))
          (every? valid-fn-declrs? fdeclrs))
     (let [count-arg (fn [v] (if (contains? (set v) '&)
                               :variadic
                               (count v)))]
       `(fn* ~@(concat (remove nil? [fname docstring])) []
          (def args arguments)

          ~(concat ['case `(count args)]
                   (apply concat
                          (for [fdeclr# fdeclrs]
                            (let [[v# _] fdeclr#]
                              (if (= (count-arg v#) :variadic)
                                (vector `(let [f ~(cons 'fn fdeclr#)]
                                           (return (apply f args))))
                                (vector (count-arg v#)
                                        `(let [f ~(cons 'fn fdeclr#)]
                                           (return (apply f args))))))))))
       )
     )))

(defmacro defn [fname & fdeclrs]
  (chlorine.js/undef-macro fname)
  `(set! ~fname ~(cons 'fn fdeclrs)))

(defmacro defn- [fname & fdeclrs]
  (chlorine.js/undef-macro fname)
  `(def ~fname ~(cons 'fn fdeclrs)))

(defmacro lvar [& bindings]
  `(inline
    ~(str "var "
          (clojure.string/join ","
            (map (fn [[vname vval]]
                   (str vname " = " (chlorine.js/emit-str vval)))
                 (partition 2 bindings))))))

(defmacro dotimes [[var n] & body]
  (let [nsym (gensym)]
    `(do
       (lvar ~nsym ~n)
       (loop [~var 0]
         (when (< ~var ~nsym)
           ~@body
           (recur (+ ~var 1)))))))

(defmacro doseq [[bindings coll] & body]
  (cond
   (symbol? bindings)
   `(let [m# ~coll]
      (dokeys [i# m#]
              (let [~bindings (get m# i#)]
                ~@body)))
   (vector? bindings)
   (let [[kname# vname#] bindings]
     `(let [m# ~coll]
        (dokeys [~kname# m#]
                (let [~vname# (get m# ~kname#)]
                  ~@body))))))

(defmacro for [[bindings coll] & body]
  (cond
   (symbol? bindings)
   `(let [m# ~coll]
      (def ret# [])
      (dokeys [i# m#]
              (.push ret#
                     (let [~bindings (get m# i#)]
                       ~@body)))
      ret#)
   (vector? bindings)
   (let [[kname# vname#] bindings]
     `(let [m# ~coll]
        (def ret# [])
        (dokeys [~kname# m#]
                (.push ret#
                       (let [~vname# (get m# ~kname#)]
                         ~@body)))
        ret#))))

(defmacro . [x & tail]
  (let [dot-form-for (fn [x] (symbol (str "." (name x))))
        [method args] (cond
                        (< 1 (count tail))
                        [(dot-form-for (first tail)) (rest tail)]

                        :default
                        (if (seq? (first tail))
                          [(dot-form-for (first (first tail)))
                           (rest (first tail))]
                          [(dot-form-for (first tail)) ()])
                        )]
    `(~method ~x ~@args)))

(defmacro re-test [regexp s]
  `(.. ~regexp (test ~s)))

(defmacro re-exec [regexp s]
  `(.. ~regexp (exec ~s)))

(defmacro defmulti [fname dispatch-fn]
  `(do
     (defn ~fname [& args]
       (let [dispatch-fn# ~(cond
                            (keyword? dispatch-fn)
                            (list 'fn '[obj] (list '-> 'obj dispatch-fn))

                            :default
                            dispatch-fn)
             dispatched-val# (apply dispatch-fn# args)]
         (if (contains? (-> ~fname :methods) dispatched-val#)
           (let [dispatcher# (get (-> ~fname :methods) dispatched-val#)]
             (apply dispatcher# args))
           (if (fn? (-> ~fname :default-method))
             (let [default-method# (-> ~fname :default-method)]
               (apply default-method# args))
             (throw
              (str "No method in multimethod '" ~(name fname)
                   "' for dispatch value: " dispatched-val#))))))
     (set! (-> ~fname :methods) {})))

(defmacro defmethod [fname dispatch-val & fdeclr]
  (let [setee (if (= :default dispatch-val)
                `(-> ~fname :default-method)
                `(get (-> ~fname :methods) ~dispatch-val))]
       `(set! ~setee ~(cons 'fn fdeclr))))

(defmacro get
  ([coll index]
     `(get ~coll ~index))
  ([coll index not-found]
     `(or (get ~coll ~index)
          ~not-found)))

(defmacro nth [& args] `(get ~@args))

(defmacro as-fn [x]
  (cond (keyword? x) `(fn [coll] (get coll ~x))
        (set? x)     `(fn [y] (contains? ~x y))))