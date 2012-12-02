(defmacro apply [fun & args] `(.apply ~fun ~fun ~@args))
(defmacro not [expr] `(! ~expr))
(defmacro contains? [m k]
  `(in ~k ~m))

(defmacro when [pred & body] `(if ~pred (do ~@body)))
(defmacro when-not [pred & body] `(if (not ~pred) (do ~@body)))
(defmacro unless [pred & body] `(if (not ~pred) (do ~@body)))
(defmacro cond [& [pred consequent & alternates]]
  (if (coll? alternates)
    (if (= (first alternates) :else)
      `(if ~pred ~consequent ~(second alternates))
      `(if ~pred ~consequent (cond ~@alternates)))
    `(if ~pred ~consequent)))
(defmacro isa? [a t]
  `(instanceof ~a ~(symbol t)))
(defmacro join [sep seq] `(.join ~seq ~sep))
(defmacro inc! [arg] `(set! ~arg (+ 1 ~arg)))
(defmacro dec! [arg] `(set! ~arg (- ~arg 1)))
(defmacro inc-after! [arg] `(inline ~(str arg "++")))
(defmacro dec-after! [arg] `(inline ~(str arg "--")))
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

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro ->
  ([x] x)
  ([x form] (if (seq? form)
              `(~(first form) ~x ~@(next form))
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

(defmacro ->>
  ([x form] (if (seq? form)
              `(~(first form) ~@(next form)  ~x)
              (list form x)))
  ([x form & more] `(->> (->> ~x ~form) ~@more)))

(defmacro re-test [regexp s]
  `(.. ~regexp (test ~s)))

(defmacro re-exec [regexp s]
  `(.. ~regexp (exec ~s)))

(defmacro defmulti [fname dispatch-fn]
  `(do
     (let [dispatch-fn# ~(cond
                         (keyword? dispatch-fn)
                         (list 'fn '[obj] (list '-> 'obj dispatch-fn))

                         :default
                         dispatch-fn)]
       (defn ~fname [& args]
         (let [dispatched-val# (apply dispatch-fn# args)]
           (if (contains? (-> ~fname :methods) dispatched-val#)
             (let [dispatcher# (get (-> ~fname :methods) dispatched-val#)]
               (apply dispatcher# args))
             (if (fn? (-> ~fname :default-method))
               (let [default-method# (-> ~fname :default-method)]
                 (apply default-method# args))
               (throw
                (str "No method in multimethod '" ~(name fname)
                     "' for dispatch value: " dispatched-val#))))))
       (set! (-> ~fname :methods) {}))))

(defmacro defmethod [fname dispatch-val & fdeclr]
  (let [setee (if (= :default dispatch-val)
                `(-> ~fname :default-method)
                `(get (-> ~fname :methods) ~dispatch-val))]
       `(set! ~setee ~(cons 'fn fdeclr))))

(defmacro defn* [fname & fdeclrs]
  (let [count-arg (fn [v] (if (contains? (set v) '&)
                            :variadic
                            (count v)))
        runner-defs (for [fdeclr# fdeclrs]
                      (let [[v# _] fdeclr#]
                        `(set! (get (-> this :args) ~(count-arg v#))
                               ~(cons 'fn fdeclr#))))]
    `(defn ~fname [& args]
       (set! (-> this :args) {})
       ~runner-defs
       (let [n# (count args)]
         (if (contains? (-> this :args) n#)
           (let [runner# (get (-> this :args) n#)]
             (apply runner# args))
           (if (contains? (-> this :args) :variadic)
             (let [runner# (get (-> this :args) :variadic)]
               (apply runner# args))
             (throw (str "Wrong number of args (" n#
                         ") passed to: " ~(name fname)))))))))

(defmacro include-core! []
  `(include! [:resource
              "/private/core.cl2"
              "/private/core-funcs.cl2"]))

(defmacro include-core-macros! []
  `(include! [:resource
              "/private/core-macros.cl2"
              "/private/core.cl2"]))