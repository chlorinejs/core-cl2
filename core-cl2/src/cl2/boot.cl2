(defmacro isa?
  [a t]
  `(instanceof ~a ~(symbol t)))

(defmacro borrow-macros
  "Reuses clojure.core macros in Chlorine."
  [& syms]
  (apply chlorine.js/borrow-macros syms))

(borrow-macros when when-not unless if-not if-let when-let cond
               condp .. -> ->> cond-> cond->> doto)

(defmacro apply [fun & args] `(.apply ~fun 0 ~@args))

(defmacro fn
  "Function form. Supports multi-arity."
  [& fdeclrs]
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
                               (count v)))
           root-arguments (gensym "args")
           children
           (for [fdeclr# fdeclrs]
             (let [[v# _]    fdeclr#
                   arg-count (count-arg v#)]
               (let [child-name (gensym "f")
                     excution-form
                     `(apply ~child-name ~root-arguments)]
                 {:child-declr
                  `(def ~child-name (fn* ~@fdeclr#))
                  :case-form
                  (if (= arg-count :variadic)
                    ;; in `case`, default values are not specified
                    [excution-form]
                    ;; other cases:
                    [arg-count excution-form])})))
           child-dclrs   (map    :child-declr children)
           case-forms    (apply concat (map :case-form   children))]
       `(fn* ~@(concat (remove nil? [fname docstring]))
             []
             (def ~root-arguments arguments)
             ~@child-dclrs
             (case (count ~root-arguments)
               ~@case-forms)))
     :otherwise
     (throw (Exception. "Invalid function declaration!"))
     )))

(defmacro defn
  "Defines a function."
  [fname & fdeclrs]
  ;; Will undefine macros when ^:inline is implemented.
  ;; Chlorine currently uses macros instead of ^:inline
  ;;(chlorine.js/undef-macro fname)
  ;; If fname contains "." which means member access, use `set!`
  ;; else use `def`
  (let [setter (if ((set (name fname)) \.)
                 'set!
                 'def)]
    `(~setter ~fname ~(cons 'fn fdeclrs))))

(defmacro dotimes
  "Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [[var n] & body]
  (let [nsym (gensym)]
    `(do
       (let* ~nsym ~n)
       (loop [~var 0]
         (when (< ~var ~nsym)
           ~@body
           (recur (+ ~var 1)))))))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [[& for-declrs] & body]
  (let [for-declrs
        (reverse (partition 2 for-declrs))]
    (letfn [(loop-form
              [bindings coll body]
              (let [coll-name (gensym 'coll)]
                (list
                 `(let* ~coll-name ~coll)

                 (let [[kname vname] (if (vector? bindings)
                                       bindings
                                       [(gensym 'k) bindings])]
                   `(dokeys [~kname ~coll-name]
                            (let* ~vname (get ~coll-name ~kname))
                            ~@body)))))
            (modifier-form
              [modifier-type modifier-expr body]
              (cond
               (= :while modifier-type)
               (list `(if ~modifier-expr
                        (do ~@body)
                       break))
               (= :when modifier-type)
               (list `(when ~modifier-expr
                        ~@body))
               (= :let modifier-type)
               (cons `(let* ~@modifier-expr) body)
               :else
               (throw
                (Exception. "Unsupported modifier form passed to `doseq`"))))]
      (loop [for-declrs for-declrs
             body body]
        (if-let [binding-pair (first for-declrs)]
          (recur (rest for-declrs)
                 (cond
                  (or (symbol? (first binding-pair))
                      (vector? (first binding-pair)))
                  (apply loop-form  `(~@binding-pair ~body))
                  (keyword? (first binding-pair))
                  (apply modifier-form `(~@binding-pair ~body))
                  :else
                  (throw (Exception. "Invalid binding form passed to `doseq`"))
                  ))
          `((fn [] ~@body)))))))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a (NOT LAZY!) vector of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test."
  [[& for-declrs] body]
  (let [for-declrs
        (reverse (partition 2 for-declrs))
        return-name (gensym 'ret)]
    (letfn [(loop-form
              [bindings coll body]
              (let [coll-name (gensym 'coll)]
                (list
                 `(let* ~coll-name ~coll)

                 (let [[kname vname] (if (vector? bindings)
                                       bindings
                                       [(gensym 'k) bindings])]
                   `(dokeys [~kname ~coll-name]
                            (let* ~vname (get ~coll-name ~kname))
                            ~@body)))))
            (modifier-form
              [modifier-type modifier-expr body]
              (cond
               (= :while modifier-type)
               (list `(if ~modifier-expr
                        (do ~@body)
                       break))
               (= :when modifier-type)
               (list `(when ~modifier-expr
                        ~@body))
               (= :let modifier-type)
               (cons `(let* ~@modifier-expr) body)
               :else
               (throw
                (Exception. "Unsupported modifier form passed to `for`"))))]
      (loop [for-declrs for-declrs
             body (list `(. ~return-name push ~body))]
        (if-let [binding-pair (first for-declrs)]
          (recur (rest for-declrs)
                 (cond
                  (or (symbol? (first binding-pair))
                      (vector? (first binding-pair)))
                  (apply loop-form  `(~@binding-pair ~body))
                  (keyword? (first binding-pair))
                  (apply modifier-form `(~@binding-pair ~body))
                  :else
                  (throw (Exception. "Invalid binding form passed to `for`"))
                  ))
          `(let [~return-name []]
             ~@body
             ~return-name))))))

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
           (if (fn? (get ~fname :default-method))
             (let [default-method# (-> ~fname :default-method)]
               (apply default-method# args))
             (throw
              (str "No method in multimethod '" ~(name fname)
                   "' for dispatch value: " dispatched-val#))))))
     (set! (get ~fname :methods) {})))

(defmacro defmethod [fname dispatch-val & fdeclr]
  (let [setee (if (= :default dispatch-val)
                `(get ~fname :default-method)
                `(get (-> ~fname :methods) ~dispatch-val))]
       `(set! ~setee ~(cons 'fn fdeclr))))

(defmacro nth [& args] `(get ~@args))

(defmacro undefined? [sym]
  `(=== "undefined" (typeof ~sym)))

(defmacro require [& args]
  (let [detect-main-symbol
        (fn [require-string]
          (let [main-group
                (-> require-string
                    (clojure.string/split #"[/]")
                    last)
                extracted-symbols
                (-> main-group
                    (clojure.string/split #"[.]")
                    reverse)
                main-symbol
                (symbol
                 (if (and (#{"js" "json" "node"}
                           (first extracted-symbols))
                          (<= 2 (count extracted-symbols)))
                   (second extracted-symbols)
                   (first extracted-symbols)))]
            main-symbol))]
    (cond
     (and (= 1 (count args)) (not (coll? (first args))))
     (let [requiree (first args)]
       (cond
        (string? requiree)
        `(def ~(detect-main-symbol requiree)
           (require* ~requiree))
        (symbol? requiree)
        `(def ~requiree (require* ~(str requiree)))
        :default
        (throw (Exception. "Invalid syntax for require"))))
     (every? coll? args)
     (let [final-forms
           (->> args
                (mapcat
                 (fn [[requiree & option-vals]]
                   (let [options (apply hash-map option-vals)
                         main-symbol (or (:as options)
                                         (detect-main-symbol requiree))
                         refer (:refer options)]
                     (if (< 1 (count refer))
                       `[(def ~main-symbol (require* ~requiree))
                         ~@(for [sym refer]
                             `(def ~sym (get ~main-symbol ~sym)))]
                       [`(def ~main-symbol (require* ~requiree))])))))]
       (if (= 1 (count final-forms))
         (first final-forms)
         `(do ~@final-forms)))
     :default
     (throw (Exception. "Invalid syntax for require")))))
