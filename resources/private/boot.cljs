(defmacro apply [fun & args] `(.apply ~fun ~fun ~@args))
(defmacro not [expr] `(! ~expr))
(defn count [x] (get x :length))
(defn empty? [s] (or (undefined? s) (nil? s) (= 0 (count s))))
(defn not-empty? [s] (not (empty? s)))

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

(defn +' []
  (reduce (fn [x y] (+ x y)) 0 arguments))

(defn -' []
  (reduce (fn [x y] (- x y)) 0 arguments))

(defn *' []
  (reduce (fn [x y] (* x y)) 1 arguments))

(defmacro lvar [& bindings]
  `(inline
    ~(str "var "
          (clojure.string/join ","
            (map (fn [[vname vval]]
                   (str vname " = " (clojurejs.js/emit-str vval)))
                 (partition 2 bindings))))))

(defmacro doseq [[var seq] & body]
  (let [seqsym (gensym)]
    `(do
       (lvar ~seqsym ~seq)
       (loop [i# 0]
         (when (< i# (count ~seqsym))
           (let [~var (get ~seqsym i#)]
             ~@body)
           (recur (+ i# 1)))))))

(defmacro dotimes [[var n] & body]
  (let [nsym (gensym)]
    `(do
       (lvar ~nsym ~n)
       (loop [~var 0]
         (when (< ~var ~nsym)
           ~@body
           (recur (+ ~var 1)))))))

(defn reduce [f val coll]
  (loop [i 0
         r val]
    (if (< i (count coll))
      (recur (+ i 1) (f r (get coll i)))
      r)))

(def *gensym* 999)
(defn gensym []
  (inc! *gensym*)
  (str "G__" *gensym*))

(defn subvec [a s e]
  (let [e (or e (count a))
        r (new Array)]
    (loop [i (or s 0)]
      (if (< i e)
        (do
          (.push r (get a i))
          (recur (+ i 1)))
        r))))

(defn map? [m]
  (and m
       (not (or (contains? #{:string :number :boolean} (typeof m))
                (array? m)))))

(defn map [fun arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (.push r (fun (get arr i)))
        (recur r (+ i 1)))
      r)))

(defn remove [pred seq]
  (loop [r []
         i 0]
    (if (< i (count seq))
      (do
        (when-not (pred (get seq i))
          (.push r (get seq i)))
        (recur r (+ 1 i)))
      r)))

(defn filter [pred arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (if (pred (get arr i)) (.push r (get arr i)))
        (recur r (+ i 1)))
      r)))

(defn merge
  "Merge the contents of map `m2' into map `m1' and return a new map."
  [m1 m2]
  (or (and m2
           (let [m {}]
             (dokeys [k m1]
                     (if (.hasOwnProperty m1 k)
                       (set! (get m k) (get m1 k))))
             (dokeys [k m2]
                     (if (.hasOwnProperty m2 k)
                       (set! (get m k) (get m2 k))))
             m))
      m1))

(defn select-keys [m ks]
  (let [m1 {}]
    (doseq [k ks]
      (if (.hasOwnProperty m k)
        (set! (get m1 k) (get m k))))
    m1))

(defn keys [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v k)))
    v))

(defn vals [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v (get m k))))
    v))
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
              (with-meta `(~(first form) ~x ~@(next form)) (meta form))
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

(defmacro ->>
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
              (list form x)))
  ([x form & more] `(->> (->> ~x ~form) ~@more)))
