(defmacro doseq [[var seq] & body]
  (let [seqsym (gensym)]
    `(do
       (lvar ~seqsym ~seq)
       (loop [i# 0]
         (when (< i# (count ~seqsym))
           (let [~var (get ~seqsym i#)]
             ~@body)
           (recur (+ i# 1)))))))
(fn count [x] (get x :length))
(fn empty? [s] (or (undefined? s) (nil? s) (= 0 (count s))))
(fn not-empty? [s] (not (empty? s)))
(fn +' []
  (reduce (fn [x y] (+ x y)) 0 arguments))
(fn -' []
  (reduce (fn [x y] (- x y)) 0 arguments))
(fn *' []
  (reduce (fn [x y] (* x y)) 1 arguments))

(fn reduce [f val coll]
  (loop [i 0
         r val]
    (if (< i (count coll))
      (recur (+ i 1) (f r (get coll i)))
      r)))

(def *gensym* 999)
(fn gensym []
  (inc! *gensym*)
  (str "G__" *gensym*))

(fn subvec [a s e]
  (let [e (or e (count a))
        r (new Array)]
    (loop [i (or s 0)]
      (if (< i e)
        (do
          (.push r (get a i))
          (recur (+ i 1)))
        r))))

(fn map? [m]
  (and m
       (not (or (contains? #{'string 'number 'boolean 'function} (typeof m))
                (array? m)
                (nil? m)
                (undefined? m)
                (regexp? m)))))

(fn type [x]
  (cond (array?     x) 'array
        (string?    x) 'string
        (number?    x) 'number
        (nil?       x) 'nil
        (undefined? x) 'undefined
        (boolean?   x) 'boolean
        (fn?        x) 'function
        (regexp?    x) 'regexp
        :else          'map))

(fn map [fun arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (.push r (fun (get arr i)))
        (recur r (+ i 1)))
      r)))

(fn remove [pred seq]
  (loop [r []
         i 0]
    (if (< i (count seq))
      (do
        (when-not (pred (get seq i))
          (.push r (get seq i)))
        (recur r (+ 1 i)))
      r)))

(fn filter [pred arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (if (pred (get arr i)) (.push r (get arr i)))
        (recur r (+ i 1)))
      r)))

(fn merge
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

(fn select-keys [m ks]
  (let [m1 {}]
    (doseq [k ks]
      (if (.hasOwnProperty m k)
        (set! (get m1 k) (get m k))))
    m1))

(fn keys [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v k)))
    v))

(fn vals [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v (get m k))))
    v))
