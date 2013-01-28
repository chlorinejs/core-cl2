(fn not [x] (! x))
(fn contains? [m k]
  (in k m))
(defmacro isa? [a t]
  `(instanceof ~a ~(symbol t)))
(fn true? [expr] (=== true expr))
(fn false? [expr] (=== false expr))
(fn undefined? [expr] (=== undefined expr))
(fn nil? [expr] (=== nil expr))

(fn first [x] (get x 0))
(fn second [x] (get x 1))
(fn third [x] (get x 2))
(fn last [x] (get x (- (count x) 1)))
(fn next [x] (if (empty? x) nil (if (< 1 (count x)) (.slice x 1))))
(fn rest [x] (if (nil? x) [] (.slice x 1)))
(fn nnext [x] (next (next x)))
(fn array? [a] (isa? a "Array"))
(fn string? [s] (=== "string" (typeof s)))
(fn number? [n] (=== "number" (typeof n)))
(fn boolean? [b] (=== "boolean" (typeof b)))
(fn fn? [f] (=== "function" (typeof f)))
(fn regexp? [re] (isa? re "RegExp"))

(fn inc [arg] (+ 1 arg))
(fn dec [arg] (- arg 1))

(fn count [x] (get x :length))
(fn empty? [s] (or (undefined? s) (nil? s) (= {} s) (= [] s)))
(fn not-empty? [s] (not (empty? s)))

(fn reduce* [f val coll]
  (loop [i 0
         r val]
    (if (< i (count coll))
      (recur (+ i 1) (f r (get coll i)))
      r)))
(defn* reduce
  ([f val coll]
    (reduce* f val coll))
  ([f coll]
    (reduce* f (first coll) coll)))

(fn reductions* [f val coll]
  (def ret [])
  (loop [i 0
         r val]
    (if (< i (count coll))
      (recur (+ i 1) (f (do (.push ret r) r)
                        (get coll i)))
      (.push ret r)))
  ret)
(defn* reductions
  ([f val coll]
    (reductions* f val coll))
  ([f coll]
    (reductions* f (first coll) coll)))

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
        (nil?       x) "nil"
        (undefined? x) 'undefined
        (boolean?   x) 'boolean
        (fn?        x) 'function
        (regexp?    x) 'regexp
        :else          'map))

(fn map* [fun arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (.push r (fun (get arr i)))
        (recur r (+ i 1)))
      r)))
(fn map [f coll]
  (if (fn? Array.prototype.map)
    (.map coll f)
    (map* f coll)))
(fn remove [pred seq]
  (loop [r []
         i 0]
    (if (< i (count seq))
      (do
        (when-not (pred (get seq i))
          (.push r (get seq i)))
        (recur r (+ 1 i)))
      r)))

(fn filter* [pred arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (if (pred (get arr i)) (.push r (get arr i)))
        (recur r (+ i 1)))
      r)))
(fn filter [pred coll]
  (if (fn? Array.prototype.filter)
    (.filter coll pred)
    (filter* pred coll)))
(fn merge
  "Merge the contents of map `m2' into map `m1' and return a new map."
  [& ms]
  (or (let [ret {}]
        (for [m ms]
          (for [[k v] m]
            (set! (get ret k) v)))
        ret)
      {}))

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

(fn =* [x y]
  (if (=== x y)
    true
    (if (=== (type x) (type y))
      (cond
       (array? x)
       (if (=== (count x) (count y))
         (loop [a x b y c (count x)]
           (if (=== 0 c) ;;empty arrays
             true
             (if (=* (first a) (first b))
               (recur (rest a) (rest b) (dec c))
               false)))
         false)

       (map? x)
       (let [xkeys (.sort (keys x))]
         (if (=* xkeys (.sort (keys y)))
           ;;keys-equal
           (loop [ks xkeys c (count xkeys)]
             (if (=* (get x (first ks))
                     (get y (first ks)))
               (if (=== 0 c)
                 true
                 (recur (rest ks) (dec c)))
               false))
           false))

       :default
       false)
      ;; not same type
      false)))

(defn* =
  ([]    true)
  ([x]   true)
  ([x y] (=* x y))
  ([a b & c]
     (loop [x a y b more c]
       (if (=* x y)
         (if (next more)
           (recur y (first more) (next more))
           (=* y (first more)))
         false))))
(fn + [& args]
  (reduce (fn [x y] (+ x y)) 0 args))
(fn - [& args]
  (reduce (fn [x y] (- x y)) 0 args))
(fn * [& args]
  (reduce (fn [x y] (* x y)) 1 args))
(fn identity
  [x] x)

(fn reverse [x] (.reverse (.slice x 0)))
(fn compare [x y]
  (cond
   (=== x y)
   0
   (> x y)
   1
   (< x y)
   -1))
(fn zero? [x] (=== 0 x))

(fn int [x]
  (if (number? x) (bit-or x 0)))

(def max Math.max)
(def min Math.min)
(fn pos? [x]
  (and (number? x) (> x 0)))
(fn neg? [x]
  (and (number? x) (< x 0)))
(defn* rand
  ([]  (Math.random))
  ([n] (* n (Math.random))))
(fn quot [x y]
  (int (/ x y)))
(fn integer? [n]
  (=== n (int n)))
(fn even? [n]
  (= 0 (rem n 2)))
(fn odd? [n]
  (= 1 (rem n 2)))
(fn complement [f]
  (fn [& args] (not (apply f args))))
(fn constantly [x]
  (fn [] x))
(fn peek [x]
  (if (nil? x) nil (get x 0)))
(fn pop [x]
  (if (nil? x) nil (.slice x 1)))
(fn conj [coll & xs]
  (if (empty? coll)
    xs
    (let [ret (.slice coll 0)]
      (.concat ret xs))))
(fn cons [x coll]
  (.concat [x] coll))
(fn assoc [m k v]
  (let [ret (merge m {})]
    (set! (get ret k) v)
    ret))
(fn dissoc [m & ks]
  (let [ret (merge m {})]
    (for [k ks]
      (delete (get ret k)))
    ret))
(fn find [m k]
  (if (contains? m k)
    [k (get m k)]
    nil))
(fn every?
  [pred coll]
  (cond
   (empty? coll) true
   (pred (first coll)) (every? pred (next coll))
   :else false))
(fn some
  [pred coll]
  (when coll
    (or (pred (first coll))
        (some pred (next coll)))))

(defn* concat
  ([] [])
  ([x] [x])
  ([x y] (.concat x y))
  ([x & xs] (concat x (apply concat xs))))

(fn mapcat
  [f coll]
  (apply concat (map f coll)))
(fn drop
  [n coll]
  (when (pos? n)
    (when-let [s coll]
      (.slice s n))))
(fn take
  [n coll]
  (when (pos? n)
    (when-let [s coll]
      (.slice s 0 n))))
(fn set [& ks]
  (def ret {})
  (for [k ks]
    (set! (get ret k) true))
  ret)
(defn* sort
  ([coll]
     (.sort (Array.prototype.slice.call coll 0)))
  ([comp x]
     (.sort (Array.prototype.slice.call x 0) comp)))
(fn take-while [pred coll]
  (when-let [s coll]
    (when (pred (first s))
      (conj (take-while pred (rest s)) (first s)))))

(fn drop-last [n coll]
  (.slice coll 0 (- coll.length n)))

(fn take-last [n coll]
  (.slice coll (- coll.length n)))

(fn drop-while [pred coll]
  [pred coll]
  (let [step (fn [pred coll]
               (let [s coll]
                 (if (and s (pred (first s)))
                   (step pred (rest s))
                   s)))]
    (step pred coll)))
(fn cycle [coll n]
  (loop [ret [] n n]
    (if (zero? n)
      ret
      (recur (.concat ret coll) (dec n)))))
(fn split-at [n coll]
  [(take n coll) (drop n coll)])
(fn repeat [n x]
  (loop [ret [] n n]
    (if (zero? n)
      ret
      (recur (conj ret x) (dec n)))))
(fn iterate [f x n]
  (def ret [])
  (cons x (loop [v x i (dec n)]
            (if (zero? i)
              ret
              (recur (let [val (f v)] (ret.push val) val)
                     (dec i))))))
(fn split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(fn zipmap
  [keys vals]
  (def map {})
  (loop [ks keys
         vs vals]
    (if (and ks vs)
      (do (set! (get map (first ks)) (first vs))
          (recur (next ks)
                 (next vs)))
      map)))

(fn nthnext
  [coll n]
  (loop [n n xs coll]
    (if (and xs (pos? n))
      (recur (dec n) (next xs))
      xs)))
(fn nthrest
  [coll n]
  (loop [n n xs coll]
    (if (and xs (pos? n))
      (recur (dec n) (rest xs))
      xs)))
(fn rand-int [n] (int (rand n)))
(fn rand-nth
  [coll]
  (nth coll (rand-int (count coll))))
(fn range*
  [start end step]
  (let [ret []
        comp (if (pos? step) #(< %1 %2) #(> %1 %2))]
    (loop [i start]
      (if (comp i end)
        (do
          (.push ret i)
          (recur (+ i step)))
        (if (comp i end)
          (cons ret (range* i end step))
          ret)))))
(defn* range
  ([end] (range* 0 end 1))
  ([start end] (range* start end 1))
  ([start end step] (range* start end step)))
(fn partition
  [n step coll]
  (when-let [s coll]
    (when-let [p (take n s)]
      (if (= n (count p))
        (cons p (partition n step (nthrest s step)))
        []))))
