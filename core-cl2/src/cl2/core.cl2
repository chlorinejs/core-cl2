(load-file "./compatible.cl2")
;;(load-file "./native.cl2")

(defn not [x] (! x))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false. Note that for numerically indexed collections like
  vectors, this tests if the numeric key is within the range of indexes;
  for maps and sets, this tests if key is within the keys. Also, keys must
  be of javascript primitive types.
  See also 'some'."
  [m k]
  (in k m))

(defn boolean
  "Coerces to boolean"
  [x]
  (and (!= null x)
       (!== false x)))

(defn get'
  "Returns the value mapped to key, not-found or nil if key not present."
  [m k not-found]
  (let [look-up (if (string? m)
                  (seq m)
                  m)]
    (if (contains? look-up k)
      (get m k)
      not-found)))

(alias get get')

(defn true? [expr] (=== true expr))

(defn false? [expr] (=== false expr))

(defn nil? [expr] (or (=== nil expr)  (=== null expr)))

(defn first
  "Returns the first item in the collection. Doesn't work on maps.
  If coll is nil, returns nil."
  [x]
  (if (nil? x) nil (get x 0)))

(defn second
  "Returns the second item in the collection. Doesn't work on maps.
  If coll is nil, returns nil."
  [x]
  (if (nil? x) nil (get x 1)))

(defn last
  "Returns the last item in the collection. Doesn't work on maps.
  If coll is nil, returns nil."
  [x]
  (if (nil? x) nil (get x (dec (count x)))))

(defn next
  "Returns a vector of the items after the first. Doesn't work on maps.
  If there are no more items, returns nil."
  [x]
  (if (empty? x) nil (if (< 1 (count x)) (.slice x 1))))

(defn rest
  "Returns a possibly empty vector of the items after the first.
  Doesn't work on maps."
  [x]
  (if (nil? x) [] (.slice x 1)))

(defn nnext
  "Same as (next (next x))"
  [x]
  (next (next x)))

(defn set?
  "Return true if x is a set"
  [x] (isa? x "Cl2Set"))

(defn vector?
  "Return true if x is a Vector."
  [x] (isa? x "Array"))

(defn string?
  "Returns true if x is a String"
  [x]
  (or (=== "string" (typeof x))
      (isa? x "String")))

(defn number?
  "Return true if x is a Number"
  [n]
  (=== "number" (typeof n)))

(defn zero? [x] (=== 0 x))

(defn fn?
  "Return true if x is a Function"
  [f]
  (=== "function" (typeof f)))

(defn inc
  "Returns a number one greater than num."
  [arg]
  (+ 1 arg))

(defn dec
  "Returns a number one less than num."
  [arg]
  (- arg 1))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0."
  [x]
  (if (or (vector? x) (string? x))
    (. x -length)
    (. (keys x) -length)))

(defn empty?
  "Returns true if coll has no items."
  [coll]
  (or (=== coll "") (nil? coll) (= {} coll) (= [] coll) (= #{} coll)))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f val coll]
     (reduce' f val coll))
  ([f coll]
     (reduce' f (first coll) (rest coll))))

(defn reductions*
  "Standard version of reductions"
  [f init coll]
  (let [c (count coll)
        ret []]
    (loop [i 0
           r init]
      (if (< i c)
        (recur (+ i 1) (f (do (.push ret r) r)
                          (get coll i)))
        (.push ret r)))
    ret))

(defn reductions
  "Returns a vector of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  ([f init coll]
     (reductions* f init coll))
  ([f coll]
     (reductions* f (first coll) (rest coll))))

(def *gensym* 999)
(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  [prefix-string]
  (set! *gensym* (inc *gensym*))
  (str (or prefix-string "G__") *gensym*))

(defn subvec
  "Returns a persistent vector of the items in vector from
  start (inclusive) to end (exclusive).  If end is not supplied,
  defaults to (count vector)."
  [v start end]
  (. v slice start end))

(defn map?
  "Returns true if m is a map. Note: all objects of non-primitive types
   are considered maps."
  [m]
  (and m
       (= "object" (typeof m))
       (not (or ;; (contains? #{'string 'number 'boolean 'function} (typeof m))
             ;; null is "object", too
             (vector? m)
             (= null m)
             (isa? m "RegExp")
             (isa? m "String")
             (isa? m "Cl2Set")))))

;; More info about type detection in javascript:
;; http://stackoverflow.com/questions/332422/how-do-i-get-the-name-of-an-objects-type-in-javascript

(defn type
  "Returns the type of x as a string. Returns 'map on non-primitive types.
  Won't try to determine Class name of x."
  [x]
  (cond (vector?     x)  'vector
        (set?        x)  'set
        (string?     x)  'string
        (number?     x)  'number
        (nil?        x)  "nil"
        (=== null    x)  'null
        (=== "boolean"
             (typeof x)) 'boolean
        (fn?         x)  'function
        (isa? x
              "RegExp")  'regexp
        :else            'map))

(defn remove
  "Returns a vector of the items in coll for which
  (pred item) returns false. pred must be free of side-effects."
  [pred seq]
  (let [c (count seq)]
    (loop [r []
           i 0]
      (if (< i c)
        (do
          (when-not (pred (get seq i))
            (.push r (get seq i)))
          (recur r (+ 1 i)))
        r))))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  [& ms]
  (or (let [ret {}]
        (doseq [m ms]
          (doseq [[k v] m]
            (set! (get ret k) v)))
        ret)
      {}))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  [m ks]
  (let [m1 {}]
    (doseq [k ks]
      (if (.hasOwnProperty m k)
        (set! (get m1 k) (get m k))))
    m1))

(defn keys
  "Returns a sequence of the map's keys."
  [m]
  (for [[k v] m] k))

(defn vals
  "Returns a sequence of the map's values."
  [m]
  (for [[k v] m] v))

(defn v=*
  "Checks if two vectors are equal."
  [x y]
  (if (=== (count x) (count y))
    (loop [a x b y c (count x)]
      (if (=== 0 c) ;;empty vectors
        true
        (if (=* (first a) (first b))
          (recur (rest a) (rest b) (dec c))
          false)))
    false))

(defn m=*
  "Checks if two maps are equal."
  [x y]
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
      false)))

(defn =*
  "Equality. Returns true if x equals y, false if not."
  [x y]
  (if (=== x y)
    true
    (if (=== (type x) (type y))
      (cond
       (vector? x)
       (v=* x y)

       (or (map? x)  (set? x))
       (m=* x y)

       :else
       false)
      ;; non comparable types
      false)))

(defn =**
  "Primitive equality. Returns true if all its arguments are equal."
  [x & xs]
  (loop [a x more xs]
    (if (=== x a)
      (if (next more)
        (recur (first more) (next more))
        (=== x (first more)))
      false)))

(defn ='
  "Equality. Returns true if all its arguments are equal."
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

;; Alias of `=` function to use in `=` macro
(def = =')

(defn not=
  "Same as (not (= obj1 obj2))"
  ([x] false)
  ([x y] (not (= x y)))
  ([& args]
     (not (apply = args))))

(defn +
  "Returns the sum of nums. (+) returns 0."
  [& args]
  (reduce #(+ %1 %2) 0 args))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  [& args]
  (reduce #(- %1 %2) 0 args))

(defn *
  "Returns the product of nums. (*) returns 1."
  [& args]
  (reduce #(* %1 %2) 1 args))

(defn identity
  "Returns its argument."
  [x] x)

(defn reverse
  "Returns a seq of the items in coll in reverse order."
  [x]
  (.reverse (.slice x 0)))

(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y."
  [x y]
  (cond
   (=== x y)
   0

   (> x y)
   1

   (< x y)
   -1))

(defn int
  "Coerce to int."
  [x]
  (if (number? x) (bit-or x 0)))

(def ^{:doc "Returns the greatest of the nums. If no number is provided,
  returns `-Infinity`."}
  max Math.max)

(def ^{:doc "Returns the least of the nums. If no number is provided,
  returns `Infinity`."}
  min Math.min)

(defn pos?
  "Returns true if num is greater than zero, else false"
  [x]
  (and (number? x) (> x 0)))

(defn neg?
  "Returns true if num is less than zero, else false"
  [x]
  (and (number? x) (< x 0)))

(defn integer?
  "Returns true if n is an integer"
  [n]
  (=== n (int n)))

(defn even?
  "Returns true if n is even, else false"
  [n]
  (=== 0 (rem n 2)))

(defn odd?
  "Returns true if n is odd, else false"
  [n]
  (=== 1 (rem n 2)))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)"
  ([]  (Math.random))
  ([n] (* n (Math.random))))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  [x y]
  (int (/ x y)))

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  [f]
  (fn [& args] (not (apply f args))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x"
  [x]
  (fn [] x))

(defn conj
  "conj[oin]. Returns a new vector with the xs 'added' to the end."
  [coll & xs]
  (if (empty? coll)
    xs
    (.concat coll xs)))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [x coll]
  (.concat [x] coll))

(defn assoc
  "assoc[iate]. When applied to a map, returns a new map  that contains
  the mapping of the new key to the new val. When applied to a vector,
  returns a new vector that contains val at index.
   Note - index must be <= (count vector)."
  [m & kvs]
  (let [ret (if (vector? m)
               (. m slice 0)
               (merge m {}))]
    (loop [kv-tail kvs]
      (if kv-tail
        (do (let* [k v] kv-tail)
            (set! (get ret k) v)
            (recur (nnext kv-tail)))
        ret))))

(defn dissoc
  "dissoc[iate]. Returns a new map that does not contain a mapping for key(s)."
  [m & ks]
  (let [ret (merge m {})]
    (doseq [k ks]
      (delete (get ret k)))
    ret))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [m k]
  (if (contains? m k)
    [k (get m k)]
    nil))

(defn concat
  "Returns a vector representing the concatenation of the elements in
  the supplied colls."
  [& xs]
  (loop [ret []
         xs-tail xs]
    (if xs-tail
      (recur (. ret concat (get xs-tail 0))
             (next xs-tail))
      ret)))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection."
  [f coll]
  (apply concat (map f coll)))

(defn drop
  "Returns a vector of all but the first n items in coll."
  [n coll]
  (when (pos? n)
    (when-let [s coll]
      (.slice s n))))

(defn take
  "Returns a vector of the first n items in coll, or all items if
  there are fewer than n."
  [n coll]
  (when (pos? n)
    (when-let [s coll]
      (.slice s 0 n))))

(defn Cl2Set
  "Constructor for set data type."
  [coll]
  (let [that this]
    (doseq [k coll]
      (set! (get that k) true))))

(defn hash-set
  "Returns a new hash set with supplied keys."
  [& ks]
  (Cl2Set. ks))

(defn set
  "Returns a set of the distinct elements of coll."
  [coll]
  (Cl2Set. coll))

(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses Javascript native sort."
  ([coll]
     (.sort (Array.prototype.slice.call coll 0)))
  ([comp x]
     (.sort (Array.prototype.slice.call x 0) comp)))

(defn take-while
  "Returns a vector of successive items from coll while
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (when-let [s coll]
    (when (pred (first s))
      (conj (take-while pred (rest s)) (first s)))))

(defn drop-last
  "Return a vector of all but the last n (default 1) items in coll."
  [n coll]
  (.slice coll 0 (- coll.length n)))

(defn take-last
  "Returns a seq of the last n items in coll. See also subvec."
  [n coll]
  (.slice coll (- coll.length n)))

(defn drop-while
  "Returns a vector of the items in coll starting from the first
  item for which (pred item) returns logical false."
  [pred coll]
  (let [step (fn [pred coll]
               (let [s coll]
                 (if (and s (pred (first s)))
                   (step pred (rest s))
                   s)))]
    (step pred coll)))

(defn cycle
  "Returns a vector of n repetitions of the items in coll."
  [coll n]
  (loop [ret [] n n]
    (if (zero? n)
      ret
      (recur (.concat ret coll) (dec n)))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  [n coll]
  [(take n coll) (drop n coll)])

(defn repeat
  "Returns a sequence of xs for n times."
  [n x]
  (loop [ret [] n n]
    (if (zero? n)
      ret
      (recur (conj ret x) (dec n)))))

(defn iterate
  "Returns a vector of x, (f x), (f (f x)) etc upto n times.
  f must be free of side-effects"
  [f x n]
  (def ret [])
  (cons x (loop [v x i (dec n)]
            (if (zero? i)
              ret
              (recur (let [val (f v)] (ret.push val) val)
                     (dec i))))))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  [keys vals]
  (def map {})
  (loop [ks keys
         vs vals]
    (if (and ks vs)
      (do (set! (get map (first ks)) (first vs))
          (recur (next ks)
                 (next vs)))
      map)))

(defn nthnext
  "Returns the nth next of coll, coll when n is 0."
  [coll n]
  (loop [n n xs coll]
    (if (and xs (pos? n))
      (recur (dec n) (next xs))
      xs)))

(defn nthrest
  "Returns the nth rest of coll, coll when n is 0."
  [coll n]
  (loop [n n xs coll]
    (if (and xs (pos? n))
      (recur (dec n) (rest xs))
      xs)))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the vector."
  [coll]
  (nth coll (rand-int (count coll))))

(defn range*
  "Standard version of range."
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
(defn range
  "Returns a vector of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end
  to infinity."
  ([end] (range* 0 end 1))
  ([start end] (range* start end 1))
  ([start end step] (range* start end step)))

(defn partition3
  "Three-argument version of partition"
  [n step coll]
  (when-let [s coll]
    (when-let [p (take n s)]
      (if (= n (count p))
        (cons p (partition3 n step (nthrest s step)))
        []))))

(defn partition4
  "Four-argument version of partition"
  [n step pad coll]
  (when-let [s coll]
    (when-let [p (take n s)]
      (if (= n (count p))
        (cons p (partition4 n step pad (nthrest s step)))
        [(take n (concat p pad))]))))

(defn partition
  "Returns a vector of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  ([n coll] (partion3 n n coll))
  ([n step coll]
     (partition3 n step coll))
  ([n step pad coll]
     (partition4 n step pad coll)))

(alias ^{:doc "Returns the substring of s beginning at start inclusive,
 and ending at end (defaults to length of string), exclusive."}
  subs subvec)

(def println (if (=== "object" (typeof console))
               console.log
               (fn [])))

(defn seq
  "Returns a seq on the collection. If the collection is
empty, returns nil. (seq nil) returns nil."
  [x]
  (cond
   (empty? x)
   nil

   (vector? x)
   x

   (string? x)
   (.split x "")

   (set? x)
   (keys x)

   :else ;;map?
   (for [[k v] x]
     [k v])))

(defn interleave
  [& colls]
  (let [max-index (apply min (map count colls))]
    (apply concat
           (for [index (range 0 max-index)]
             (map #(nth % index) colls)))))

(defn interpose
  [sep coll]
  (let [c (seq coll)]
    (drop 1 (interleave (repeat (count c) sep) c))))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks] v]
  (if (count ks)
    (assoc m k (assoc-in (or (get m k) {}) ks v))
    (assoc m k v)))

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (loop [ret (f)] (if (fn? ret) (recur (ret)) ret)))
  ([f & args]
     (trampoline #(apply f args))))
