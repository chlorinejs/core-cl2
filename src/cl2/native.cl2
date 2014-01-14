;; Array.prototype.map passes not only one but three arguments
;; to f. Wrapping f with an anonymous function to ignore the
;; rest arguments

(defn reduce'
  [f val coll]
  (.reduce coll
           (fn [x y] (f x y))
           val))

(defn map
  "Creates a new vector with the results of calling a
provided function  on every element in this vector."
  [f coll]
  (.map coll (fn [x] (f x))))

(defn filter
  "Returns a vector of the items in coll for which
  (pred item) returns true. pred must be free of side-effects.
  wrapping pred with an anonymous function to ignore
  unwanted (index and coll) arguments passed by
  native filter."
  [pred coll]
  (.filter coll (fn [x] (pred x))))

(defn every?
  "Returns true if (pred x) is logical true for every x in
coll, else false."
  [pred coll]
  (.every coll (fn [x] (pred x))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil. One common idiom is to use a set as pred, for example this
  will return :fred if :fred is in the sequence, otherwise nil: (some
  #{:fred} coll) wrapping pred with an anonymous function to ignore
  unwanted (index and coll) arguments passed by native filter."
  [pred coll]
  (def ret)
  (. coll some #(do (set! ret (pred %)) ret))
  ret)
