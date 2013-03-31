(defmacro inc! [arg] `(set! ~arg (+* ~arg 1)))

(defmacro dec! [arg] `(set! ~arg (-* ~arg 1)))

(defn assoc!
  "Mutable version of assoc."
  [m & kvs]
  (loop [kv-tail kvs]
    (if kv-tail
      (do (let* [k v] kv-tail)
          (set! (get* m k) v)
          (recur (nnext kv-tail)))
      m)))

(defn dissoc!
  "Mutable version of dissoc."
  [m & ks]
  (doseq [k ks]
    (delete (get* m k)))
  m)
(fn reverse! [x] (.reverse x))
(fn conj! [coll & xs]
  (.apply coll.push coll xs)
  coll)
(fn cons! [coll & xs]
  (.apply coll.unshift coll xs)
  coll)
(fn merge!
  [m0 & ms]
  (for [m ms]
    (for [[k v] m]
      (set! (get m0 k) v)))
  m0)