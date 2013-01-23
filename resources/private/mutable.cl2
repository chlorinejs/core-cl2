(defmacro inc! [arg] `(set! ~arg (+ 1 ~arg)))
(defmacro dec! [arg] `(set! ~arg (- ~arg 1)))
(fn assoc! [m k v]
  (set! (get m k) v)
  m)
(fn dissoc! [m & ks]
  (for [k ks]
    (delete (get m k)))
  m)
(fn reverse! [x] (.reverse x))
