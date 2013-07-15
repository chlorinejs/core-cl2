(load-file "./native.cl2")

;;  Non-native implementation of reduce, map, every?, some, filter

(defn reduce*
  [f val coll]
  (let [c (count coll)]
    (loop [i 0
           r val]
      (if (< i c)
        (recur (+ i 1) (f r (get coll i)))
        r))))

(defn map*
  [f arr]
  (let [c (count arr)]
    (loop [r []
           i 0]
      (if (< i c)
        (do
          (.push r (f (get arr i)))
          (recur r (+ i 1)))
        r))))

(defn filter*
  [pred arr]
  (let [c (count arr)]
    (loop [r []
           i 0]
      (if (< i c)
        (do
          (if (pred (get arr i)) (.push r (get arr i)))
          (recur r (+ i 1)))
        r))))

(defn every?*
  [pred coll]
  (cond
   (empty? coll) true
   (pred (first coll)) (every?* pred (next coll))
   :else false))


(defn some*
  [pred coll]
  (when coll
    (or (pred (first coll))
        (some* pred (next coll)))))

;; Checks if native implementation of map, reduce, filter, some, every?
;; (as defined in native.cl2) can work. If not, replace them with
;; non-native ones.

(when-not (fn? Array.prototype.every)
  (set! every? every?*))

(when-not (fn? Array.prototype.some)
  (set! some some*))

(when-not (fn? Array.prototype.reduce)
  (set! reduce' reduce*))

(when-not (fn? Array.prototype.map)
  (set! map map*))

(when-not (fn? Array.prototype.filter)
  (set! filter filter*))
