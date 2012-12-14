(defn true? [expr] (=== true expr))
(defn false? [expr] (=== false expr))
(defn undefined? [expr] (=== undefined expr))
(defn nil? [expr] (=== nil expr))

(defn first [x] (get x 0))
(defn second [x] (get x 1))
(defn third [x] (get x 2))
(defn last [x] (get x (- (count x) 1)))

(defn array? [a] (isa? a "Array"))
(defn string? [s] (=== "string" (typeof s)))
(defn number? [n] (=== "number" (typeof n)))
(defn boolean? [b] (=== "boolean" (typeof b)))
(defn fn? [f] (== "function" (typeof f)))

(defn str [& args] (.join args ""))
(defn inc [arg] (+ 1 arg))
(defn dec [arg] (- arg 1))
