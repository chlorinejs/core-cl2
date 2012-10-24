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


(defn map->str [m]
  (def e "")
  (dokeys [k m]
          (def e (+ "" e " :" k " " (to-str (get m k)) ",") ))
  e)

(defn to-str [x]
  (cond
    (array? x)
    (+ "[" (map to-str x) "]")

    (map? x)
    (+ "{" (map->str x) "}")

    (= nil x)
    ""

    :default
    (+ "" x)))

(defn str []
  (.. (map to-str arguments) (join "")))

(defn pr-map [m]
  (def e "")
  (dokeys [k m]
          (def e (+ "" e " :" k " " (pr-str' (get m k)) ",") ))
  e)

(defn pr-str' [x]
  (cond
    (string? x)
    (+ "\"" x "\"")

    (array? x)
    (+ "[" (map pr-str' x) "]")

    (map? x)
    (+ "{" (pr-map x) "}")

    (= nil x)
    "nil"

    :default
    (+ "" x)))

(defn pr-str []
  (.. (map pr-str' arguments) (join " ")))

(defn prn-str []
  (+ (.. (map pr-str' arguments) (join " ")) "\n"))

(defn inc [arg] (+ 1 arg))
(defn dec [arg] (- arg 1))
