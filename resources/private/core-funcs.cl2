(fn true? [expr] (=== true expr))
(fn false? [expr] (=== false expr))
(fn undefined? [expr] (=== undefined expr))
(fn nil? [expr] (=== nil expr))

(fn first [x] (get x 0))
(fn second [x] (get x 1))
(fn third [x] (get x 2))
(fn last [x] (get x (- (count x) 1)))

(fn array? [a] (isa? a "Array"))
(fn string? [s] (=== "string" (typeof s)))
(fn number? [n] (=== "number" (typeof n)))
(fn boolean? [b] (=== "boolean" (typeof b)))
(fn fn? [f] (=== "function" (typeof f)))
(fn regexp? [re] (isa? re "RegExp"))

(fn map->str [m]
  (def e "")
  (dokeys [k m]
          (def e (+ "" e " :" k " " (to-str (get m k)) ",") ))
  e)

(fn to-str [x]
  (cond
    (array? x)
    (+ "[" (map to-str x) "]")

    (map? x)
    (+ "{" (map->str x) "}")

    (nil? x)
    ""

    :default
    (+ "" x)))

(fn str []
  (.. (map to-str arguments) (join "")))

(fn pr-map [m]
  (def e "")
  (dokeys [k m]
          (def e (+ "" e " :" k " " (pr-str' (get m k)) ",") ))
  e)

(fn pr-str' [x]
  (cond
    (string? x)
    (+ "\"" x "\"")

    (array? x)
    (+ "[" (map pr-str' x) "]")

    (map? x)
    (+ "{" (pr-map x) "}")

    (regexp? x)
    (+ "#" "\"" (.x toString) "\"")

    (= nil x)
    "nil"

    :default
    (+ "" x)))

(fn pr-str []
  (.. (map pr-str' arguments) (join " ")))

(fn prn-str []
  (+ (.. (map pr-str' arguments) (join " ")) "\n"))

(fn inc [arg] (+ 1 arg))
(fn dec [arg] (- arg 1))
