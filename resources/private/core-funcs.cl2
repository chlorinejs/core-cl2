(fn true? [expr] (=== true expr))
(fn false? [expr] (=== false expr))
(fn undefined? [expr] (=== undefined expr))
(fn nil? [expr] (=== nil expr))

(fn first [x] (get x 0))
(fn second [x] (get x 1))
(fn third [x] (get x 2))
(fn last [x] (get x (- (count x) 1)))
(fn next [x] (if (< 1 (count x)) (.slice x 1)))
(fn rest [x] (if (nil? x) [] (.slice x 1)))

(fn array? [a] (isa? a "Array"))
(fn string? [s] (=== "string" (typeof s)))
(fn number? [n] (=== "number" (typeof n)))
(fn boolean? [b] (=== "boolean" (typeof b)))
(fn fn? [f] (=== "function" (typeof f)))
(fn regexp? [re] (isa? re "RegExp"))

(fn inc [arg] (+ 1 arg))
(fn dec [arg] (- arg 1))
