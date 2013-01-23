(defmacro inc-after! [arg] `(inline ~(str arg "++")))
(defmacro dec-after! [arg] `(inline ~(str arg "--")))
