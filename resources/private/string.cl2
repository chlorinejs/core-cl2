(defmacro join [sep seq] `(.join ~seq ~sep))
(fn join [sep seq] (.join seq sep))
