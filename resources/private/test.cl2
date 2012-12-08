(fn is [expr msg]
  (if (not expr)
    (throw (new Error msg))))

(defmacro deftest [test-name & tests]
  `(test
    ~(name test-name)
    (fn [] ~@tests)))
