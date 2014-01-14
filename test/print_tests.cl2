(deftest pr-str-tests
  (is (= "\"a\""
         (pr-str 'a)))
  (is (= "nil"
         (pr-str nil)))
  (is (= "{\"a\" 1, \"b\" 2}"
         (pr-str {:a 1 :b 2})))
  (is (= "[1 2 \"a\" 3]"
         (pr-str [1 2 :a 3])))
  ;; set keys are coerced to strings
  (is (= "#{\"1\" \"2\" \"3\" \"a\" \"b\"}"
         (pr-str #{1 2 :a 3 'b})))
  )
