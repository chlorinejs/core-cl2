(deftest core-macros
  (is (= (macroexpand
          (.. foo (bar 1 2) (baz 3 4)))
         (macroexpand
          (. (. foo (bar 1 2)) (baz 3 4)))))

  (is (= (macroexpand
          (-> foo bar baz))
         (macroexpand
          (baz (-> foo bar)))))

  (is (= (macroexpand
          (-> foo (bar 1 2) (baz 3 4)))
         (macroexpand
          (baz (-> foo (bar 1 2)) 3 4))))

  (is (= (macroexpand
          (->> foo (bar 1 2) (baz 3 4)))
         (macroexpand
          (baz 3 4 (->> foo (bar 1 2)))))))
