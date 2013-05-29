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

(deftest require-tests
  (is (= (macroexpand
          (require "./bar.foo.js"))
         (macroexpand
          (def foo (require* "./bar.foo.js")))))
  (is (= (macroexpand
          (require "js"))
         (macroexpand
          (def js (require* "js")))))
  (is (= (macroexpand
          (require ["./foo.js" :as bar]))
         (macroexpand
          (def bar (require* "./foo.js")))))
  (is (= (macroexpand
          (require ["./foo.js"]))
         (macroexpand
          (def foo (require* "./foo.js")))))
  (is (= (macroexpand
          (require ["./foo.js" :refer [func1 func2]]))
         (macroexpand
          (do (def foo (require* "./foo.js"))
              (def func1 (get foo func1))
              (def func2 (get foo func2)))))))
