;; `for` macro tests stolen from clojuredocs.org
;;
;; use Clojure reader macro `#=` to evaluate the same code
;; in **Clojure**.

(deftest for-tests
  (is (= (for [x [0 1 2 3 4 5]
               :let [y (* x 3)]
               :when (even? y)]
           y)
         #=(vec
            #=(eval
               (for [x [0 1 2 3 4 5]
                     :let [y (* x 3)]
                     :when (even? y)]
                 y)))))
  (is (= (for [x (range 1 6)
               :let [y (* x x)
                     z (* x x x)]]
           [x y z])
         #= (vec
             #= (eval
                 (for [x (range 1 6)
                       :let [y (* x x)
                             z (* x x x)]]
                   [x y z])))))
  (is (= (for [[x y] {:a 1 :b 2 :c 0}
               :when (= y 0)] x)
         #= (vec
             #= (eval
                 (for [[x y] {:a 1 :b 2 :c 0}
                       :when (= y 0)] x)))))
  (is (= (for [x (range 3) y (range 3)
               :when (not= x y)] [x y])
         #= (vec
             #= (eval
                 (for [x (range 3) y (range 3)
                       :when (not= x y)] [x y])))))
  (is (= (for [x (range 3) y (range 3)
               :while (not= x y)] [x y])
         #= (vec
             #= (eval
                 (for [x (range 3) y (range 3)
                       :while (not= x y)] [x y])))))

  (is (= (for [x (range 10) :while (< x 10)
               y (range 10) :while (<= y x)]
           [x y])
         #= (vec
             #= (eval
                 (for [x (range 10) :while (< x 10)
                       y (range 10) :while (<= y x)]
                   [x y])))))
  (is (= (for [x [1 2 3]
               y [1 2 3]
               :while (<= x y)
               z [1 2 3]]
           [x y z])
         #= (vec
             #= (eval
                 (for [x [1 2 3]
                       y [1 2 3]
                       :while (<= x y)
                       z [1 2 3]]
                   [x y z])))))
  (is (= (for [x [1 2 3]
               y [1 2 3]
               z [1 2 3]
               :while (<= x y)]
           [x y z])
         #= (vec
             #= (eval
                 (for [x [1 2 3]
                       y [1 2 3]
                       z [1 2 3]
                       :while (<= x y)]
                   [x y z]))))))