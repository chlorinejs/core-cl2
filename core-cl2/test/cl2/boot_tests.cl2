(deftest apply-tests
  (is (= (macroexpand (apply (fn* [x y] (+ x y)) [1 2]))
         (macroexpand
          (.apply (fn* [x y] (+ x y)) 0 [1 2]))))
  (is (= (apply* concat [1 2 3 [4 5]])
         [1 2 3 4 5]))
  (is (= (apply* (fn* [x y z t] (+ x y z t)) [1 2 3 4])
         10))

  (is (= (macroexpand (apply (fn* [x y] (+ x y)) [1 2]))
         (macroexpand (.apply (fn* [x y] (+ x y)) 0 [1 2]))))
  (is (= (apply (fn* [x y] (+ x y)) [1 2])
         3))

  (is (= (macroexpand
          (apply (fn* [x y z t] (+ x y z t)) 1 2 [3 4]))
         (macroexpand
          (.apply (fn* [x y z t] (+ x y z t)) 0 [1 2 3 4]))))
  (is (= (apply (fn* [x y z t] (+ x y z t)) 1 2 [3 4])
         10))

  (is (= (macroexpand
          (apply (fn* [x y z t] (+ x y z t)) 1 2 c))
         (macroexpand
          (.apply (fn* [x y z t] (+ x y z t)) 0 (.concat [1 2] c)))))
  (is (= (let [c [3 6]]
           (apply (fn* [x y z t] (+ x y z t)) 1 2 c))
         12)))

(deftest dotimes-tests
  (is (= 100
         (do
           (fn test []
             (let [sum 0]
               (dotimes [i 5]
                 (dotimes [j 5]
                   (set! sum (+ sum (* i j)))))
               sum))
           (test)))))

(deftest contains?-tests
  (is (=== true (contains? [0 1 2] 0 )))
  (is (=== false (contains? [0 1 2] 3 )))
  (is (=== true (contains? {:a 1 :b 2} :a)))
  (is (=== false (contains? {:a 1 :b 2} :c))))

(deftest dot-form-tests
  (is (=== "c;b;a"
         (.. [:c :b :a]
             (join ";"))))
  (is (=== "a,b,c"
         (.. [:c :b :a]
             (sort)
             (join ",")))))

(deftest regexp-tests
  (is (=== (.test #"e" "e")
         true))
  (is (=== (.. #"e" (test "E"))
         false)))

(deftest native-operator-tests
  (is (=== "foobar"
         (+* "f" "oo" "bar")))
  (is (=== 10
         (+* 1 2 3 4)))
  (is (=== -15
         (-* 0 1 2 3 4 5)))
  (is (=== 120
         (** 1 2 3 4 5))))

(deftest doseq-vector-tests
  (def x 0)
  (doseq [n [1 2 3]]
    (set! x (+ n x)))
  (is (=== x 6))

  (def a "")
  (def b 0)
  (doseq [[c d] {:a 1 :b 2}]
    (set! a (+* a c))
    (set! b (+* b d)))
  (is (=== "ab" a))
  (is (=== 3 b)))

(deftest ->-tests
  (is (=== (+* "" (-> [3 4] (.concat 1 2) (.sort)))
           "1,2,3,4")))

(deftest ->>-tests
  (is (=== (+* "" (->> [5 6] (.concat [3 4]) (.concat [1 2])))
           "1,2,3,4,5,6")))

(deftest ..-tests
  (is (=== (+* "" (.. [3 4] (concat 1 2) (sort)))
           "1,2,3,4"))
  (is (=== (+* "" (.. [5 6] (concat [3 4]) (concat [1 2])))
           "5,6,3,4,1,2")))

(deftest keyword-tests
  (is (=== (:a {:a 1 :b 2}) 1)))

(deftest condp-tests
  (is (=== (condp #(> %1 %2) 3
             1 :one
             2 :two
             3 :three
             4 :four)
           :four)))

(deftest cond->-test
  (is (=== 6
           (cond-> 1
                   true inc
                   false (* 42)
                   (= 2 2) (* 3)))))

(deftest cond->>-test
  (is (=== 10
           (let [d [0 1 2 3]]
             (cond->> d
                      true (map inc)
                      (number? d) (map dec)
                      (= (count d) 4) (reduce +)
                      )))))

(deftest doto-test
  (is (= ["four" "three" 2 1]
         (doto [1 2 3]
           (.pop)
           (.push "three" "four")
           (.reverse)))))
