(deftest apply-tests
  (is (= (apply (fn* [x y] (+ x y)) [1 2])
         3)))

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

(deftest inc!-dec!-tests
  (def a 2)
  (inc! a)
  (def b 5)
  (dec! b)
  (is (=== 3 a))
  (is (=== 4 b)))

(deftest dot-form-tests
  (is (=== "c;b;a"
         (.. [:c :b :a]
             (join ";"))))
  (is (=== "a,b,c"
         (.. [:c :b :a]
             (sort)
             (join ",")))))

(deftest regexp-tests
  (is (=== (.test #"/e/" "e")
         true))
  (is (=== (.. #"/e/" (test "E"))
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

(deftest as-fn-tests
  (is (=== (apply (as-fn :foo) [{:foo 'bar}])
           "bar"))
  (is (apply (as-fn #{:a :b :c}) [:a]))
  (is (=== false (apply (as-fn #{:a :b :c}) [:d]))))

(deftest condp-tests
  (is (=== (condp #(> %1 %2) 3
             1 :one
             2 :two
             3 :three
             4 :four)
           :four)))