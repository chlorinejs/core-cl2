(deftest dotimes-tests
  (is (=== 100
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

(def a 2)
(inc! a)
(def b 5)
(dec! b)

(deftest inc!-dec!-tests
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
  (is (=== (re-test #"/e/" "e")
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

(deftest for-tests
  (def x 0)
  (for [n [1 2 3]]
        (set! x (+ n x)))
  (is (=== x 6))

  (def x "")
  (def y 0)
  (for [[k v] {:a 1 :b 2}]
        (set! x (+* x k))
        (set! y (+* y v)))
  (is (=== "ab" x))
  (is (=== 3 y)))

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
