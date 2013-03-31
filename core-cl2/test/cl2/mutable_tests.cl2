(deftest reverse!-tests
  (def a [1 2 3 4 5])
  (is (= (reverse! a) [5 4 3 2 1]))
  (is (= a [5 4 3 2 1])))

(deftest assoc!-map-test
  (def my-map {:a 1 :b 2})
  (def new-map (assoc! my-map :c 3))
  (is (= new-map {:a 1 :b 2 :c 3}))
  (is (= my-map {:a 1 :b 2 :c 3})))

(deftest assoc!-vector-test
  (def my-vec [0 1 2])
  (def new-vec (assoc! my-vec 1 :x))
  (is (= new-vec [0 :x 2]))
  (is (= my-vec [0 :x 2])))

(deftest dissoc!-tests
  (def my-map {:a 1 :b 2})
  (def new-map (dissoc! my-map :b))
  (is (= new-map {:a 1}))
  (is (= my-map {:a 1}))

  (is (= (dissoc! {:a 1 :b 2} :a :b) {}))
  (is (= (dissoc! {:a 1 :b 2} :a :c) {:b 2}))
  (is (= (dissoc! #{:a 1 :b 2} :a :c) #{1 :b 2})))
