(deftest native-operator-macro-tests
  (is (= "foobar"
         (+* "f" "oo" "bar")))
  (is (= 10
         (+ 1 2 3 4)))
  (is (= -15
         (- 0 1 2 3 4 5)))
  (is (= 120
         (* 1 2 3 4 5))))

(deftest last-next-rest-tests
  (is (= 3 (last [1 2 3])))
  (is (= nil (last [])))
  (is (= nil (last nil)))
  (is (= [2 3] (next [1 2 3])))
  (is (= nil (next [])))
  (is (= [2 3 4]
         (rest [1 2 3 4])))
  (is (= [] (rest [])))
  (is (= (butlast [1 2 3 4])
         [1 2 3]))
  (is (= nil (butlast []))))

(deftest type-tests
  (is (vector? [1 2 3]))
  (is (= false
         ((fn [] (vector? arguments)))))
  (is (= false (vector? {:a 1 :b 2})))
  (is (string? "abc"))
  (is (string? :def))
  (is (string? 'xyz))
  (is (string? (String. "foo")))

  (is (number? 123))
  (is (number? -456))
  (is (number? 9.8))

  (is (fn? (fn [])))
  (is (fn? console.log))
  (is (= false (fn? :a)))

  (is (= (type #"a") 'regexp))

  (is (set? #{:a :b 2}))
  (is ((fn [] (map? arguments))))
  )

(deftest map?-tests
  (is (map? {:a 1 :b 2}))
  (is (not (map? 1)))

  (is (not (map? #{:a 1 :b 2})))

  (is (not (map? true)))

  (is (not (map? null)))

  (is (not (map? #"foo")))

  (is (not (map? (fn []))))

  (is (not (map? [:yo])))
  (is (not (map? (Array. :yo))))
  )

(deftest boolean-tests
  (is (= (boolean 3)
         true))
  (is (= (boolean 0)
         true))
  (is (= (boolean "")
         true))
  (is (= (boolean [])
         true))
  (is (= (boolean nil)
         false))
  (is (= (boolean false)
         false))
  (is (= (boolean null)
         false)))

(deftest inc-dec-test
  (is (= (inc 3) 4))
  (is (= (dec 3) 2)))

(deftest str-tests
  (is (= (str 1 2 3 nil) "123"))
  (is (= (str {:a 1 :b {:c 3 "d and e" 4}})
         "{\"a\" 1, \"b\" {\"c\" 3, \"d and e\" 4}}")))

(deftest math-operator-tests
  (is (= 10
         (+ 1 2 3 4)))
  (is (= -15
         (- 0 1 2 3 4 5)))
  (is (= 120
         (* 1 2 3 4 5)))
  (is (= 6
         (apply + [1 2 3]))))

(deftest primity-type-equal-tests
  (is (= nil nil))
  (is (= false false))
  (is (= 2 2))
  (is (= "ab" :ab))
  (is (= true true))
  (is (= null null))
  (is (= false (= [1 2] [2 3])))
  (is (= false (= {:a 1} {:a 1 :b 2})))
  (is (= false (=* (fn []) (fn []))))
  (is (= false (apply = [1 1 2]))))

(deftest primitve-equal-tests
  (is (=** 'foo 'foo 'foo 'foo))
  (is (not (=** 'foo 'foo 'bar 'foo))))

(deftest non-primity-type-equal-tests
  (is (= "ab" :ab 'ab))
  (is (= false false false false))
  (is (= nil nil nil nil))
  (is (= {} {}))
  (is (= {} {} {} {}))
  (is (= [1 2] [1 2]))
  (is (= {:a 1 :b 2} {:a 1 :b 2}))
  (is (= {:a 1 :b 2} {:b 2 :a 1}))
  (is (= console.log console.log))
  (def my-print console.log)
  (is (= my-print console.log))
  (is (apply = [1 1 1])))

(deftest function-tests
  (is (= 3
         (count [1 2 3])))
  (is (= false
         (not true)))
  (is (= true
         (contains? {:foo 1 :bar 2} :foo)))

  (is (= {"foo" 1, "baz" 3}
         (select-keys {:foo 1 :bar 2 :baz 3} [:foo :baz])))

  (is (= [1 2 3]
         (vals {:foo 1 :bar 2 :baz 3})))

  (is (= ["foo" "bar" "baz"]
         (keys {:foo 1 :bar 2 :baz 3})))

  (is (= [2 4 6]
         (filter (fn [x] (=== (rem x 2) 0)) [1 2 3 4 5 6]))))

(deftest count-test
  (is (= 5 (count "abcde")))
  (is (= 0 (count [])))
  (is (= 1 (count [1])))
  (is (= 3 (count [1 2 3]))))

(deftest reduce-test
  (is (= 120
         (reduce (fn [x y] (* x y))
                     1
                     [1 2 3 4 5])))
  (is (= 120
         (reduce (fn [x y] (* x y))
                     [1 2 3 4 5])))
  (is (= 10
         (reduce + [1 2 3 4]))))

(deftest non-native-tests
  (is (= 120
         (reduce* (fn [x y] (* x y))
                  1
                  [1 2 3 4 5])))
  (is (= (map* #(+ 1 %) [1 2 3 4 5]) [2 3 4 5 6]))
  (is (= true (every?* even? [2 4 6])))
  (is (= false (every?* even? [1 4 6])))
  (is (= true (some* even? [1 2 3 4])))
  (is (not (some* even? [1 3 5 7])))
  (is (= (filter* even? [1 2 3 4 5]) [2 4])))

(deftest defmulti-tests
  (defmulti  foo (fn [& args] (count args)))
  (defmethod foo 0 [] "zero")
  (defmethod foo 1 [x] "one")
  (defmethod foo 2 [x y] "two")
  (defmethod foo :default [& args] "anything")

  (is (= [(foo)
          (foo 'x)
          (foo 'x 'y)
          (foo 'x 'y 'z)]
         ["zero" "one" "two" "anything"])))

(deftest fn-tests
  (fn bar
    ([] 0)
    ([x] 1)
    ([x y]   (+ x y))
    ([x y z] (* x y z)))

  (is (= [(bar)
          (bar 1)
          (bar 2 3)
          (bar 4 5 6)]
         [0 1 5 120])))

(deftest true?-and-false?-tests
  (is (= true (true? true)))
  (is (= false (true? false)))
  (is (= true (false? false)))
  (is (= false (false? true))))

(deftest not-tests
  ;;(is (= false (not 0)))
  (is (not 0))
  (is (not nil))
  )

(deftest not=-tests
  (is (= (not= 1 2)
         true))
  (is (= (not= 1 1 1 2)
         true))
  (is (= (not= 1 1)
         false))
  (is (= (not= 1 1 1 1)
         false)))

(deftest if-tests
  (is (= true (if-not false true false))))

(deftest if-not-tests
  (is (if-not false true))
  (is (= false (if-not true true false)))
  ;; (is (if-not true false true))
  )

(deftest and-tests
  (is (and true true true))
  (is (not (and true false true)))
  (is (not (and nil true))))

(defn fact [n] (loop [n n x (dec n)] (if (= 1 x) n (recur (* n x) (dec x)))))

(deftest factorial-tests
  (is (= 1307674368000 (fact 15))))

(deftest compare-tests
  (is (= (compare 0 1) -1))
  (is (= (compare 0 -1) 1))
  (is (= (compare "a" "b") -1)))

(deftest or-tests
  (is (= true (or true false false)))
  (is (= false (or false false false)))
  (is (= true (or false true false))))

(deftest zero?-tests
  (is (= true (zero? 0)))
  (is (= false (zero? 1))))

(deftest int-tests
  (is (= 1 (int 1.1)))
  (is (= 1 (int 1.9)))
  (is (= -1 (int -1.1)))
  (is (= -1 (int -1.9))))

(deftest <-tests
  ;;(is (= true (< 1)))
  (is (= true (< 1 2)))
  (is (= true (< 1 2 3)))
  (is (= true (< 1 2 3 4)))
  ;;(is (= false (< 1 3 2 4)))
  )

(deftest reverse-tests
  (def a [1 2 3 4 5])
  (is (= (reverse a) [5 4 3 2 1]))
  (is (= a [1 2 3 4 5])))

(deftest max-tests
  (is (= (max 1) 1))
  (is (= (max 1 2) 2))
  (is (= (max 3 2 1) 3))
  (is (= (apply max [1 2 3]) 3)))

(deftest min-tests
  (is (= (min 1) 1))
  (is (= (min 1 2) 1))
  (is (= (min 3 2 1) 1))
  (is (= (apply min [1 2 3]) 1)))

(deftest pos?-tests
  (is (= true (pos? 1)))
  (is (= false (pos? -1)))
  (is (= false (pos? 0))))

(deftest neg?-tests
  (is (= false (neg? 1)))
  (is (= true (neg? -1)))
  (is (= false (neg? 0))))

(deftest quot-tests
  (is (= (quot 23 7) 3))
  (is (= (quot 4 2) 2))
  (is (= (quot 3 2) 1))
  (is (= (quot 6 4) 1))
  (is (= (quot 0 5) 0))
  (is (= (quot 4.0 2.0) 2.0))
  (is (= (quot 4.5 2.0) 2.0))
  (is (= (quot 42 5) 8))
  (is (= (quot 42 -5) -8))
  (is (= (quot -42 5) -8))
  (is (= (quot -42 -5) 8))
  (is (= (quot 9 3) 3))
  (is (= (quot 9 -3) -3))
  (is (= (quot -9 3) -3))
  (is (= (quot -9 -3) 3))
  (is (= (quot 2 5) 0))
  (is (= (quot 2 -5) 0))
  (is (= (quot -2 5) 0))
  (is (= (quot -2 -5) 0))
  (is (= (quot 0 3) 0))
  (is (= (quot 0 -3) 0)))

(deftest bit-and-tests
  (is (= (bit-and 5 4) 4))
  (is (= (bit-and 5 4 1) 0))

(deftest bit-or-tests
  (is (= (bit-or 6 5 4 2) 7))))

(deftest bit-xor-tests
  (is (= (bit-xor 2 3 4) 5)))

(deftest bit-shift-left-tests
  (is (= (bit-shift-left 1 3) 8)))

(deftest bit-shift-right-tests
  (is (= (bit-shift-right 8 3) 1)))

;; (deftest bit-and-not-tests
;;   (is (= (bit-and-not 3 1 2) 0)))

;; (deftest bit-clear-tests
;;   (is (= (bit-clear 3 1) 1)))

;; (deftest bit-set-tests
;;   (is (= (bit-set 0 1) 2)))

;; (deftest bit-flip-tests
;;   (is (= (bit-flip 0 1) 2))
;;   (is (= (bit-flip 2 1) 0)))

;; (deftest bit-test-tests
;;   (is (= true (bit-test 3 1)))
;;   (is (= false (bit-test 1 1))))

(deftest integer?-tests
  (is (= true (integer? 1)))
  (is (= false (integer? "1"))))

(deftest even?-tests
  (is (= true (even? 2)))
  (is (= false (even? 1))))

(deftest odd?-tests
  (is (= true (odd? 1)))
  (is (= false (odd? 2))))

(deftest complement-tests
  (is (= true
         ((complement (fn [] false)))))
  (is (= true
         ((complement (fn [x] false)) 1)))
  (is (= true
         ((complement (fn [x y] false)) 1 2)))
  (is (= true
         ((complement (fn [x y z] false)) 1 2 3))))

(deftest constantly-tests
  (is (= ((constantly 1) 1 2 3 4 5) 1)))

(deftest identity-tests
  (is (= (identity 3) 3))
  (is (= (identity 4) 4)))

(deftest contains?-tests
  (is (= true (contains? [4 4 4 4] 3)))
  (is (= true (contains? {:a 1 :b 2} :a)))
  (is (= false (contains? [1 1 1] 4)))
  (is (= false (contains? {:a 4} :b))))

(deftest get-tests
  (is (= (get {:a 1} :a) 1))
  (is (= (get {:a 1} :b) nil))
  (is (= (get {:a 1} :b 1) 1))
  (is (= (get {:a 1 :b 0} :b 1) 0))
  (is (= (get [1 :x 'y] 2) 'y))
  (is (= (get {:a false} :a 4) false))
  (is (= (get {:a nil} :a 4) nil))
  (is (= (get "abc" 1) "b"))
  (is (= (get "abc" 10 :xyz) :xyz)))

(deftest conj-tests
  (is (= (conj [1] 2) [1 2]))
  (is (= (conj nil 1) [1])))

(deftest assoc-map-test
  (def my-map {:a 1 :b 2})
  (def new-map (assoc my-map :c 3))
  (is (= (assoc nil :k 3)
         {:k 3}))
  (is (= new-map {:a 1 :b 2 :c 3}))
  (is (= my-map {:a 1 :b 2})))

(deftest assoc-vector-test
  (def my-vec [0 1 2])
  (def new-vec (assoc my-vec 1 :x))
  (is (= new-vec [0 :x 2]))
  (is (= my-vec [0 1 2])))

(deftest dissoc-tests
  (def my-map {:a 1 :b 2})
  (is (= (dissoc my-map :b) {:a 1}))
  (is (= (dissoc my-map :a :b) {}))
  (is (= (dissoc my-map :a :c) {:b 2}))
  (is (= my-map {:a 1 :b 2}))
  (is (= (dissoc #{:a 1 :b 2} :a :c) {1 true :b true 2 true})))

(deftest find-tests
  (is (= [:a 1] (find {:a 1 :b 2} :a))))

(deftest select-keys-tests
  (is (= (select-keys {:a 1 :b 2 :c 3} [:a]) {:a 1})))

(deftest keys-tests
  (is (= (keys {:a 1}) [:a])))

(deftest vals-tests
  (is (= (vals {:a 1}) [1])))

(deftest if-let-tests
  (is (= (if-let [a 4] (+ a 4) (+ 10 10)) 8))
  (is (= (if-let [a nil] (+ a 4) (+ 10 10)) 20)))

(deftest when-let-tests
  (is (= (when-let [a 9] (+ a 4))  13))
  (is (not (when-let [a nil] (+ a 4)))))

(deftest fn*-tests
  (is (= (#(+ 1 %) 2) 3)))

;; (deftest partial-tests
;;   (is (= ((partial + 1) 1) 2)))

(deftest every?-tests
  (is (= true (every? even? [2 4 6])))
  (is (= false (every? even? [1 4 6])))
)

(deftest some-tests
  (is (= true (some even? [1 2 3 4])))
  (is (not (some even? [1 3 5 7])))
  (is (= {:id 1 :username "foo"}
         (some #(and (= 1 (:id %)) %)
               [{:id 1 :username "foo"}
                {:id 2 :username "bar"}]))))

(deftest map-tests
  (is (= (map #(+ 1 %) [1 2 3 4 5]) [2 3 4 5 6])))

(deftest mapcat-tests
  (is (= (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
         [0 1 2 3 4 5 6 7 8 9])))

(deftest filter-tests
  (is (= (filter even? [1 2 3 4 5]) [2 4])))

(deftest remove-tests
  (is (= (remove even? [1 2 3 4 5]) [1 3 5])))

(deftest take-tests
  (is (= (take 2 [1 2 3 4]) [1 2])))

(deftest take-while-tests
  (is (= (take-while even? [2 2 1 1]) [2 2])))

(deftest drop-tests
  (is (= (drop 1 [1 2 3]) [2 3])))

(deftest drop-last-tests
  (is (= (drop-last 2 [1 2 3 4]) [1 2])))

(deftest take-last-tests
  (is (= (take-last 3 [1 2 3 4]) [2 3 4])))

(deftest drop-while-tests
  (is (= (drop-while even? [2 4 6 1 2 3]) [1 2 3])))

(deftest cycle-tests
  (is (= (cycle [1 2 3] 3) [1 2 3 1 2 3 1 2 3])))

(deftest split-at-tests
  (is (= (split-at 3 [1 2 3 4 5]) [[1 2 3] [4 5]])))

(deftest split-with-tests
  (is (= (split-with odd? [1 1 1 1 2 2 2 2]) [[1 1 1 1] [2 2 2 2]])))

(deftest repeat-tests
  (is (= (repeat 3 1) [1 1 1])))

(deftest cons-tests
  (is (= (cons 0 [1 2]) [0 1 2])))

(deftest iterate-tests
  (is (= (iterate #(+ 1 %) 0 3) [0 1 2])))

(deftest merge-tests
  (is (= (merge {:a 1 :b 2} {:a 3 :c 4}) {:a 3 :b 2 :c 4})))

;; (deftest merge-with-tests
;;   (is (= (merge-with +
;;                      {:a 1  :b 2}
;;                      {:a 9  :b 98 :c 0})
;;          {:c 0, :a 10, :b 100})))

(deftest zipmap-tests
  (is (= (zipmap [:a :b :c :d :e] [1 2 3 4 5])
         {:e 5, :d 4, :c 3, :b 2, :a 1}))
  (is (= (zipmap [:a :b :c :d :e :f] [1 2 3 4 5])
         {:e 5, :d 4, :c 3, :b 2, :a 1})))

(deftest sort-tests
  (is (= (sort [3 1 2 4]) [1 2 3 4]))
  ;;(is (= (sort #(< %1 %2) (vals {:foo 5, :bar 2, :baz 10})) [10 5 2]))
  )

(deftest set-tests
  (is (= (set [1 2 :true 3 4])
         (hash-set 1 2 :true 3 4)
         (Cl2Set. [1 2 :true 3 4]))))

(deftest nthnext-tests
  (is (= (nthnext [0 1 2 3 4 5 6 7 8 9] 3)
         [3 4 5 6 7 8 9])))

(deftest get-tests
  (is (= (get [1 2 3] 1) 2))
  (is (= (get [1 2 3] 3 (.slice [4])) [4])))

(deftest range-tests
  (is (= (range 0 8 2) [0 2 4 6]))
  (is (= (range 0 -9 -3) [0 -3 -6])))

(deftest reductions-tests
  (is (= [1 3 6 10 15] (reductions #(+ %1 %2) 1 [2 3 4 5])))
  (is (= [2 5 9 14] (reductions + [2 3 4 5]))))

(deftest empty-tests
  (is (= true (empty? [])))
  (is (= true (empty? {})))
  (is (= true (empty? #{})))
  (is (= false (empty? [1])))
  (is (= false (empty? [1 2 3 4]))))

(deftest randnth-tests
  (is (= true (< (rand-nth (range 5)) 5)))
  (is (= true (= (rand-nth [2 2 2 2]) 2))))

(deftest partition-tests
  (is (= (partition 4 4 (range 20))
         [[0 1 2 3] [4 5 6 7] [8 9 10 11] [12 13 14 15] [16 17 18 19]]))
  (is (= (partition 10 10 [] [1 2 3 4])
         [[1 2 3 4]]))
  (is (= (partition 3 3 [] [1 2 3 4])
         [[1 2 3] [4]]))
  (is (= (partition 10 10 nil [1 2 3 4])
         [[1 2 3 4]]))
  (is (= (partition 3 3 [nil nil nil] [1 2 3 4])
         [[1 2 3] [4 nil nil]])))

(deftest subs-tests
  (is (= (subs "abcde" 1)
         "bcde"))
  (is (= (subs "abcde" 1 3)
         "bc")))

(deftest subvec-tests
  (is (= (subvec [1 2 3 4 5] 1)
         [2 3 4 5]))
  (is (= (subvec [1 2 3 4 5] 1 3)
         [2 3])))

(deftest seq-tests
  (is (= (seq [])
         nil))
  (is (= (seq [1 2 3])
         [1 2 3]))
  (is (= (seq "abc")
         ["a" "b" "c"]))
  (is (= (seq {:a 1 :b 2})
         [[:a 1] [:b 2]]))
  (is (= (seq #{:a :b})
         [:a :b])))

(deftest trampoline-tests
  (defn tramfactorial [x n] (if (= 1 n) x #(tramfactorial (* x n) (dec n))))
  (is (= (trampoline tramfactorial 1 5)
         120)))

(deftest interleave-tests
  (is (= (interleave [:a :b :c] [1 2 3] [:x :y :z :zee])
         [:a 1 :x :b 2 :y :c 3 :z])))

(deftest interpose-tests
  (is (= (interpose "," [:a :b :c])
         [:a "," :b "," :c]))
  (is (= (interpose "," {:a 1 :b 2})
         [[:a 1] "," [:b 2]]))
  ;; you can do this but (.join "," "hello") is even better
  (is (= (interpose "," "hello")
         ["h" "," "e" "," "l" "," "l" "," "o"])))

(deftest assoc-in-tests
  (def users {:foo  {:name "John" :age 43}})
  (is (= (assoc-in users [:foo :age] 44)
         {:foo  {:name "John" :age 44}}))
  ;; ensure data is not mutated
  (is (= users
         {:foo  {:name "John" :age 43}}))

  (def user-list [{:foo  {:name "John" :age 43}}
                  {:foo  {:name "Paul" :age 34}}])
  (is (= (assoc-in user-list [0 :foo :age] 44)
         [{:foo  {:name "John" :age 44}}
          {:foo  {:name "Paul" :age 34}}]))
  ;; ensure data is not mutated
  (is (= user-list
         [{:foo  {:name "John" :age 43}}
          {:foo  {:name "Paul" :age 34}}]))
  (is (= {:foo 1 :bar {:boo {:buzz 5}}}
         (assoc-in {:foo 1} [:bar :boo :buzz] 5))))

(deftest update-in-tests
  (def users [{:name "James" :age 26}
              {:name "John" :age 43}])
  (is (= (update-in users [1 :age] inc)
         [{:name "James", :age 26}
          {:name "John", :age 44}]))
  (= users [{:name "James" :age 26}
            {:name "John" :age 43}]))
