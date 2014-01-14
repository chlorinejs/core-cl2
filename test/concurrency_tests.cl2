(deftest atom-tests
  (def a (atom 3))
  (is (= 3 @a))
  (swap! a inc)
  (is (= 4 @a))
  (reset! a 5)
  (is (= 5 @a))
  (swap! a + 1 2)
  (is (= 8 @a)))

(deftest atom-validator-tests
  (def b (atom 3 :validator odd?))
  (is (= 3 @b))

  (is (= "Invalid reference state"
         (do (try
               (swap! b inc)
               (catch e e)))))
  (is (= 3 @b))

  (reset! b 5)
  (is (= 5 @b))

  (is (= "Invalid reference state"
         (do (try
               (reset! b 6)
               (catch e e)))))
  (is (= 5 @b)))

(deftest add-watch-&remove-watch-tests
  (def c (atom 4))
  (def temp-1)
  (def temp-2)
  (add-watch c :all-keys
             (fn [k a o n]
               (set! temp-1
                     (pr-str {:k k :o o :n n}))))
  (add-watch c :inc-old-dec-new
             (fn [k a o n]
               (set! temp-2
                     (pr-str {:k k :o (inc o) :n (dec n)}))))

  (is (= 4 @c))

  (swap! c inc)

  (is (= temp-1
         (pr-str {:k :all-keys :o 4 :n 5})))
  (is (= temp-2
         (pr-str {:k :inc-old-dec-new :o 5 :n 4})))

  (reset! c 3)

  (is (= temp-1
         (pr-str {:k :all-keys :o 5 :n 3})))
  (is (= temp-2
         (pr-str {:k :inc-old-dec-new :o 6 :n 2})))

  (remove-watch c :inc-old-dec-new)
  (reset! c 3)

  (is (= temp-1
         (pr-str {:k :all-keys :o 3 :n 3})))
  (is (= temp-2
         (pr-str {:k :inc-old-dec-new :o 6 :n 2})))

  )
