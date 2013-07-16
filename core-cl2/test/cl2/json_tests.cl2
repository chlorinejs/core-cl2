(deftest serialize-tests
  (is (= (serialize "foo")
         "\"foo\""))
  (is (= (serialize 1)
         "1"))
  (is (= (serialize {:a 1 :b 2})
         "{\"a\":1,\"b\":2}")))

(deftest deserialize-tests
  (is (= (deserialize "foo")
         nil))
  (is (= (deserialize "foo")
         nil))
  (is (= (deserialize "{\"a\":1,\"b\":2}")
         {:a 1 :b 2})))
