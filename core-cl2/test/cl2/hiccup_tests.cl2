(deftest hiccup-tests
  (defn span-sum
    "Calculates sum of two numbers and puts the result into a span."
    [x y]
    (let [z (+ x y)]
      (hiccup [:div.foo [:span#bar z]]
              [:div.jazz y])))

  (is (= (span-sum 4 5)
         (+ "<div class=\"foo\"><span id=\"bar\">9</span></div>"
            "<div class=\"jazz\">5</div>"))))
