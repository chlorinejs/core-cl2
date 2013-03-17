(defhtml hello-html
  "My day!"
  [a-var an-other-var]
  [:div
   [:div#id.foo
    a-var]
   [:span.bar an-other-var]])

(deftest defhtml-tests
  (is (= (hello-html "foo" 3)
         (+* "<div><div class=\"foo\" id=\"id\">foo</div>"
             "<span class=\"bar\">3</span></div>"))))