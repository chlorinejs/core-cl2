(fn ok [expr msg]
  (if (not expr)
    (throw (Error. msg))))

(defn equal
  "An other implement of qunit's equal that use Chlorine's = to compare values"
  [actual expected message]
  (if (= "object" (typeof QUnit))
    (.. QUnit (push (= expected actual) actual expected message))
    (ok (= actual expected)
        (+ "" (or message "")
           "(Expected: " (pr-str expected)
           ", Actual: " (pr-str actual) ")"))))

(defn equal==
  "An other implement of qunit's equal that use Chlorine's == to compare values"
  [actual expected message]
  (if (= "object" (typeof QUnit))
    (.. QUnit (push (= expected actual) actual expected message))
    (ok (= actual expected)
        (+ "" (or message "")
           "(Expected: " (pr-str expected)
           ", Actual: " (pr-str actual) ")"))))

(defn equal===
  "An other implement of qunit's equal that use Chlorine's === to compare values"
  [actual expected message]
  (if (= "object" (typeof QUnit))
    (.. QUnit (push (= expected actual) actual expected message))
    (ok (= actual expected)
        (+ "" (or message "")
           "(Expected: " (pr-str expected)
           ", Actual: " (pr-str actual) ")"))))

(defmacro is [expr & [msg]]
  (if (and (list? expr)
           (#{'= '== '===} (first expr))
           (= 2 (count (rest expr))))
    (let [[expected actual] (rest expr)]
      `(~(case (first expr)
           =   'equal
           ==  'equal==
           === 'equal===) ~actual ~expected ~@(or [msg] [])))
    `(ok ~expr ~@(or [msg] []))))

(defmacro deftest [test-name & tests]
  `(test
    ~(name test-name)
    (fn [] ~@tests)))
