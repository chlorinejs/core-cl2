(defmulti  str* type)
(defmethod str* "array"    [a]
  (+* "["
      (.join (map str* a) ", ")
      "]"))
(defmethod str* "nil"      [x] "")
(defmethod str* "function" [x] (+* "" x))
(defmethod str* "map"      [m]
  (+* "{"
      (.join (map (fn [k] (+* "\"" k "\" " (str* (get m k))))
                  (keys m)) ", ")
      "}"))

(defmethod str* "regexp"   [x] (+* "#\"" x "\""))
;; string, number, boolean, function
(defmethod str* :default [x] (+* "" x))

(fn str [& args]
  (.join (map str* args) ""))

(defmulti  pr-str* type)

(defmethod pr-str* "string" [x]
  (+* "\""
      (.. x
          (replace #"/\\/g" "\\\\")
          (replace #"/\t/g" "\\t")
          (replace #"/\v/g" "\\v")
          ;;(replace #"/\b/g" "\\b")
          (replace #"/\f/g" "\\f")
          (replace #"/\n/g" "\\n")
          (replace #"/\r/g" "\\r")
          (replace #"/\"/g" "\"")
          (replace #"/\'/g" "\\'"))
      "\""))

(defmethod pr-str* "array"    [a]
  (+* "["
      (.join (map pr-str* a) ", ")
      "]"))
(defmethod pr-str* "nil"      [x] "nil")
(defmethod pr-str* "function" [x] (+* "(inline " (pr-str* (+* "" x)) ")"))
(defmethod pr-str* "map"      [m]
  (+* "{"
      (.join (map (fn [k] (+* "\"" k "\" " (pr-str* (get m k))))
                  (keys m)) ", ")
      "}"))
(defmethod pr-str* "regexp"   [x] (+* "#\"" x "\""))
;; string, number, boolean, function
(defmethod pr-str* :default [x] (+* "" x))

(fn pr-str []
  (.. (map pr-str* arguments) (join " ")))
