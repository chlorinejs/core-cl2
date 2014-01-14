(defmacro hiccup
  "Creates an expression mixed of HTML code (using Hiccup at compile time)
  and symbols."
  [& forms]
  (let [symbol->place-holder #(str "{{CL2HIC}}" (name %) "{{CL2HIC}}")
        body (doall (clojure.walk/postwalk
                     (fn [x] (if (symbol? x) (symbol->place-holder x) x))
                     forms))
        hiccup-output (clojure.string/split
                       (hiccup.core/html body)
                       #"\{\{CL2HIC\}\}")]
    `(+* ~@(mapcat (fn [[html var]] (list html (symbol var)))
                 (partition 2 hiccup-output))
         ~(last hiccup-output))))