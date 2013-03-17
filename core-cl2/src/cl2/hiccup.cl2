(defmacro defhtml
  "Creates a function that return HTML code using Hiccup at compile time."
  [fname & fdeclr]
  (let [[docstring bindings & body] ;; fdeclr
        (if (string? (first fdeclr))
          fdeclr
          (cons nil fdeclr))
        varname->place-holder #(str "{{CL2HIC}}" (name %) "{{CL2HIC}}")
        body (doall (clojure.walk/postwalk
                     (fn [x] (if (symbol? x) (varname->place-holder x) x))
                     body))
        hiccup-output (clojure.string/split
                       (hiccup.core/html body)
                       #"\{\{CL2HIC\}\}")
        final-body (concat '(+*)
                           (mapcat (fn [[html var]] (list html (symbol var)))
                                   (partition 2 hiccup-output))
                           [(last hiccup-output)])]
    `(defn ~fname
       ~@(if docstring [docstring] [])
       ~bindings ~final-body)))