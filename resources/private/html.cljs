(defn html-set-attrs [el attrs]
  (dokeys [k attrs] (.setAttribute el k (get attrs k))))

(defn html [spec]
  (cond
   (undefined? spec) (.createTextNode document "")
   (string? spec) (.createTextNode document spec)
   (array? spec) (let [el (.createElement document (first spec))
                       kindex 1]
                   (when (map? (second spec))
                     (html-set-attrs el (second spec))
                     (set! kindex (+ 1 kindex)))
                   (loop [i kindex]
                     (when (< i (count spec))
                       (.appendChild el (html (get spec i)))
                       (recur (+ i 1))))
                   el)
   :else spec))

(defn html-str
  "Generate a string representation of the specified HTML spec."
  [spec]
  (let [map-str (fn [m]
                  (let [s []]
                    (dokeys [k m] (.push s (+ k "='" (get m k) "'")))
                    (join " " s)))]
    (if (array? spec)
      (join ""
            [(+ "<" (first spec)
                (if (map? (second spec)) (+ " " (map-str (second spec))) "")
                ">")
             (let [s []
                   kindex (if (map? (second spec)) 2 1)]
               (loop [i kindex]
                 (when (< i (count spec))
                   (.push s (html-str (get spec i)))
                   (recur (+ i 1))))
               (join "" s))
             (+ "</" (first spec) ">")])
      spec)))
