(defn serialize
  "Converts a message object to JSON strings so that it can be
  transferred over the network."
  [data]
  (. JSON (stringify data)))

(defn deserialize
  "Converts a serialized string back to object. Return nil if string
  is an invalid JSON."
  [s]
  (try (JSON.parse s) (catch e nil)))
