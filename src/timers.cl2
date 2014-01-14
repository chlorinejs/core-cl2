(defmacro do-interval
  "Executes a block of code repeatedly, with a fixed time delay between
  each execution. Current scope (aka `this` object) is reserved."
  [time & body]
  `(let [this# this]
     (setInterval
      (fn [] (let [func# (fn [] ~@body)]
               (.call func# this#)
               nil))
      ~time)))

(defmacro add-interval
  "Assigns a do-interval to a symbol so that it can be removed
  in the future."
  [symbol time & body]
  `(def ~symbol (do-interval ~time ~@body)))

(defmacro remove-interval
  "Removes a named interval."
  [symbol]
  `(do (clearInterval ~symbol)
       (set! ~symbol nil)))

(defmacro do-timeout
  "Executes a block of code after specified time. Current scope
  (aka `this` object) is reserved."
  [time & body]
  `(let [this# this]
     (setTimeout
      (fn [] (let [func# (fn [] ~@body)]
               (.call func# this#)
               nil))
      ~time)))

(defmacro add-timeout
  "Assigns a do-timeout to a symbol so that it can be removed
  in the future."
  [symbol time & body]
  `(def ~symbol (do-timeout ~time ~@body)))

(defmacro remove-timeout
  "Removes a named timeout."
  [symbol]
  `(do (clearTimeout ~symbol)
       (set! ~symbol nil)))
