(defn Cl2Atom
  "Constructor for ChlorineJS atoms."
  [x validator]
  (def that this)
  (set! that.deref x)
  (when (fn? validator)
    (set! that.validator validator))
  (set! that.watchers {}))

(defmacro atom
  "Creates and returns an Atom instance with an initial value of x and
  zero or more options :validator validate-fn
  validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false."
  [x & options]
  (let [opt (apply hash-map options)]
    (if-let [vali (:validator opt)]
      `(Cl2Atom. ~x ~vali)
      `(Cl2Atom. ~x))))

(defmacro deref
  "Returns an atom's current state."
  [x]
  `(:deref ~x))

(defn deref
  "Returns an atom's current state."
  [x]
  (:deref x))

(defn swap!*
  "Two argument version of swap!"
  [x func]
  (let [old-val @x
        new-val (func old-val)
        valid? (if (:validator x)
                 ((:validator x) new-val)
                 true)]
    (if valid?
      (do (set! (:deref x) new-val)
          (doseq [[watcher-name watcher] (:watchers x)]
            (watcher watcher-name x old-val new-val))
          @x)
      (throw "Invalid reference state"))))

(defmacro swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args)"
  ([x func]
     `(swap!* ~x ~func))
  ([x func & args]
     `(swap!* ~x #(~func % ~@args))))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  [x new-val]
  (let [old-val @x
        valid? (if (:validator x)
                 ((:validator x) new-val)
                 true)]
    (if valid?
      (do (set! (:deref x) new-val)
          (doseq [[watcher-name watcher] (:watchers x)]
            (watcher watcher-name x old-val new-val))
          @x)
      (throw "Invalid reference state"))))

(defn add-watch
  "Adds a watch function to an atom reference. The watch
  fn must be a fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called.
  Keys must be unique per atom, and can be used to remove
  the watch with remove-watch, but are otherwise considered opaque by
  the watch mechanism."
  [x watcher-name func]
  (set! (get (:watchers x) watcher-name) func))

(defn remove-watch
  "Removes a watch (set by add-watch) from a reference."
  [x watcher-name]
  (delete (get (:watchers x) watcher-name)))
