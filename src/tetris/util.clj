(ns tetris.util)

(defn hash-slice
  "Returns a new hash, with only the specefic key/value pairs
   from the original hash."
  [orig-hash keys]
  (reduce (fn [hash key]
            (if-let [value (orig-hash key)]
              (assoc hash key (orig-hash key))
              hash)) {} keys))

