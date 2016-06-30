(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- permutations [coll]
  (if (seq coll)
    (mapcat (fn [x]
              (map #(cons x %) (permutations (remove #{x} coll))))
            coll)
    [[]]))

(defn- sum-rows [m]
  (map #(reduce + %) m))

(defn- sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn- sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn- valid? [s]
  (and
   (= (set (sum-rows s))
      (set (sum-cols s))
      (set (sum-diagonals s)))
   (= 1
      (count (set (sum-rows s)))
      (count (set (sum-cols s)))
      (count (set (sum-diagonals s))))))

(defn magic-square [values]
  (->> values
       permutations
       (map #(map vec (partition 3 %)))
       (map vec)
       (filter valid?)
       first))
