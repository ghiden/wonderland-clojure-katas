(ns alphabet-cipher.coder)

(def alphabets (seq "abcdefghijklmnopqrstuvwxyz"))

(def table
  (zipmap alphabets
          (for [i (range 26)]
            (let [values (concat (drop i alphabets) (take i alphabets))]
              (zipmap alphabets values)))))

(defn encode [keyword message]
  (let [code (take (count message) (cycle (seq keyword)))]
    (apply str (map (fn [r c] (get-in table [r c])) (seq message) (seq code)))))

(defn decode [keyword message]
  (let [code (take (count message) (cycle (seq keyword)))
        find-row (fn [m c] (first (vals (first (filter #(= (get % c) m) (vals table))))))]
    (apply str (map (fn [m c] (find-row m c)) (seq message) (seq code)))))
