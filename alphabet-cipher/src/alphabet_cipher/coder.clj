(ns alphabet-cipher.coder)

(def alphabets (seq "abcdefghijklmnopqrstuvwxyz"))

(def table
  (zipmap alphabets
          (for [i (range 26)]
            (let [values (concat (drop i alphabets) (take i alphabets))]
              (zipmap alphabets values)))))

(defn gen-code [keyword message]
  (take (count message) (cycle (seq keyword))))

(defn encode [keyword message]
  (let [code (gen-code keyword message)]
    (apply str (map (fn [r c] (get-in table [r c])) (seq message) (seq code)))))

(defn find-row [m c]
  (->> table
      (vals)
      (filter #(= (get % c) m))
      (first)
      (vals)
      (first)))

(defn decode [keyword message]
  (let [code (gen-code keyword message)]
    (apply str (map (fn [m c] (find-row m c)) (seq message) (seq code)))))
