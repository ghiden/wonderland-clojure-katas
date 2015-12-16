(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn next-words
  ([word]
   (next-words word '()))
  ([word used]
   (let [used-words (conj used word)
         candidates (filter #(not (some #{%} used-words)) words)
         w-pattern '(\\ \w)
         matches (for [i (range (count word))]
                   (let [pattern (re-pattern (apply str (concat (take i word) w-pattern (drop (inc i) word))))]
                     (filter #(re-matches pattern %) candidates)))]
     (apply concat matches))))

(defn doublets-helper [word1 word2 used]
  (println word1)
  (println (str "used: " used))
  (if (= word1 word2)
    used
    (for [candidate (next-words word1 used)]
      (doublets-helper candidate word2 (conj used word1)))))

(defn doublets [word1 word2]
  (let [result (flatten (for [candidate (next-words word1)]
                          (doublets-helper candidate word2 (vector word1))))]
    (if (empty? result)
      result
      (concat result (list word2)))))

