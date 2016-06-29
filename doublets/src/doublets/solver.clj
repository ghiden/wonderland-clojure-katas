(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(def a-letter '(\\ \w))

(defn word-regexp-patterns [word]
  (for [i (range (count word))]
    (re-pattern (apply str
                       (concat (take i word)
                               a-letter
                               (drop (inc i) word))))))

(defn next-words
  ([word]
   (next-words word '()))
  ([word used]
   (let [used-words (conj used word)
         candidates (filter #(not (some #{%} used-words)) words)
         matches (for [pattern (word-regexp-patterns word)]
                   (filter #(re-matches pattern %) candidates))]
     (apply concat matches))))

(defn- doublets-helper [word1 word2 used]
  (if (= word1 word2)
    used
    (for [candidate (next-words word1 used)]
      (doublets-helper candidate word2 (conj used word1)))))

(defn doublets [word1 word2]
  (let [result (flatten (doublets-helper word1 word2 []))]
    (if (empty? result)
      result
      (concat result (list word2)))))

;; a better version using tree-seq https://github.com/gensym/wonderland-clojure-katas/blob/doublets-solution/doublets/src/doublets/solver.clj

;; (defn- one-away? [word1 word2]
;;   (and (= (count word1) (count word2))
;;        (= 1 (apply + (map
;;                       #(if (= %1 %2) 0 1)
;;                       word1 word2)))))

;; (defn- start-expansion [word]
;;   [word])

;; (defn- expand [dict path]
;;   (let [word (last path)
;;         seen (into #{} path)]
;;     (->> dict
;;          (filter (complement seen))
;;          (filter (partial one-away? word))
;;          (map (fn [w] (conj path w))))))

;; (defn- expansions [dict word]
;;   (tree-seq (constantly true)
;;             (partial expand dict)
;;             (start-expansion word)))

;; (defn doublets [word1 word2]
;;   (let [matches (filter (fn [path] (= word2 (last path))) (expansions words word1))]
;;     (if-let [shortest  (first (sort-by count matches))]
;;       shortest
;;             [])))
