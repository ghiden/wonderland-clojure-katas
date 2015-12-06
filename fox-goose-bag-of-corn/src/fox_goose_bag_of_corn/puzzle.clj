(ns fox-goose-bag-of-corn.puzzle
  (require [clojure.set]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def manual-solution [[[:fox :goose :corn :you] [:boat] []]
                      [[:fox :corn] [:boat :goose :you] []]
                      [[:fox :corn] [:boat] [:goose :you]]
                      [[:fox :corn] [:boat :you] [:goose]]
                      [[:fox :corn :you] [:boat] [:goose]]
                      [[:fox] [:boat :corn :you] [:goose]]
                      [[:fox] [:boat] [:goose :corn :you]]
                      [[:fox] [:boat :goose :you] [:corn]]
                      [[:fox :goose :you] [:boat] [:corn]]
                      [[:goose] [:boat :fox :you] [:corn]]
                      [[:goose] [:boat] [:fox :corn :you]]
                      [[:goose] [:boat :you] [:fox :corn]]
                      [[:goose :you] [:boat] [:fox :corn]]
                      [[] [:boat :goose :you] [:fox :corn]]
                      [[] [:boat] [:fox :goose :corn :you]]])

(defn done? [state]
  (empty? (first state)))

(defn from-start [states]
  (let [current (first (map set (last states)))]
    (cond
      (and (contains? current :fox)
           (contains? current :goose)
           (contains? current :corn))
      (conj states [[:fox :corn] [:boat :goose :you] []]))))

(defn is-you-at-start [state]
  (contains? (first state) :you))

(defn from-boat-to-start [states]
  (let [state (last states)
        start (set (first state))
        boat (set (second state))
        bank (nth state 2)]
    [(into [] (clojure.set/union start (clojure.set/difference boat #{:boat}))) [:boat] bank])) 

(defn from-boat-to-bank [states]
  (let [state (last states)
        start (first state)
        boat (set (second state))
        bank (set (nth state 2))]
    [start [:boat] (into [] (clojure.set/union bank (clojure.set/difference boat #{:boat})))])) 

(defn on-boat [states]
  (let [current (second (map set (last states)))
        previous (second (reverse states))]
    (if (is-you-at-start previous)
      (from-boat-to-bank states)
      (from-boat-to-start states))))

(defn not-safe? [state]
  (or
    (and (contains? :goose) (contains? :corn))
    (and (contains? :fox) (contains? :goose))))

(defn bring-back-from-bank [current previous]
  (into [] (clojure.set/union #{:boat} (clojure.set/difference current previous))))

(defn from-bank [states]
  (let [state (last states)
        start (first state)
        boat (set (second state))
        bank (set (nth state 2))
        previous-bank (set (second (second (reverse states))))]
    (if (not-safe? bank)
      (bring-back-from-bank bank previous-bank)
      [start [:boat :you] (into [] (clojure.set/difference bank #{:you}))])))

(defn next-move [states]
  (let [current (map set (last states))]
    (cond
      (contains? (first current) :you)
      (from-start states)
      (contains? (second current) :you)
      (on-boat)
      :else
      (from-bank states))))

(defn river-crossing-plan []
  start-pos)
