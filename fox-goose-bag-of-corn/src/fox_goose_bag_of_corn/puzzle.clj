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

(defn done? [states]
  (let [bank (nth (map set (last states)) 2)]
    (= bank #{:you :goose :corn :fox})))

(defn not-safe? [state]
  (or
   (and (contains? state :goose) (contains? state :corn))
   (and (contains? state :fox) (contains? state :goose))))

(defn from-start [states]
  (let [state (last states)
        start (first (map set state))
        bank (nth state 2)]
    (cond
      (= (count start) 4)
      [[:fox :corn] [:boat :goose :you] bank]
      (and (= (count start) 3)
           (not-safe? start))
      (let [previous-boat (set (second (second (reverse states))))
            taking-to-boat (clojure.set/difference start previous-boat)
            keeping-at-start (clojure.set/difference start taking-to-boat #{:you})]
        [(into [] keeping-at-start) (into [:boat :you] taking-to-boat) bank])
      :else
      (let [without-you (clojure.set/difference start #{:you})
            taking-to-boat (first without-you)
            left-over (clojure.set/difference without-you #{taking-to-boat})]
        [(into [] left-over) [:boat :you taking-to-boat] bank]))))

(defn is-you-at-start [state]
  (contains? (set (first state)) :you))

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

(defn bring-back-from-bank [current previous-boat start]
  (let [taking-back (clojure.set/difference current previous-boat)]
    [start
     (into [:boat :you] taking-back)
     (into [] (clojure.set/difference current taking-back #{:you}))]))

(defn from-bank [states]
  (let [state (last states)
        start (first state)
        boat (set (second state))
        bank (set (nth state 2))
        previous-boat (set (second (second (reverse states))))]
    (if (not-safe? bank)
      (bring-back-from-bank bank previous-boat start)
      [start [:boat :you] (into [] (clojure.set/difference bank #{:you}))])))

(defn next-move [states]
  (let [current (map set (last states))]
    (cond
      (contains? (first current) :you)
      (from-start states)
      (contains? (second current) :you)
      (on-boat states)
      :else
      (from-bank states))))

(defn river-crossing-plan []
  (loop [states start-pos]
    (if (not (done? states))
      (recur (conj states (next-move states)))
      states)))
