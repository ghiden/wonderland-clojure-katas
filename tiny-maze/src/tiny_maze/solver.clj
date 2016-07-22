(ns tiny-maze.solver)

(defn pos [x y maze]
  (get-in maze [x y]))

(defn positions [maze]
  (let [size (count maze)]
    (for [x (range size)
          y (range size)]
      [x y])))

(defn start-pos [maze]
  (some (fn [[x y]] (if (= :S (pos x y maze)) [x y])) (positions maze)))

(defn end-pos [maze]
  (some (fn [[x y]] (if (= :E (pos x y maze)) [x y])) (positions maze)))

(defn path? [maze [x y]]
  (not= 1 (pos x y maze)))

(defn next-pos [m paths]
  (let [maze-size (count m)
        last-pos (second (reverse paths))
        [pos-x pos-y] (last paths)
        available (concat
                   (for [x (range (- pos-x 1) (+ pos-x 2))
                         :when (and (>= x 0) (not= x pos-x) (< x maze-size))]
                     [x pos-y])
                   (for [y (range (- pos-y 1) (+ pos-y 2))
                         :when (and (>= y 0) (not= y pos-y) (< y maze-size))]
                     [pos-x y])
                   )]
    (filter #(and (path? m %) (not= last-pos %)) available)))

(defn draw [maze paths]
  (reduce (fn [a [x y]] (assoc-in a [x y] :x)) maze paths))

(defn helper [maze paths]
  (prn paths)
  (cond
    (= (end-pos maze) (last paths)) paths
    (nil? (last paths)) nil
    :else
    (first (filter (complement empty?)
                   (map (fn [p] (helper maze (conj paths p)))
                        (next-pos maze paths))))))

(defn solve-maze [maze]
  (draw maze (helper maze [(start-pos maze)])))

(defn print-maze [maze]
  (doseq [r maze]
    (prn r)))

(def m3 [[:S 0 1]
         [1  0 1]
         [1  0 :E]])

(def a3 [[:x :x 1]
         [1  :x 1]
         [1  :x :x]])

(def m4 [[:S 0 0 1]
         [1  1 0 0]
         [1  0  0 1]
         [1  1  0 :E]])

(def a4 [[:x :x :x 1]
         [1  1 :x 0]
         [1  0 :x 1]
         [1  1  :x :x]])

(def m5 [[0 0 0 1 :S]
         [1  1 0 0 0]
         [1  0  0 1 1]
         [1  1  0 0 0]
         [:E  0  0 1 1]])

(def a5 [[0 0 0 1 :x]
         [1  1 :x :x :x]
         [1  0  :x 1 1]
         [1  1  :x 0 0]
         [:x  :x  :x 1 1]])
