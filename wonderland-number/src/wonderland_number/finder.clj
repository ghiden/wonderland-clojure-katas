(ns wonderland-number.finder)

(defn checkDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn check [wondernum]
  (and (= 6 (count (str wondernum)))
       (checkDigits? wondernum (* 2 wondernum))
       (checkDigits? wondernum (* 3 wondernum))
       (checkDigits? wondernum (* 4 wondernum))
       (checkDigits? wondernum (* 5 wondernum))
       (checkDigits? wondernum (* 6 wondernum))))

(defn wonderland-number []
  (first (take 1 (filter check (range 100000 999999)))))
