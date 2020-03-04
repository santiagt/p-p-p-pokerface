(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
       figures {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14}]
    (if (contains? figures (str fst))
       (get figures (str fst))
       (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [check-pair (filter (fn [x] (> x 1)) (vals (frequencies (map rank hand))))]
    (not (empty? check-pair))))

(defn three-of-a-kind? [hand]
  (let [check-pair (filter (fn [x] (> x 2)) (vals (frequencies (map rank hand))))]
    (not (empty? check-pair))))

(defn four-of-a-kind? [hand]
  (let [check-pair (filter (fn [x] (> x 3)) (vals (frequencies (map rank hand))))]
    (not (empty? check-pair))))

(defn flush? [hand]
  (let [check-pair (filter (fn [x] (> x 4)) (vals (frequencies (map suit hand))))]
    (not (empty? check-pair))))

(defn full-house? [hand]
  (let [re-cards (sort (vals (frequencies (map rank hand))))]
      (if (= re-cards (sequence [2 3]))
          true
          false)))

(defn two-pairs? [hand]
  (let [re-cards (sort (vals (frequencies (map rank hand))))
        comb1 (sequence [1 2 2])
        comb2 (sequence [1 4])]
    (if (or (= re-cards comb1)
           (= re-cards comb2))
      true
      false)))

(defn straight? [hand]
  (let [max-h (apply max (map rank hand))
        min-h (apply min (map rank hand))
        max-as (apply max (replace {14 1} (map rank hand)))
        min-as (apply min (replace {14 1} (map rank hand)))
        as? (= (some #{14} (map rank hand)) 14)]
    (cond (and (not as?) (not (pair? hand)) (= (- max-h min-h) 4)) true
          (and as? (not (pair? hand)) (or (= (- max-h min-h) 4) (= (- max-as min-as) 4))) true
          :else false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
        true
        false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    (high-card? hand) 0))

