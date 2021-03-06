(ns turtles.core
  (:require [clojure.pprint :refer [pprint]]))

(def tokens-to-win 10)
(def num-players 4)
(def shortcut-cost 4)
(def sided-die 6)
(def all-colors [:red :blue :green :purple])

; not used
(def tokens-to-take-shortcut 10)
(def tokens-to-start 1)

(defn spy [x]
  (println x)
  x)

(defn roll-die []
  (inc (rand-int sided-die)))


   ;; :take-shortcut (fn [player]
   ;;                  (or (= :green color)
   ;;                      (and (= :purple color)
   ;;                           (<= tokens-to-take-shortcut (:tokens player)))))})
(defn make-player [i color] 
  {:color color
   :tokens i #_(if (zero? i) 0 tokens-to-start)
   :skip-next-turn false
   :tile :tile1
   :has-won false
   :lost-turns 0
   :laps 0
   :take-shortcut (constantly (rand-nth [true false]))})

(defn make-players [num-players] 
  {:pre [(<= 2 num-players (count all-colors))]}
  (map-indexed make-player (take num-players all-colors)))

(defn make-tile 
  ([] (make-tile :empty 0))
  ([tile-type] (make-tile tile-type 0))
  ([tile-type value] {:tile-type tile-type :value value}) )

(defmulti forward-from (fn [game t _ _] (get-in game [:board :tiles t :tile-type])))

(defmethod forward-from :empty [game tile player n]
  (if (zero? n)
    (assoc-in game [:players player :tile] tile)
    (let [next-tile (first (get-in game [:board :graph tile]))]
      (forward-from game next-tile player (dec n)))))

(defmethod forward-from :token-change [game tile player n]
  (if (zero? n)
    (let [delta (get-in game [:board :tiles tile :value])]
      (-> game
        (update-in [:players player :tokens] + delta)
        (update-in [:players player :tokens] max 0)
        (assoc-in [:players player :tile] tile)))
    (let [next-tile (first (get-in game [:board :graph tile]))]
      (forward-from game next-tile player (dec n)))))

(defmethod forward-from :lose-a-turn [game tile player n]
  (if (zero? n)
    (let [delta (get-in game [:board :tiles tile :value])]
      (-> game
        (assoc-in [:players player :skip-next-turn] true)
        (update-in [:players player :lost-turns] inc)
        (assoc-in [:players player :tile] tile)))
    (let [next-tile (first (get-in game [:board :graph tile]))]
      (forward-from game next-tile player (dec n)))))

(defmethod forward-from :shortcut-option [game tile player n]
  (assoc-in game [:players player :tile] tile))

(defmethod forward-from :steal [game tile player n]
  (if (zero? n)
    (let [target-player (apply max-key :tokens (-> game :players (dissoc player) vals))
          roll (roll-die)
          takable (min roll (:tokens target-player))]
      (-> game
        (assoc-in [:players player :tile] tile)
        (update-in [:players player :tokens] + takable)
        (update-in [:players player :took] conj takable)
        (update-in [:players (:color target-player) :tokens] - takable)
        (update-in [:players (:color target-player) :taken] conj takable)))
    (let [next-tile (first (get-in game [:board :graph tile]))]
      (forward-from game next-tile player (dec n)))))

(defmethod forward-from :start-finish [game tile player n]
  (if (> (get-in game [:players player :tokens]) tokens-to-win)
    (assoc-in game [:players player :has-won] true)
    (if (zero? n)
      (assoc-in game [:players player :tile] tile)
      (let [next-tile (first (get-in game [:board :graph tile]))]
        (-> game
          (update-in [:players player :laps] inc)
          (forward-from next-tile player (dec n)))))))

(defn make-board [] 
  {:graph {
           :tile1 [:tile2]
           :tile2 [:tile3]
           :tile3 [:tile4]
           :tile4 [:tile5]
           :tile5 [:tile6]
           :tile6 [:tile7]
           :tile7 [:tile8]
           :tile8 [:tile9]
           :tile9 [:tile10]
           :tile10 [:tile11]
           :tile11 [:tile12]
           :tile12 [:tile13]
           :tile13 [:tile14]
           :tile14 [:tile15]
           :tile15 [:tile16]
           :tile16 [:tile17]
           :tile17 [:tile18, :tile23]
           :tile18 [:tile19]
           :tile19 [:tile20]
           :tile20 [:tile21]
           :tile21 [:tile22]
           :tile22 [:tile23]
           :tile23 [:tile24]
           :tile24 [:tile25]
           :tile25 [:tile26]
           :tile26 [:tile1]
           } 
   :tiles {
           ; bridges DO NOT COUNT
           :tile1 (make-tile :start-finish) ; start
           :tile2 (make-tile :token-change 1)
           :tile3 (make-tile)
           :tile4 (make-tile :token-change -1)
           :tile5 (make-tile)
           :tile6 (make-tile :token-change 2)
           :tile7 (make-tile)
           :tile8 (make-tile :lose-a-turn) ; corner
           :tile9 (make-tile :token-change 3)
           :tile10 (make-tile)
           :tile11 (make-tile :token-change 2)
           :tile12 (make-tile :token-change -2)
           :tile13 (make-tile :steal)
           :tile14 (make-tile :token-change 2)
           :tile15 (make-tile :lose-a-turn) ; corner
           :tile16 (make-tile :token-change -2)
           :tile17 (make-tile :shortcut-option shortcut-cost)
           :tile18 (make-tile :token-change 2)
           :tile19 (make-tile :token-change -1)
           :tile20 (make-tile :token-change 3)
           :tile21 (make-tile :steal) ; corner
           :tile22 (make-tile)
           :tile23 (make-tile)
           :tile24 (make-tile :token-change -2)
           :tile25 (make-tile)
           :tile26 (make-tile :token-change 2)
           }})

(defn make-game []
  (let [the-players (make-players num-players)] 
    {:board (make-board)
     :players (zipmap (map :color the-players) the-players)
     :turn 0
     :the-players the-players
     :last-color-played (last all-colors) }))

(defn display-game [game]
  (pprint {:turn (:turn game) :players (:players game)}))

(defn move-player [game current-player moves] 
  (let [tile (get-in game [:players current-player :tile])
        next-tile (first (get-in game [:board :graph tile]))
        player (get-in game [:players current-player])]
    (if (and (= :shortcut-option (:tile-type tile))
             (:take-shortcut player)
             (<= (:value tile) (:tokens player)))
      (forward-from (update-in game [:players current-player :tokens] - (:value tile))
                    (last (get-in game [:board :graph tile]))
                    current-player
                    (dec moves))
      (forward-from game
                    next-tile
                    current-player
                    (dec moves)))))

(defn roll-and-move [game current-player] 
  (move-player game current-player (roll-die)))

(defn play-player [game current-player] 
  (if (get-in game [:players current-player :skip-next-turn])
    (assoc-in game [:players current-player :skip-next-turn] false)
    (roll-and-move game current-player)))

(defn find-first [f coll]
  (first (filter f coll)))

(defn find-winner [game]
  (find-first :has-won (vals (:players game))))

(defn has-winner? [game]
  (some :has-won (vals (:players game))))

(defn find-next-player [game] 
  (let [last-color (:last-color-played game)
        last-index (.indexOf all-colors last-color)
        ]
    (get all-colors (mod (inc last-index) (count all-colors)))))

(defn play-turn [game] 
  (let [ current-player (find-next-player game) ]
    (-> game
        (assoc :previous-game game)
        (play-player current-player)
        (assoc :last-color-played current-player)
        (update :turn inc))))

(defn play-game [] 
  (loop [game (make-game)]
    (if (not (has-winner? game))
      (recur (play-turn game)) 
      game)))

(defn avg [xs]
  (/ (reduce + xs) (count xs)))

(defn -main
  [& args]
  ;; (display-game (play-game)))
  ;; (display-game (first (take 5 (map play-game)))))
  ;; (display-game (last (repeatedly 10000 #(play-game)))))
  (spy (float (avg (map :turn (repeatedly 100 #(play-game)))))))
