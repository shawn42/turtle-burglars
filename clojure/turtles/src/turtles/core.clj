(ns turtles.core
  (:require [clojure.pprint :refer [pprint]]))

(def all-players [:red :blue :green :purple])

(defn make-player [color] 
  {:color color :tokens 0 :skip-next-turn false :tile :tile1})

(defn make-tile 
  ([] (make-tile :empty 0))
  ([tile-type] (make-tile tile-type 0))
  ([tile-type value] 
   {:tile-type tile-type :value value}) )

(defn make-players [num-players] 
  {:pre [(<= 2 num-players (count all-players))]}
  (map make-player (take num-players all-players)))

(defn spy [x]
  (println x)
  x)

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
           :tile17 [:tile18]
           :tile18 [:tile19]
           :tile19 [:tile20]
           :tile20 [:tile21, :tile29]
           :tile21 [:tile22]
           :tile22 [:tile23]
           :tile23 [:tile24]
           :tile24 [:tile25]
           :tile25 [:tile26]
           :tile26 [:tile27]
           :tile27 [:tile28]
           :tile28 [:tile29]
           :tile29 [:tile1]
           } 
   :tiles {
           :tile1 (make-tile :token-change 2) 
           :tile2 (make-tile :token-change -1)
           :tile3 (make-tile)
           :tile4 (make-tile :token-change 1)
           :tile5 (make-tile)
           :tile6 (make-tile)
           :tile7 (make-tile)
           :tile8 (make-tile)
           :tile9 (make-tile :token-change 2)
           :tile10 (make-tile)
           :tile11 (make-tile #_:lose-a-turn)
           :tile12 (make-tile)
           :tile13 (make-tile)
           :tile14 (make-tile :token-change 2)
           :tile15 (make-tile)
           :tile16 (make-tile)
           :tile17 (make-tile)
           :tile18 (make-tile)
           :tile19 (make-tile :token-change 2)
           :tile20 (make-tile #_:shortcut-option #_5)
           :tile21 (make-tile)
           :tile22 (make-tile)
           :tile23 (make-tile)
           :tile24 (make-tile)
           :tile25 (make-tile :token-change 2)
           :tile26 (make-tile)
           :tile27 (make-tile #_:steal)
           :tile28 (make-tile)
           :tile29 (make-tile)
           }})

(defn make-game []
  (let [the-players (make-players 2)] 
    {:board (make-board) :players (zipmap (map :color the-players) the-players) :turn 0 :next-players (cycle (map :color the-players))}))

(defn display-game [game]
  (pprint {:turn (:turn game) :players (:players game)}))

(defn roll-die []
  (inc (rand-int 6)))

(defn move-player [game current-player moves] 
  (let [tile (get-in game [:players current-player :tile])
        next-tile (first (get-in game [:board :graph tile]))]
    (forward-from game
                  next-tile
                  current-player
                  (dec moves))))

(defn roll-and-move [game current-player] 
  (move-player game current-player (roll-die)))

(defn play-player [game current-player] 
  (if (get-in game [:players current-player :skip-next-turn])
    (assoc-in game [:players current-player :skip-next-turn] false)
    (roll-and-move game current-player)))

(defn play-turn [game] (let [ current-player (first (:next-players game)) ]
    (-> game
        (assoc :previous-game game)
        (play-player current-player)
        (update :next-players rest)
        (update :turn inc))))

(defn do-stuff [game]
  (doseq [turn-state (take-while (complement nil?) (iterate :previous-game game))]
    (display-game turn-state) ))

(defn -main
  [& args]
  (display-game (loop [game (make-game)]
                  (if (< (:turn game) 10) 
                    (recur (play-turn game)) 
                    game))))

