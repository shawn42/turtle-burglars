(ns turtles.core
  (:require [clojure.pprint :refer [pprint]]))

(def all-players [:red :blue :green :purple])

(defn make-player [color] 
  {:color color :tokens 0 :skip-next-turn false})

(defn make-tile 
  ([] (make-tile :emtpy 0))
  ([tile-type value] 
    {:tile-type tile-type :value value}) )

(defn make-players [num-players] 
  {:pre [(<= 2 num-players (count all-players))]}
  (map make-player (take num-players all-players)))

(defn make-board [] 
  {:graph {
           :tile1 [:tile2]
           :tile2 [:tile3, :tile1]
           } 
   :tiles {
           :tile1 (make-tile :token-change 2) 
           :tile2 (make-tile :token-change -1)
           :tile3 (make-tile)
           }})

(defn make-game []
  {:board (make-board) :players (make-players 2) :turn 0})

(defn display-game [game]
  (pprint game))

(defn play-turn [game]
  (update game :turn inc))

(defn -main
  [& args]
  (loop [game (make-game)]
    (display-game game)
    (when (< (:turn game) 4)
      (recur (play-turn game)))))

