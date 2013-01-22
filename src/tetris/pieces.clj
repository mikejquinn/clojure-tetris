(ns tetris.pieces
  (:require [tetris.colors :as colors]))

(def piece-I
  { :color colors/light-blue
    :size 4
    :squares [[0 1] [1 1] [2 1] [3 1]] })

(def piece-J
  { :color colors/dark-blue
    :size 3
    :squares [[0 0] [0 1] [1 1] [2 1]] })

(def piece-L
  { :color colors/orange
    :size 3
    :squares [[0 1] [1 1] [2 1] [2 0]] })

(def piece-O
  { :color colors/yellow
    :size 2
    :squares [[0 0] [1 0] [0 1] [1 1]] })

(def piece-S
  { :color colors/green
    :size 3
    :squares [[0 1] [1 1] [1 0] [2 1]] })

(def piece-T
  { :color colors/purple
    :size 3
    :squares [[0 1] [1 1] [2 1] [1 0]] })

(def piece-Z
  { :color colors/red
    :size 3
    :squares [[0 0] [1 0] [1 1] [2 1]] })

(def all-pieces
  [piece-I piece-J piece-L piece-O piece-S piece-T piece-Z])

(defn random-piece
  []
  (rand-nth all-pieces))

(defn rotate-counter-clockwise
  [piece]
  (let [{ :keys [squares size] } piece
        new-squares (map #(do [(% 1) (- size (% 0) 1)]) squares)]
    (assoc piece :squares new-squares)))
    
(defn rotate-clockwise
  [piece]
  (let [{ :keys [squares size] } piece
        new-squares (map #(do [(- size (% 1) 1) (% 0)]) squares)]
    (assoc piece :squares new-squares)))

