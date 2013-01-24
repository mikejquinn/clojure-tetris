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

(defn random-piece-generator
  "Returns an infinite sequence of tetrominos. The randomization
  algorithm generates a randomized bag of all available tetrominos,
  and selects only from that bag until its contents are exhausted,
  at which point a new random bag is generated. This prevents
  the player from receiving a long string of the exact same tetromino
  in a row."
  ([] (random-piece-generator (shuffle all-pieces)))
  ([piece-bag]
   (lazy-seq
     ;(if-let [s (seq piece-bag)]
     (if-let [next-piece (first piece-bag)]
       (cons next-piece (random-piece-generator (rest piece-bag)))
       (random-piece-generator)))))

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

