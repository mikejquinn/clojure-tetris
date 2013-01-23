(ns tetris.board
  (:require [tetris.pieces :as pieces]))

(defn rotate-piece-counter-clockwise
  [positioned-piece]
  (let [piece (:piece positioned-piece)
        rotated-piece (pieces/rotate-counter-clockwise piece)]
    (assoc positioned-piece :piece rotated-piece)))

(defn rotate-piece-clockwise
  [positioned-piece]
  (let [piece (:piece positioned-piece)
        rotated-piece (pieces/rotate-clockwise piece)]
    (assoc positioned-piece :piece rotated-piece)))

(defn test-coord-in-bounds
  [board [x y]]
  (let [{{:keys [width height]} :size} board]
    (and (<= 0 x) (< x width)
         (<= 0 y) (< y height))))

(defn test-coords-in-bounds
  [board coords]
  (empty? (filter #(not (test-coord-in-bounds board %)) coords)))

(defn positioned-piece
  [board piece]
  { :position [0 0]
    :piece piece })

(defn empty-board
  "Create a rectangular tetris board with the specified width and height."
  [width height]
  {:size {:width width :height height}
   :state {}})


