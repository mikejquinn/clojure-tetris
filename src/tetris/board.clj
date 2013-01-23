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

(defn- positioned-piece-to-board-coords
  [positioned-piece]
  (let [{{ squares :squares } :piece position :position } positioned-piece]
    (map #(do [(+ (% 0) (position 0)) (+ (% 1) (position 1))]) squares)))

(defn test-coord-in-bounds
  [board [x y]]
  (let [{{:keys [width height]} :size} board]
    (and (<= 0 x) (< x width)
         (<= 0 y) (< y height))))

(defn test-coords-in-bounds
  [board coords]
  (empty? (filter #(not (test-coord-in-bounds board %)) coords)))

(defn piece-collision-test
  "Tests to see if the passed piece collides with any other board elements (walls, blocks)"
  [board piece]
  (let [{{:keys [width height]} :size} board
        state (:state board)
        piece-board-coords (positioned-piece-to-board-coords piece)
        collisions (filter #(do (contains? state %)) piece-board-coords)]
    (if (empty? collisions)
      (not (test-coords-in-bounds board piece-board-coords))
      true)))

(defn translate-positioned-piece
  "Translates a positioned piece in the x and y directions"
  [positioned-piece [dx dy]]
  (let [{ :keys [position piece] } positioned-piece
        [x y] position
        new-origin [(+ x dx), (+ y dy)]]
    { :position new-origin :piece piece }))

(defn translated-piece-seq
  "Creates a lazy sequence of iterative translations to a positioned piece on
  the game board. The sequence ends when the translated piece collides with another
  element on the board."
  [board positioned-piece translation]
  (cons positioned-piece
        (lazy-seq
          (let [new-piece (translate-positioned-piece positioned-piece translation)]
            (if (piece-collision-test board new-piece)
              nil
              (translated-piece-seq board new-piece translation))))))

(defn positioned-piece
  [board piece]
  { :position [0 0]
    :piece piece })

(defn empty-board
  "Create a rectangular tetris board with the specified width and height."
  [width height]
  {:size {:width width :height height}
   :state {}})

