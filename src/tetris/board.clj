(ns tetris.board
  (:require [tetris.pieces :as pieces]
            [tetris.util :as util])
  (:use clojure.contrib.math))

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

(defn row-coords
  "Returns a sequence with the coordinates of all points in a row
   on the game board. The bottom row of the board is considered row 0."
  [board row]
  (let [{{ height :height width :width } :size} board
        y (- height 1 row)]
    (map #(do [% y]) (range width))))

(defn all-row-seqs
  "Returns a sequence of sequences - one sequence per row on the game board,
   from bottom to top. Each sequence contains all the coordinates in the row."
  [board]
  (let [{{ height :height width :width } :size} board]
    (map #(row-coords board %) (range height))))

(defn- shift-unempty-row
  "Returns a function used by remove-filled-rows to remove all 'full' rows
   from the game board AND cause higher rows to fall down to fill the new
   available space."
  [orig-state width]
  (fn [[translate state] row]
    (let [filled (util/hash-slice orig-state row)]
      (if (< (count filled) width)
        (let [row-map (reduce-kv (fn [h [x y] v]
                                   (assoc h [x (+ y translate)] v)) {} filled)]
          [translate (merge state row-map)])
        [(+ 1 translate) state]))))

(defn remove-filled-rows
  [board]
  "Returns a new board by removing all filled rows and shifting higher rows
   downward to fill the new available space."
  (let [{{ width :width } :size state :state} board
        [_ new-state] (reduce (shift-unempty-row state width) [0 {}] (all-row-seqs board))]
    (assoc board :state new-state)))

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

(defn dropped-piece
  [board positioned-piece]
  (let [drop-seq (translated-piece-seq board positioned-piece [0 1])]
    (last drop-seq)))

(defn positioned-piece
  [board piece]
  (let [{{ width :width } :size} board
        center (floor (/ width 2))
        sub (floor (/ (piece :size) 2))
        center (- center sub)]
    {:position [center 0]
     :piece piece }))

(defn empty-board
  "Create a rectangular tetris board with the specified width and height."
  [width height]
  {:size {:width width :height height}
   :state {}})

