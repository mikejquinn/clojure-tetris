(ns tetris.game
  (:require [tetris.pieces :as pieces]
            [tetris.board :as board]))

(def ^:private move-delay 150)
(def ^:private fast-fall-factor (/ 1 10))

(defn- positioned-piece-to-board-coords
  [positioned-piece]
  (let [{{ squares :squares } :piece position :position } positioned-piece]
    (map #(do [(+ (% 0) (position 0)) (+ (% 1) (position 1))]) squares)))

(defn- piece-collision-test
  "Tests to see if the passed piece collides with any other elements"
  [board piece]
  (let [{{:keys [width height]} :size} board
        state (:state board)
        piece-board-coords (positioned-piece-to-board-coords piece)
        collisions (filter #(do (contains? state %)) piece-board-coords)]
    (if (empty? collisions)
      (not (board/test-coords-in-bounds board piece-board-coords))
      true)))

(defn- rotate-piece-clockwise
  [game]
  (let [piece (:current-piece game)
        rotated-piece (board/rotate-piece-clockwise piece)]
    (if (piece-collision-test (:board game) rotated-piece)
      game
      (assoc game :current-piece rotated-piece))))

(defn- lock-in-dropping-piece
  [game]
  (let [piece (:current-piece game)
        board (:board game)
        {{:keys [color squares]} :piece [origin-x origin-y] :position} piece
        blocks (map (fn [[x y]] [(+ x origin-x) (+ y origin-y)]) squares)
        block-map (reduce #(assoc %1 %2 color) {} blocks)
        new-board-state (merge (:state board) block-map)]
    (assoc-in game [:board :state] new-board-state)))

(defn- place-new-piece
  [game]
  (-> game
    (lock-in-dropping-piece)
    (assoc :current-piece (board/positioned-piece (:board game ) (:next-piece game)))
    (assoc :next-piece (pieces/random-piece))))

(defn- move-piece
  "Returns a new game with the current piece translated by [x y] spaces,
   if there are no collisions. Returns unchanged game if there are."
  [game translation]
  (let [{ :keys [current-piece board] } game
        { :keys [position piece] } current-piece
        new-position [(+ (position 0) (translation 0))
                      (+ (position 1) (translation 1))]
        new-piece { :position new-position :piece piece }]
    (if (piece-collision-test board new-piece)
      (if (> (translation 1) 0)
        (place-new-piece game)
        game)
      (do
        (assoc game :current-piece new-piece)))))

(defn- throttle-action
  [game action-name action]
  (let [last-press (get-in game [:input-delays action-name])]
    (if (or (nil? last-press) (> (- (System/currentTimeMillis) last-press) move-delay))
      (do
        (-> game
          (action)
          (assoc-in [:input-delays action-name] (System/currentTimeMillis))))
      game)))

(defn- drop-piece
  [game]
  game)

(defn- handle-input
  [game input]
  (let [game (if (contains? input :move-left)
               (throttle-action game input #(move-piece % [-1 0])) game)
        game (if (contains? input :move-right)
               (throttle-action game input #(move-piece % [1 0])) game)
        game (if (contains? input :rotate-right)
               (throttle-action game input #(rotate-piece-clockwise %)) game)
        fast-drop (:fast-drop game)
        game (if (contains? input :move-down)
               (assoc game :fast-drop true)
               (dissoc game :fast-drop))
        game (if (contains? input :drop-now) (drop-piece game) game)]
    game))

(defn- fall-delay
  [game]
  (let [{:keys [fast-drop fall-delay]} game]
  (if fast-drop
    (* fall-delay fast-fall-factor)
    fall-delay)))

(defn- handle-fall
  [game]
  (let [time-delta (- (System/currentTimeMillis) (:last-fall-time game))
        fall-delay (fall-delay game)]
    (if (> time-delta fall-delay)
      (let [game (move-piece game [0 1])
            game (assoc game :last-fall-time (System/currentTimeMillis))]
        game)
      game)))

(defn start
  [game]
  (assoc game :status :dropping :last-fall-time (System/currentTimeMillis)))

(defn step
  [game input]
  (-> game
    (handle-input input)
    (handle-fall)))

(defn new-game
  "Initializes a new game with an empty board"
  [width height]
  (let [board (board/empty-board width height)]
    {:board board
     :score 0
     :level 1
     :input-delays {}
     :fall-delay 1000  ; milliseconds between piece drop
     :next-piece (pieces/random-piece)
     :current-piece (board/positioned-piece board (pieces/random-piece))
     :status :new}))
