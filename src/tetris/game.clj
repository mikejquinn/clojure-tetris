(ns tetris.game
  (:require [tetris.colors :as colors]))

(def ^:private tetromino-size 4)

(def move-delay 150)
(def fast-fall-factor (/ 1 10))

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

(defn rotate-piece-counter-clockwise
  [positioned-piece]
  (let [{{ squares :squares size :size } :piece} positioned-piece
        new-squares (map #(do [(% 1) (- size (% 0) 1)]) squares)]
    (assoc-in positioned-piece [:piece :squares] new-squares)))
    
(defn rotate-piece-clockwise
  [positioned-piece]
  (let [{{ squares :squares size :size } :piece} positioned-piece
        new-squares (map #(do [(- size (% 1) 1) (% 0)]) squares)]
    (assoc-in positioned-piece [:piece :squares] new-squares)))

(defn random-piece
  []
  (rand-nth all-pieces))

(defn positioned-piece
  [piece board]
  ;{ :position [(- (/ (:width (:size board)) 2) (/ (:size piece) 2)) 0]
  { :position [0 0]
    :piece piece })

(defn positioned-piece-to-board-coords
  [positioned-piece]
  (let [{{ squares :squares } :piece position :position } positioned-piece]
    (map #(do [(+ (% 0) (position 0)) (+ (% 1) (position 1))]) squares)))

(defn empty-board
  "Create a rectangular tetris board with the specified width and height."
  [width height]
  {:size {:width width :height height}
   :state {}})

(defn piece-in-bounds
  [board blocks]
  (let [{{:keys [width height]} :size} board
        out-of-bounds (filter (fn [block]
                                (let [[x y] block]
                                  (or (< x 0) (>= x width)
                                       (< y 0) (>= y height))))
                              blocks)]
    (empty? out-of-bounds)))

(defn piece-collision-test
  "Tests to see if the passed piece collides with any other elements"
  [board piece]
  (let [{{:keys [width height]} :size} board
        state (:state board)
        piece-board-coords (positioned-piece-to-board-coords piece)
        collisions (filter #(do (contains? state %)) piece-board-coords)]
    (if (empty? collisions)
      (not (piece-in-bounds board piece-board-coords))
      true)))

(defn game-rotate-piece-clockwise
  [game]
  (let [piece (:current-piece game)
        rotated-piece (rotate-piece-clockwise piece)]
    (if (piece-collision-test (:board game) rotated-piece)
      game
      (assoc game :current-piece rotated-piece))))

(defn game-lock-in-dropping-piece
  [game]
  (let [piece (:current-piece game)
        board (:board game)
        {{:keys [color squares]} :piece [origin-x origin-y] :position} piece
        blocks (map (fn [[x y]] [(+ x origin-x) (+ y origin-y)]) squares)
        block-map (reduce #(assoc %1 %2 color) {} blocks)
        new-board-state (merge (:state board) block-map)]
    (assoc-in game [:board :state] new-board-state)))

(defn game-place-new-piece
  [game]
  (-> game
    (game-lock-in-dropping-piece)
    (assoc :current-piece (positioned-piece (:next-piece game) (:board game)))
    (assoc :next-piece (random-piece))))

(defn game-move-piece
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
        (game-place-new-piece game)
        game)
      (do
        (assoc game :current-piece new-piece)))))

(defn- game-throttle-action
  [game action-name action]
  (let [last-press (get-in game [:input-delays action-name])]
    (if (or (nil? last-press) (> (- (System/currentTimeMillis) last-press) move-delay))
      (do
        (-> game
          (action)
          (assoc-in [:input-delays action-name] (System/currentTimeMillis))))
      game)))

(defn- game-drop-piece
  [game]
  game)

(defn- game-handle-input
  [game input]
  (let [game (if (contains? input :move-left) (game-throttle-action game input #(game-move-piece % [-1 0])) game)
        game (if (contains? input :move-right) (game-throttle-action game input #(game-move-piece % [1 0])) game)
        game (if (contains? input :rotate-right) (game-throttle-action game input #(game-rotate-piece-clockwise %)) game)
        fast-drop (:fast-drop game)
        game (if (contains? input :move-down)
               (assoc game :fast-drop true)
               (dissoc game :fast-drop))
        game (if (contains? input :drop-now) (game-drop-piece game) game)]
    game))

(defn- game-fall-delay
  [game]
  (let [{:keys [fast-drop fall-delay]} game]
  (if fast-drop
    (* fall-delay fast-fall-factor)
    fall-delay)))

(defn- game-handle-fall
  [game]
  (let [time-delta (- (System/currentTimeMillis) (:last-fall-time game))
        fall-delay (game-fall-delay game)]
    (if (> time-delta fall-delay)
      (let [game (game-move-piece game [0 1])
            game (assoc game :last-fall-time (System/currentTimeMillis))]
        game)
      game)))

(defn start-game
  [game]
  (assoc game :status :dropping :last-fall-time (System/currentTimeMillis)))

(defn step-game
  [game input]
  (-> game
    (game-handle-input input)
    (game-handle-fall)))

(defn new-game
  "Initializes a new game with an empty board"
  [width height]
  (let [board (empty-board width height)]
    {:board board
     :score 0
     :level 1
     :input-delays {}
     :fall-delay 1000  ; milliseconds between piece drop
     :next-piece (random-piece)
     :current-piece (positioned-piece (random-piece) board)
     :status :new}))

