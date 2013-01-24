(ns tetris.game
  (:require [tetris.pieces :as pieces]
            [tetris.board :as board]))

(def ^:private move-delay 150)
(def ^:private fast-fall-factor (/ 1 10))

(defn- rotate-piece-clockwise
  [game]
  (let [piece (:current-piece game)
        rotated-piece (board/rotate-piece-clockwise piece)]
    (if (board/piece-collision-test (:board game) rotated-piece)
      game
      (let [new-ghost (board/dropped-piece (:board game) rotated-piece)]
        (assoc game :current-piece rotated-piece :ghost-piece new-ghost)))))

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
  (let [board (:board game)
        new-piece (board/positioned-piece board (:next-piece game))
        new-ghost (board/dropped-piece board new-piece)]
    (-> game
      (lock-in-dropping-piece)
      (assoc :current-piece new-piece :ghost-piece new-ghost :next-piece (pieces/random-piece)
             :last-fall-time (System/currentTimeMillis)))))

(defn- move-piece
  "Returns a new game with the current piece translated by [x y] spaces,
   if there are no collisions. Returns unchanged game if there are."
  [game translation]
  (let [{ :keys [current-piece board] } game
        [_ dy] translation
        new-piece (nth (board/translated-piece-seq board current-piece translation) 1 nil)]
    (if (nil? new-piece)
      (if (> dy 0)
        (place-new-piece game)
        game)
      (assoc game
             :current-piece new-piece
             :ghost-piece (board/dropped-piece (:board game) new-piece)))))

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
  (let [{ :keys [current-piece board] } game
        translation [0 1]
        dropped-piece (board/dropped-piece board current-piece)]
    (-> game
      (assoc :current-piece dropped-piece)
      (place-new-piece))))

(defn- handle-input
  [game input]
  (let [game (if (contains? input :move-left)
               (throttle-action game :move-left #(move-piece % [-1 0])) game)
        game (if (contains? input :move-right)
               (throttle-action game :move-right #(move-piece % [1 0])) game)
        game (if (contains? input :rotate-right)
               (throttle-action game :rotate-right #(rotate-piece-clockwise %)) game)
        fast-drop (:fast-drop game)
        game (if (contains? input :move-down) (assoc game :fast-drop true) (dissoc game :fast-drop))
        game (if (contains? input :drop-piece)
               (throttle-action game :drop-piece #(drop-piece %))
               game)]
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
  (let [board (board/empty-board width height)
        current-piece (board/positioned-piece board (pieces/random-piece))]
    {:board board
     :score 0
     :level 1
     :input-delays {}
     :fall-delay 1000  ; milliseconds between piece drop
     :next-piece (pieces/random-piece)
     :current-piece current-piece
     :ghost-piece (board/dropped-piece board current-piece)
     :status :new}))
