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
  [game current-time]
  (let [game (lock-in-dropping-piece game)
        board (:board game)
        new-piece (board/positioned-piece board (:next-piece game))
        new-ghost (board/dropped-piece board new-piece)]
    (assoc game
           :current-piece new-piece :ghost-piece new-ghost :next-piece (pieces/random-piece)
           :last-fall-time current-time)))

(defn- move-piece
  "Returns a new game with the current piece translated by [x y] spaces,
   if there are no collisions. Returns unchanged game if there are."
  [game translation current-time]
  (let [{ :keys [current-piece board] } game
        [_ dy] translation
        new-piece (nth (board/translated-piece-seq board current-piece translation) 1 nil)]
    (if (nil? new-piece)
      (if (> dy 0)
        (place-new-piece game current-time)
        game)
      (assoc game
             :current-piece new-piece
             :ghost-piece (board/dropped-piece (:board game) new-piece)))))

(defn- throttle-action
  [game action-name action current-time]
  (let [last-press (get-in game [:input-delays action-name])]
    (if (or (nil? last-press) (> (- current-time last-press) move-delay))
      (do
        (-> game
          (action)
          (assoc-in [:input-delays action-name] current-time)))
      game)))

(defn- drop-piece
  [game current-time]
  (let [{ :keys [current-piece board] } game]
    (-> game
      (assoc :current-piece (:ghost-piece game))
      (place-new-piece current-time))))

(defn- handle-input
  [game input current-time]
  (let [game (if (contains? input :move-left)
               (throttle-action game :move-left #(move-piece % [-1 0] current-time) current-time) game)
        game (if (contains? input :move-right)
               (throttle-action game :move-right #(move-piece % [1 0] current-time) current-time) game)
        game (if (contains? input :rotate-right)
               (throttle-action game :rotate-right #(rotate-piece-clockwise %) current-time) game)
        fast-drop (:fast-drop game)
        game (if (contains? input :move-down) (assoc game :fast-drop true) (dissoc game :fast-drop))
        game (if (contains? input :drop-piece)
               (throttle-action game :drop-piece #(drop-piece % current-time) current-time)
               game)]
    game))

(defn- fall-delay
  [game]
  (let [{:keys [fast-drop fall-delay]} game]
  (if fast-drop
    (* fall-delay fast-fall-factor)
    fall-delay)))

(defn- handle-fall
  [game current-time]
  (let [time-delta (- current-time (:last-fall-time game))
        fall-delay (fall-delay game)]
    (if (> time-delta fall-delay)
      (let [game (move-piece game [0 1] current-time)
            game (assoc game :last-fall-time current-time)]
        game)
      game)))

(defn start
  [game current-time]
  (assoc game :status :dropping :last-fall-time current-time))

(defn step
  [game input current-time]
  (-> game
    (handle-input input current-time)
    (handle-fall current-time)))

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
