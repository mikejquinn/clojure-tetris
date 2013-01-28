(ns tetris.game
  (:require [tetris.pieces :as pieces]
            [tetris.board :as board])
  (:use clojure.contrib.math))

(def ^:private move-delay 150)
(def ^:private fast-fall-factor (/ 1 10))
(def ^:private rows-per-level 10)
(def ^:private speed-increase-factor (/ 9 10))

(defn- rotate-piece-clockwise
  [game]
  (let [piece (:current-piece game)
        rotated-piece (board/rotate-piece-clockwise piece)]
    (if (board/piece-collision-test (:board game) rotated-piece)
      game
      (let [new-ghost (board/dropped-piece (:board game) rotated-piece)]
        (assoc game :current-piece rotated-piece :ghost-piece new-ghost)))))

(defn- lock-in-current-piece
  "Converts the currently falling piece (which is described by an origin point
   and a set of offsets from that point) into real board coordinates and saves them
   them in the board state. We no longer care that the blocks that comprised this
   piece were ever linked, so they're stored separately in the board along with
   their color."
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
  (let [board (:board game)
        piece-bag (:piece-bag game)
        new-piece (board/positioned-piece board (first piece-bag))
        new-ghost (board/dropped-piece board new-piece)
        game-is-over (or (board/piece-collision-test board new-piece)
                         (board/piece-collision-test board (board/translate-positioned-piece new-piece [0 1])))
        new-status (if game-is-over :over :dropping)]
    (if game-is-over
      (assoc game :status :over)
      (assoc game
             :current-piece new-piece
             :ghost-piece new-ghost
             :piece-bag (rest piece-bag)
             :last-fall-time current-time))))

(defn- score-for-rows
  "Given a level and the number of lines that were removed at one time, returns
  the score the player has earned.
  http://tetris.wikia.com/wiki/Scoring"
  [level lines]
  (let [multiplier (case lines
                     1 40
                     2 100
                     3 300
                     4 1200
                     0)]
    (* multiplier level)))

(defn- clear-completed-rows
  [game]
  (let [{:keys [board removed]} (board/remove-filled-rows (:board game))
        lines (+ (:lines game) removed)
        score (+ (:score game) (score-for-rows (:level game) removed))
        level-up (> lines (* rows-per-level (:level game)))
        level (if level-up (+ (:level game) 1) (:level game))
        fall-delay (if level-up
                     (* (:fall-delay game) speed-increase-factor)
                     (:fall-delay game))]
    (assoc game :board board :lines lines :score score 
           :level level :fall-delay fall-delay)))

(defn- finalize-drop
  "Completes the current drop, and sets up a game with a new piece."
  [game current-time]
  (-> game
    (lock-in-current-piece)
    (clear-completed-rows)
    (place-new-piece current-time)))

(defn- move-piece
  "Returns a new game with the current piece translated by [x y] spaces,
   if there are no collisions. Returns unchanged game if there are."
  [game translation current-time]
  (let [{ :keys [current-piece board] } game
        [_ dy] translation
        new-piece (nth (board/translated-piece-seq board current-piece translation) 1 nil)]
    (if (nil? new-piece)
      (if (> dy 0)
        (finalize-drop game current-time)
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
      (finalize-drop current-time))))

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
  (if (= (:status game) :dropping)
    (-> game
      (handle-input input current-time)
      (handle-fall current-time))
    game))

(defn new-game
  "Initializes a new game with an empty board"
  ([width height] (new-game width height 1))
  ([width height level]
   (let [board (board/empty-board width height)
         piece-bag (pieces/random-piece-generator)
         first-piece (first piece-bag)
         piece-bag (rest piece-bag)
         current-piece (board/positioned-piece board first-piece)
         fall-delay (* 1000 (expt speed-increase-factor (- level 1)))]
     {:board board
      :score 0
      :level level
      :lines 0
      :input-delays {}
      :fall-delay fall-delay
      :piece-bag piece-bag
      :current-piece current-piece
      :ghost-piece (board/dropped-piece board current-piece)
      :status :new})))
