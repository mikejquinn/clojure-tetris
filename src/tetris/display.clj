(ns tetris.display
  (:require [tetris.colors :as colors]
            [tetris.game :as game])
  (:import (java.awt Dimension Canvas)
           (java.awt.event KeyListener KeyEvent)
           (javax.swing JFrame JPanel)))

(def footer-height 10)
(def header-height 0)
(def margin-left 20)
(def margin-right 20)
(def board-outline-width 8)
(def board-outline-color colors/yellow)
(def block-padding 1)
(def block-width 20)
(def padded-block (+ block-width block-padding))

(defn- game-screen-size [board]
  (let [{{:keys [width height]} :size} board]
    {:width (+ margin-left margin-right
              (* 2 board-outline-width)
              (* width (+ block-width block-padding)) block-padding)
     :height (+ header-height footer-height
                board-outline-width
                (* height (+ block-width block-padding)))}))

(defn- draw-walls [g board]
  (let [{{:keys [width height]} :size} board
        x1 margin-left
        y1 header-height
        x2 (+ x1
              block-padding
              (* 2 board-outline-width)
              (* width padded-block))
        y2 (+ y1 (* height padded-block) board-outline-width)
        side-bar-height (- y2 y1)
        bottom-bar-width (- x2 x1)]
    (.setColor g board-outline-color)
    (.fillRect g x1 y1 board-outline-width side-bar-height)
    (.fillRect g (- x2 board-outline-width) y1 board-outline-width side-bar-height)
    (.fillRect g x1 (- y2 board-outline-width) bottom-bar-width board-outline-width)))

(defn- draw-board [g dimensions board]
  "Draws the Tetris game board."
  (let [{:keys [width height]} dimensions]
    (.setColor g colors/black)
    (.fillRect g 0 0 width height)
    (draw-walls g board)))

(defn- translate-coords [[x y]]
  "Translates a tetris grid coordinate into a pixel coordinate for drawing"
  (let [grid-x (+ margin-left board-outline-width block-padding)
        grid-y header-height
        x-px (+ grid-x (* x padded-block))
        y-px (+ grid-y (* y padded-block))]
    [x-px y-px]))

(defn- draw-square [g square]
  (let [[x y] square
        [pixel-x pixel-y] (translate-coords [x y])]
    (.fillRect g pixel-x pixel-y block-width block-width)))

(defn- piece-to-board-coords
  [positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        [origin-x origin-y] position
        squares (:squares piece)]
    (map (fn [[x y]] [(+ x origin-x) (+ y origin-y)]) squares)))

(defn- draw-positioned-piece
  [g positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        color (:color piece)
        squares (piece-to-board-coords positioned-piece)]
    (.setColor g color)
    (doseq [square squares]
      (draw-square g square))))

(defn- draw-fallen-blocks
  [g blocks]
  (doseq [[square color] blocks]
    (.setColor g color)
    (draw-square g square)))

(defn- paint-game
  [game graphics]
  (let [board (:board game)
        dimensions (game-screen-size board)]
    (draw-board graphics dimensions board)
    (draw-positioned-piece graphics (:current-piece game))
    (draw-fallen-blocks graphics (:state board))))

(defn- observe-frame
  [{ :keys [count last-second] }]
  (let [now (.getTime (new java.util.Date))
        time-elapsed (- now last-second)]
    (if (> time-elapsed 1000)
      (do
        ;(println now " - Frame rate: " count " frames/sec   (elapsed " time-elapsed ")")
        { :count 0 :last-second now })
      { :count (+ count 1) :last-second last-second })))

(defn- create-frame
  [width-px height-px]
  (let [frame (JFrame. "Tetris")]
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.setIgnoreRepaint true))
    frame))

(def command-key-map
  {KeyEvent/VK_LEFT  :move-left
   KeyEvent/VK_RIGHT :move-right
   KeyEvent/VK_DOWN  :move-down
   KeyEvent/VK_UP    :rotate-right})

(defn- create-canvas [width-px height-px key-code-atom]
  (let [key-listener (proxy [KeyListener][]
                       (keyPressed [e]
                         (if-let [command (command-key-map (.getKeyCode e))]
                           (compare-and-set! key-code-atom @key-code-atom (conj @key-code-atom command))))
                       (keyReleased [e]
                         (if-let [command (command-key-map (.getKeyCode e))]
                           (compare-and-set! key-code-atom @key-code-atom (disj @key-code-atom command))))
                       (keyTyped [e]))
        canvas (Canvas.)]
    (doto canvas
      (.setBounds 0 0 width-px height-px)
      (.setIgnoreRepaint true)
      (.addKeyListener key-listener))
    canvas))

(defn tetris-swing []
  "Starts a tetris game drawn using Java Swing"
  (let [game-width 12
        game-height 22
        game (game/new-game game-width game-height)
        board (:board game)
        { width-px :width height-px :height } (game-screen-size board)
        frame-counter { :count 0 :last-second (.getTime (new java.util.Date)) }
        key-code-atom (atom #{})
        frame (create-frame width-px height-px)
        panel (.getContentPane frame)
        canvas (create-canvas width-px height-px key-code-atom)]
    (doto panel
      (.setPreferredSize (Dimension. width-px height-px))
      (.setLayout nil)
      (.add canvas))
    (doto frame
      (.setResizable false)
      (.setVisible true)
      (.pack))
    (doto canvas
      (.createBufferStrategy 2)
      (.requestFocus))
    (loop [game (game/start-game game)
           frame-counter frame-counter]
      (let [game (game/step-game game @key-code-atom)
            frame-counter (observe-frame frame-counter)
            buffer-strategy (.getBufferStrategy canvas)
            graphics (.getDrawGraphics buffer-strategy)]
          (paint-game game graphics)
          (.dispose graphics)
          (.show buffer-strategy)
          (Thread/sleep 10)
          (recur game frame-counter)))
    ))


