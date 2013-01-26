(ns tetris.display
  (:require [tetris.colors :as colors]
            [tetris.game :as game])
  (:import (java.awt Dimension Canvas Font)
           (java.awt.event KeyListener KeyEvent)
           (javax.swing JFrame JPanel JLabel))
  (:use clojure.contrib.math))

(def ^:private game-width 12)
(def ^:private game-height 22)
(def ^:private footer-height 20)
(def ^:private header-height 20)
(def ^:private board-outline-width 8)
(def ^:private board-outline-color colors/yellow)
(def ^:private block-padding 1)
(def ^:private block-width 20)
(def ^:private overflow 2)
(def ^:private padded-block (+ block-width block-padding))
(def ^:private margin-left 30)
(def ^:private margin-right (* padded-block 10))
(def ^:private board-origin
   [(+ margin-left board-outline-width block-padding) header-height])

(defn- next-piece-origin
  [screen-size]
  [(+ (* padded-block 2)
      (- (:width screen-size) margin-right))
   (* padded-block 4)])

(defn- game-screen-size [board]
  (let [{{:keys [width height]} :size} board]
    {:width (+ margin-left margin-right
              (* 2 board-outline-width)
              (* width (+ block-width block-padding)) block-padding)
     :height (+ header-height footer-height
                board-outline-width
                (* height (+ block-width block-padding)))}))

(defn- inset-px-rect
  "Returns the square that has been inset by a specified number of pixels"
  [rect inset]
  (let [{ { :keys [width height] } :size [x y] :origin } rect]
    { :origin [(+ x inset) (+ y inset)]
      :size { :width (- width (* 2 inset))
              :height (- height (* 2 inset))}}))

(defn- translate-coords
  "Translates a tetris grid coordinate into a pixel coordinate for drawing.
  Optionally takes the opper left pixel coordinate - but this defaults to
  the top left of the game board."
  ([[x y]] (translate-coords board-origin [x y]))
  ([[grid-x grid-y] [x y]]
   (let [x-px (+ grid-x (* x padded-block))
         y-px (+ grid-y (* y padded-block))]
     [x-px y-px])))

(defn- piece-to-board-coords
  [positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        [origin-x origin-y] position
        squares (:squares piece)]
    (map (fn [[x y]] [(+ x origin-x) (+ y origin-y)]) squares)))

(defn- draw-walls [g board]
  (let [{{:keys [width height]} :size} board
        x1 margin-left
        y1 (+ header-height (* overflow padded-block))
        x2 (+ x1
              block-padding
              (* 2 board-outline-width)
              (* width padded-block))
        y2 (+ y1 (* (- height overflow) padded-block) board-outline-width)
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

(defn- draw-filled-square
  ([g square] (draw-filled-square g square board-origin))
  ([g square origin]
   (let [[x y] square
         [pixel-x pixel-y] (translate-coords origin [x y])]
     (.fillRect g pixel-x pixel-y block-width block-width))))

(defn- draw-outline-square [g square]
  (let [[x y] square
        origin-px (translate-coords [x y])
        rect-px { :origin origin-px :size { :width block-width :height block-width } }
        rect-px (inset-px-rect rect-px 1)
        [pixel-x pixel-y] (:origin rect-px)
        { :keys [width height] } (:size rect-px)]
    (.drawRect g pixel-x pixel-y width height)))

(defn- draw-positioned-piece
  [g positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        color (:color piece)
        squares (piece-to-board-coords positioned-piece)]
    (.setColor g color)
    (doseq [square squares]
      (draw-filled-square g square))))

(defn- draw-next-piece
  [g piece origin]
  (let [{ :keys [color squares] } piece
        text (char-array "Next piece:")
        [origin-x origin-y] origin]
    (.setColor g color)
    (doseq [square squares]
      (draw-filled-square g square origin))
    (.setColor g colors/white)
    (.drawChars g text 0 (count text) (- origin-x 10) (- origin-y 15))
    ))

(defn- draw-ghost-piece
  [g positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        color (:color piece)
        squares (piece-to-board-coords positioned-piece)]
    (.setColor g color)
    (doseq [square squares]
      (draw-outline-square g square))))

(defn- draw-fallen-blocks
  [g blocks]
  (doseq [[square color] blocks]
    (.setColor g color)
    (draw-filled-square g square)))

(defn- draw-game
  [g game]
  (let [board (:board game)
        screen-size (game-screen-size board)
        dimensions screen-size
        next-origin (next-piece-origin screen-size)]
    (draw-board g dimensions board)
    (draw-ghost-piece g (:ghost-piece game))
    (draw-positioned-piece g (:current-piece game))
    (draw-next-piece g (first (:piece-bag game)) next-origin)
    (draw-fallen-blocks g (:state board))))

(defn- observe-frame
  [{ :keys [count last-second] }]
  (let [now (.getTime (new java.util.Date))
        time-elapsed (- now last-second)]
    (if (> time-elapsed 1000)
      { :count 0 :last-second now }
      { :count (+ count 1) :last-second last-second })))

(defn- create-label
  [text]
  (let [label (JLabel. text)]
    (.setFont label (Font. "Sans Serif" Font/PLAIN 22))
    (let [size (.getPreferredSize label)]
      (doto label
        (.setBounds 0 0 (.width size) (.height size))
        (.setOpaque true)
        (.setBackground colors/black)
        (.setForeground colors/white)))))

(defn- create-frame
  [width-px height-px]
  (let [frame (JFrame. "Tetris")]
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.setIgnoreRepaint true))
    frame))

(defn- add-game-over-message
  [panel]
  (let [label (create-label "Game Over")
        board-width-px (+ (* game-width padded-block) block-padding)
        board-height-px (+ (* game-height padded-block) block-padding)
        board-origin-x (+ margin-left board-outline-width)
        board-origin-y header-height
        label-width (.getWidth label)
        label-height (.getHeight label)
        label-x (+ board-origin-x
                   (floor (- (/ board-width-px 2) 
                             (/ label-width 2))))
        label-y (+ board-origin-y
                   (floor (- (/ board-height-px 2)
                             (/ label-height 2))))]
    (.setBounds label label-x label-y label-width label-height)
    (doto panel
      (.add label 0)
      (.setComponentZOrder label 0)
      (.validate)
      (.repaint))))


(def ^:private command-key-map
  {KeyEvent/VK_LEFT  :move-left
   KeyEvent/VK_RIGHT :move-right
   KeyEvent/VK_DOWN  :move-down
   KeyEvent/VK_UP    :rotate-right
   KeyEvent/VK_SPACE :drop-piece})

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
  (let [game (game/new-game game-width game-height)
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
      (.setVisible true)
      (.pack))
    (doto canvas
      (.createBufferStrategy 2)
      (.requestFocus))
    (loop [game (game/start game (System/currentTimeMillis))
           frame-counter frame-counter]
      (let [current-time (System/currentTimeMillis)
            game (game/step game @key-code-atom current-time)
            frame-counter (observe-frame frame-counter)
            buffer-strategy (.getBufferStrategy canvas)
            graphics (.getDrawGraphics buffer-strategy)]
          (draw-game graphics game)
          (.dispose graphics)
          (.show buffer-strategy)
          (Thread/sleep 10)
          (if (= (:status game) :dropping)
            (recur game frame-counter))))
    (add-game-over-message panel)
    (loop []
      (Thread/sleep 10)
      (recur))
    ))


