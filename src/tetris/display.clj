(ns tetris.display
  (:require [tetris.colors :as colors]
            [tetris.game :as game])
  (:import (java.awt Dimension Canvas Font Component)
           (java.awt.event KeyListener KeyEvent)
           (javax.swing JFrame JPanel JLabel Box BoxLayout BorderFactory))
  (:use clojure.contrib.math))

(def ^:private game-width 12)
(def ^:private game-height 22)
(def ^:private wall-thickness 8)
(def ^:private board-outline-color colors/yellow)
(def ^:private block-padding 1)
(def ^:private block-width 20)
(def ^:private padded-block (+ block-width block-padding))
(def ^:private overflow 2)

(defn- inset-px-rect
  "Returns the square that has been inset by a specified number of pixels"
  [rect inset]
  (let [{ { :keys [width height] } :size [x y] :origin } rect]
    { :origin [(+ x inset) (+ y inset)]
      :size { :width (- width (* 2 inset))
              :height (- height (* 2 inset))}}))

(defn- translate-coords
  "Translates a tetris grid coordinate into a pixel coordinate for drawing."
  ([[x y]]
   (let [x-px (* x padded-block)
         y-px (* y padded-block)]
     [x-px y-px])))

(defn- piece-to-board-coords
  "A positioned piece is defined by an offset, and a set of squares positioned
  relative to that offset. This function returns the individual squares positioned
  relative to the board."
  [positioned-piece]
  (let [{:keys [position piece]} positioned-piece
        [origin-x origin-y] position
        squares (:squares piece)]
    (map (fn [[x y]] [(+ x origin-x) (+ y origin-y)]) squares)))

(defn- draw-filled-square
  ([g square]
   (let [[x y] square
         [pixel-x pixel-y] (translate-coords [x y])]
     (.fillRect g pixel-x pixel-y block-width block-width))))

(defn- draw-outline-square
  [g square]
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

(defn- draw-stats
  [game stats-panel]
  (let [{ :keys [score-label level-label next-piece-canvas] } stats-panel
        buffer-strategy (.getBufferStrategy next-piece-canvas)
        graphics (.getDrawGraphics buffer-strategy)
        next-piece (first (:piece-bag game))]
    (doto graphics
      (.setColor colors/black)
      (.fillRect 0 0 (.getWidth next-piece-canvas) (.getHeight next-piece-canvas))
      (.setColor (:color next-piece)))
    (doseq [square (:squares next-piece)]
      (draw-filled-square graphics square))
    (.setText (:lines-label stats-panel) (.toString (:lines game)))
    (.setText (:score-label stats-panel) (.toString (:score game)))
    (.setText (:level-label stats-panel) (.toString (:level game)))
    (.dispose graphics)
    (.show buffer-strategy)))

(defn- draw-game
  [game canvas]
  (let [buffer-strategy (.getBufferStrategy canvas)
        graphics (.getDrawGraphics buffer-strategy)
        board (:board game)]
    (.setColor graphics colors/black)
    (.fillRect graphics 0 0 (.getWidth canvas) (.getHeight canvas))
    (draw-positioned-piece graphics (:current-piece game))
    (draw-ghost-piece graphics (:ghost-piece game))
    (draw-fallen-blocks graphics (:state board))
    (.dispose graphics)
    (.show buffer-strategy)))

(def ^:private command-key-map
  {KeyEvent/VK_LEFT  :move-left
   KeyEvent/VK_RIGHT :move-right
   KeyEvent/VK_DOWN  :move-down
   KeyEvent/VK_UP    :rotate-right
   KeyEvent/VK_SPACE :drop-piece})

(defn- create-key-listener
  [key-code-atom]
  (proxy [KeyListener][]
    (keyPressed [e]
      (if-let [command (command-key-map (.getKeyCode e))]
        (compare-and-set! key-code-atom @key-code-atom
                          (conj @key-code-atom command))))
    (keyReleased [e]
      (if-let [command (command-key-map (.getKeyCode e))]
        (compare-and-set! key-code-atom @key-code-atom
                          (disj @key-code-atom command))))
    (keyTyped [e])))

(defn- create-frame
  []
  (let [frame (JFrame. "Tetris")
        panel (.getContentPane frame)]
    (.setBackground panel colors/black)
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.setIgnoreRepaint true))))

(defn- create-tetris-canvas
  [width height]
  (let [canvas (Canvas.)
        width-px (- (* width padded-block) block-padding)
        height-px (- (* height padded-block) block-padding)]
    (doto canvas
      (.setBounds 0 0 width-px height-px)
      (.setIgnoreRepaint true))))

(defn- create-board-panel
  [board canvas]
  (let [canvas-width (.getWidth canvas)
        canvas-height (.getHeight canvas)
        width-px (+ (* wall-thickness 2) (* block-padding 2) canvas-width)
        height-px (+ wall-thickness canvas-height block-padding)
        wall-origin-y (* padded-block overflow)
        wall-height (- height-px wall-origin-y)
        panel (proxy [JPanel][]
                (getPreferredSize [] (Dimension. width-px height-px))
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (.setColor g board-outline-color)
                  (.fillRect g 0 wall-origin-y wall-thickness wall-height)
                  (.fillRect g 0 (- height-px wall-thickness) width-px wall-thickness)
                  (.fillRect g (- width-px wall-thickness) wall-origin-y wall-thickness wall-height)))]
    (.setBounds canvas (+ wall-thickness block-padding) 0 canvas-width canvas-height)
    (doto panel
      (.setBackground colors/black)
      (.setLayout nil)
      (.add canvas))))

(defn- create-title-label
  [text]
  (let [label (JLabel. text)]
    (doto label
      (.setAlignmentX Component/LEFT_ALIGNMENT)
      (.setFont (Font. "Sans Serif" Font/PLAIN 22))
      (.setForeground colors/white))))

(defn- create-value-label
  [text]
  (let [label (JLabel. text)]
    (doto label
      (.setFont (Font. "Sans Serif" Font/PLAIN 22))
      (.setForeground colors/white))))

(defn- create-key-and-value-row
  "Builds two labels, set horizontally in a frame, with the
  value label positioned on the right."
  [title value]
  (let [panel (JPanel.)
        title-label (create-title-label title)
        value-label (create-value-label value)]
    (.setAlignmentX title-label Component/LEFT_ALIGNMENT)
    (.setAlignmentX value-label Component/RIGHT_ALIGNMENT)
    (doto panel
      (.setAlignmentX Component/LEFT_ALIGNMENT)
      (.setLayout (BoxLayout. panel BoxLayout/X_AXIS))
      (.setBackground colors/black)
      (.add title-label)
      (.add (Box/createRigidArea (Dimension. 5 0)))
      (.add (Box/createHorizontalGlue))
      (.add value-label))
    {:container panel
     :value-label value-label}))

(defn- create-stats-panel
  []
  (let [panel (JPanel.)
        score-row (create-key-and-value-row "Score:" "0")
        lines-row (create-key-and-value-row "Lines:" "0")
        level-row (create-key-and-value-row "Level:" "1")
        next-piece-canvas (create-tetris-canvas 4 4)
        next-piece-panel (JPanel.)]
    (doto next-piece-panel
      (.setLayout nil)
      (.setBorder (BorderFactory/createEmptyBorder 10 130 0 0))
      (.setBackground colors/black)
      (.add next-piece-canvas)
      (.setPreferredSize (Dimension. (.getWidth next-piece-canvas)
                                     (.getHeight next-piece-canvas))))
    (doto panel
      (.setLayout (BoxLayout. panel BoxLayout/Y_AXIS))
      (.setBackground colors/black)
      (.add (Box/createRigidArea (Dimension. 0 (* padded-block overflow))))
      (.add (:container score-row))
      (.add (:container lines-row))
      (.add (:container level-row))
      (.add (Box/createRigidArea (Dimension. 0 30)))
      (.add (create-title-label "Next Piece:"))
      (.add (Box/createRigidArea (Dimension. 0 15)))
      (.add next-piece-panel))

    {:score-label (:value-label score-row)
     :lines-label (:value-label lines-row)
     :level-label (:value-label level-row)
     :next-piece-canvas next-piece-canvas
     :container panel }
    ))

(defn- add-game-over-message
  [panel]
  (let [label (JLabel. "Game Over")]
    (doto label
      (.setFont (Font. "Sans Serif" Font/PLAIN 32))
      (.setBackground colors/black)
      (.setOpaque true)
      (.setForeground colors/white))
    (let [label-width (.getWidth (.getPreferredSize label))
          label-height (.getHeight (.getPreferredSize label))
          label-x (+ (floor (- (/ (.getWidth panel) 2) 
                               (/ label-width 2))))
          label-y (+ (floor (- (/ (.getHeight panel) 2)
                               (/ label-height 2))))]
      (.setBounds label label-x label-y label-width label-height)
      (doto panel
        (.add label 0)
        (.setComponentZOrder label 0)
        (.validate)
        (.repaint)))))

(defn tetris-swing
  "Starts a tetris game drawn using Java Swing"
  ([] (tetris-swing 1))
  ([level]
   (let [game (game/new-game game-width game-height level)
         board (:board game)
         {{ board-width :width board-height :height } :size} board
         frame-counter { :count 0 :last-second (.getTime (new java.util.Date)) }
         key-code-atom (atom #{})
         frame (create-frame)
         canvas (create-tetris-canvas board-width board-height)
         board-panel (create-board-panel board canvas)
         stats-panel (create-stats-panel)
         content-panel (.getContentPane frame)]
     (doto content-panel
       (.setLayout (BoxLayout. content-panel BoxLayout/X_AXIS))
       (.setBorder (BorderFactory/createEmptyBorder 20 20 20 20))
       (.add board-panel)
       (.add (Box/createRigidArea (Dimension. 20 0)))
       (.add (:container stats-panel)))
     (doto frame
       (.setVisible true)
       (.addKeyListener (create-key-listener key-code-atom))
       (.requestFocus)
       (.pack))
     (.createBufferStrategy canvas 2)
     (.createBufferStrategy (:next-piece-canvas stats-panel) 2)
     (loop [game (game/start game (System/currentTimeMillis))]
       (let [current-time (System/currentTimeMillis)
             game (game/step game @key-code-atom current-time)]
         (draw-game game canvas)
         (draw-stats game stats-panel)
         (Thread/sleep 10)
         (if (= (:status game) :dropping)
           (recur game))))
     (add-game-over-message board-panel)
     (loop []
       (Thread/sleep 10)
       (recur)))))


