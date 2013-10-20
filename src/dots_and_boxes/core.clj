(ns dots-and-boxes.core
  (use dots-and-boxes.game)
  (:import
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

(def SQ_WIDTH 50)
(def SQ_HEIGHT 50)
(def INPUT_SIZE 100)
(def H_OFFSET 20)
(def BOARDW 5)
(def BOARDH 5)
(def BG "white")

(def WIDTH (atom nil))
(def HEIGHT (atom nil))
(def MOVE (atom nil))
(def BMOVE (atom nil))

;;;;;;;UI;;;;;;;;;
;;;;;;;rr ["yellow ""green" "red" "black" "yellow"] value 0] (colors (corr value)))
(def colors {"black"  Color/black
             "blue"   Color/blue
             "green"  Color/green
             "yellow" Color/yellow
             "orange" Color/orange
             "pink"   Color/pink
             "red"    Color/red
             "white"   Color/white})




(defn handle-input [#^KeyEvent event]
  ;(reset! MOVE (.getKeyCode event))
  (if (and (>= (.getKeyCode event) 48) (<= (.getKeyCode event) 57))
    (swap! MOVE #(str % (.getKeyChar event)))
    (condp = (.getKeyCode event)
      KeyEvent/VK_LEFT (reset! BMOVE LEFT)
      KeyEvent/VK_RIGHT (reset! BMOVE RIGTH)
      KeyEvent/VK_UP (reset! BMOVE UP)
      KeyEvent/VK_DOWN (reset! BMOVE BOTTOM)))
  )


(defn input-listener []
    (proxy [ActionListener KeyListener] []
          (actionPerformed [e])
          (keyPressed [e] (handle-input e))
          (keyReleased [e])
          (keyTyped [e])))

(defn border-to-color [value] 
  (let [corr ["blue" "green" "red" "black"]]
    (colors (corr value))))

(defn player-to-color [value] 
  (let [corr ["white" "green" "red"]]
    (colors (corr value))))

(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g (.getDrawGraphics buffer)]
    (try
      (draw-fn g)
      
      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw-text [#^Graphics g color text x y]
    (doto g
          (.setColor color)
          (.drawString text x y)))

(defn draw-square [board index  #^Graphics g]
  (let [[x y] (pos-to-xy board index)
        square (get-square board index)
        width SQ_WIDTH
        height SQ_HEIGHT
        xpos (+ H_OFFSET (* x SQ_WIDTH))
        ypos (+ INPUT_SIZE (* y SQ_HEIGHT))
        ]    
    (doto g
      (.setColor (player-to-color (square dots-and-boxes.game/PLAYER)))
      (.fillRect xpos ypos width height)
      ;UP
      (.setColor (border-to-color (square dots-and-boxes.game/UP)))
      (.drawLine xpos ypos (+ xpos SQ_WIDTH) ypos)
      ;LEFT
      (.setColor (border-to-color (square dots-and-boxes.game/LEFT)))
      (.drawLine xpos ypos xpos (+ ypos SQ_HEIGHT))
      ;BOTTOM
      (.setColor (border-to-color (square dots-and-boxes.game/BOTTOM)))
      (.drawLine xpos (+ ypos SQ_HEIGHT) (+ xpos SQ_WIDTH) (+ ypos SQ_HEIGHT))
      ;RIGTH
      (.setColor (border-to-color (square dots-and-boxes.game/RIGTH)))
      (.drawLine (+ xpos SQ_WIDTH) ypos (+ xpos SQ_WIDTH) (+ ypos SQ_HEIGHT)))
    (draw-text g Color/black (str index) (+ xpos (/ SQ_WIDTH 2)) (+ ypos (/ SQ_HEIGHT 2)))
    ))


(defn draw-board [board]
  (fn [#^Graphics g]
    (doto g
      (.setColor (colors BG))
      (.fillRect 0 0 @WIDTH @HEIGHT))
    
    (doseq [square (range (board :num-squares))]
      (draw-square board square g))
    
    (draw-text g Color/black (str "player: " (board :player)) 20 20)
    (draw-text g Color/black (str "move: " @MOVE) 20 40)
    (draw-text g Color/black (str "socre: " (get-score board 1) " / " (get-score board 2)) 20 60)
    
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (reset! WIDTH (* (+ BOARDW 1) SQ_WIDTH))
  (reset! HEIGHT (+ INPUT_SIZE 100 (* BOARDH SQ_HEIGHT)))
  (reset! MOVE nil)
  (reset! BMOVE nil)

  (let [first-board (init-board BOARDW BOARDH)
        frame (javax.swing.JFrame. "Timbiriche")
        canvas (Canvas.)]
    (doto frame
      (.setSize @WIDTH @HEIGHT)
      ;(.setDefaultCloseOperation java.swingt.Frame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
    (doto canvas
      (.createBufferStrategy 2)
      (.addKeyListener (input-listener))
      (.setVisible true)
      (.requestFocus))
    
    
  (loop [board first-board]
    (Thread/sleep 100)
    (draw canvas (draw-board board))
    (if (not (complete? board))
      (if-let [[x y] (if (and @MOVE @BMOVE) (pos-to-xy board (. Integer parseInt @MOVE))  nil)]
        (let [new-board (draw-line board {:x x, :y y, :position @BMOVE })]
          (do
            (reset! BMOVE nil)
            (reset! MOVE nil )
            (recur new-board)))
        (recur board)))))
  
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


