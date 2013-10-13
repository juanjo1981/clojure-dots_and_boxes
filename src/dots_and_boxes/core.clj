(ns dots-and-boxes.core
  (use dots-and-boxes.game)
  (:import
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

(def SQ_WIDTH 100)
(def SQ_HEIGHT 100)
(def INPUT_SIZE 200)

(def WIDTH (atom nil))
(def HEIGHT (atom nil))

;;;;;;;UI;;;;;;;;;
(def colors {"black"  Color/black
             "blue"   Color/blue
             "green"  Color/green
             "yellow" Color/yellow
             "orange" Color/orange
             "pink"   Color/pink
             "red"    Color/red})

(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g (.getDrawGraphics buffer)]
    (try
      (draw-fn g)
      
      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw-square [x y  #^Graphics g]
  (let [width SQ_WIDTH
        height SQ_HEIGHT
        xpos (* x SQ_WIDTH)
        ypos (* y SQ_HEIGHT)]    
    (doto g
      (.setColor Color/blue)
      (.fillRect xpos ypos width height)
      (.setColor Color/green)
      ;UP
      (.drawLine xpos ypos (+ xpos SQ_WIDTH) ypos)
      ;LEFT
      (.drawLine xpos ypos xpos (+ ypos SQ_HEIGHT))
      ;DOWN
      (.drawLine xpos (+ ypos SQ_HEIGHT) (+ xpos SQ_WIDTH) (+ ypos SQ_HEIGHT))
      ;RIGTH
      (.drawLine (+ xpos SQ_WIDTH) ypos (+ xpos SQ_WIDTH) (+ ypos SQ_HEIGHT)))))


(defn draw-board [board]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 @WIDTH @HEIGHT))
    (draw-square 0 0 g) 
    
    (doseq [square (range (board :num-squares))]
      (let [[x y] (pos-to-xy board square)]
        (draw-square x y g)))
    
    ;(doseq [square (range (count board))]
    ;  (let [[x y] (pos-to-xy square)]
    ;    (draw-square x y (get board square) g)))
    
    ;(doseq [[x y] (:shape block)]
    ;  (draw-square x y (:color block) g))
    
    ;(draw-text g Color/green (str "score: " score) 20 25)
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (reset! WIDTH (* 5 SQ_WIDTH))
  (reset! HEIGHT (+ INPUT_SIZE (* 4 SQ_HEIGHT)))
  (let [first-board (init-board 4 4)
        frame (java.awt.Frame. "Timbiriche")
        canvas (Canvas.)]
    (doto frame
      (.setSize @WIDTH @HEIGHT)
      ;(.setDefaultCloseOperation java.swingt.Frame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
    
    (doto canvas
      (.createBufferStrategy 2)
      ;(.addKeyListener (input-listener))
      (.setVisible true)
      (.requestFocus))
    (loop [board first-board]
      (Thread/sleep 10)
      (draw canvas (draw-board board))
      (if true
        1
        (recur board))))

  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
