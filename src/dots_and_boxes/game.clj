(ns dots-and-boxes.game
  (use clojure.walk))

(def UP 0)
(def RIGTH 1)
(def BOTTOM 2)
(def LEFT 3)
(def SQ_SIZE 4)

(defn init-board
  "TODO: HELP"
  [x y]
  {:pre [(> x 0) (> y 0)]
   ;:post [(vector %) (= (count %) (+ (* y x 4) (* y x)))]
   }
  (let [set-border
        #(vector 
           (if (= %2 0) -1 0)
           (if (= %1 (- x 1)) -1 0)
           (if (= %2 (- y 1)) -1 0)
           (if (= %1 0) -1 0))]
       (loop [cols 0 rows 0 
              result []]
         (if (= rows y)
           (hash-map
             :cols x,
             :rows y,
             :num-squares (* x y)
             :player 1,
             :squares (into [] (take (* x y) (repeat 0)))
             :borders result)
           (recur (if (< cols (- x 1)) (inc cols) 0)
                  (if (= cols (- x 1)) (inc rows) rows)
                  (into result (set-border cols rows)))))))

(defn get-index
  ""
  [board move]
  (let [square-index (+ (move :x) (* (move :y) (board :cols)))
        border-index (+ (* (move :x) SQ_SIZE) (* (move :y) (board :cols) SQ_SIZE) (move :position))]
    (hash-map :square square-index, :border border-index)))

(defn set-index 
  "TODO: HELP"
  [board move player]
  (let [index     (get-index board move)
        borders   (assoc (board :borders) (index :border) player)
        complete  (cond (>= (reduce + (subvec borders (index :border) (+ (index :border) 4))) (* player 4)) true
                       :else false)]
    (assoc board
      :borders borders,
      :squares (if complete (assoc (board :squares) (index :square) player) (board :squares)))))

(defn get-complement-move
  "TODO: HELP"
  [board move]
  (let [pos   (move :position)
        cols  (board :cols)
        rows  (board :rows)
        x     (move :x)
        y     (move :y)]
  (cond 
    (and (= pos UP) (> y 0)) (hash-map :x x, :y (- 1 y), :position BOTTOM)  
    (and (= pos RIGTH) (< x (- cols 1))) (hash-map :x (+ 1 x), :y y, :position LEFT)
    (and (= pos BOTTOM) (< y (- rows 1)))  (hash-map :x x, :y (+ 1 y), :position UP)
    (and (= pos LEFT) (> x 0)) (hash-map :x (- 1 x), :y y, :position RIGTH)
    :else nil)))

(defn get-score
  ""
  [board player]
  (count (filter #(= % player) (board :squares))))

(defn draw-line
  "TODO: HELP"
  [board move player]
  (let [score           (get-score board player)
        board-after     (set-index board move player)
        c-move          (get-complement-move board move)
        c-board-after   (if c-move (set-index board-after c-move player) board-after)
        score-after     (get-score c-board-after player)
        change-player   #(case % 1 2 2 1 :else -1)]
    (if (= score-after score)
      (assoc board-after :turn (change-player (board :player)))
      board-after)))

(defn pos-to-xy [board pos]
  (let [y (int (/ pos (board :cols)))
        x (- pos (* y (board :cols)))
        ]
    [x, y]))

