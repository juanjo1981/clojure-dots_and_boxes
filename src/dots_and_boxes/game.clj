(ns dots-and-boxes.game
  (use clojure.walk))

(def UP 0)
(def RIGTH 1)
(def BOTTOM 2)
(def LEFT 3)
(def PLAYER 4)
(def SQ_SIZE 5)
(def BORDER 3)

(defn init-board
  "TODO: HELP"
  [x y]
  {:pre [(> x 0) (> y 0)]
   ;:post [(vector %) (= (count %) (+ (* y x 4) (* y x)))]
   }
  (let [set-border
        #(vector 
           (if (= %2 0) BORDER 0)
           (if (= %1 (- x 1)) BORDER 0)
           (if (= %2 (- y 1)) BORDER 0)
           (if (= %1 0) BORDER 0)
           0)]
       (loop [col 0 row 0 
              result []]
         (if (= row y)
           {:cols x, :rows y, :num-squares (* x y) :player 1, :borders result}
           (recur (if (< col (- x 1)) (inc col) 0)
                  (if (= col (- x 1)) (inc row) row)
                  (into result (set-border col row)))))))

(defn xy-to-pos [board x y]
  (+ (* y SQ_SIZE (board :cols)) (* x SQ_SIZE)))

(defn get-index
  ""
  [board move]
  (let [base          (xy-to-pos board (move :x) (move :y)) 
        border-index  (+ base (move :position))
        square-index  (+ border-index (- SQ_SIZE  1 (move :position)))]
    (hash-map :base base :square square-index, :border border-index)))

(defn change-player 
  [player]
  (case player 1 2 2 1 :else 0))

(defn set-index 
  "TODO: HELP"
  [board move]
  (let [{base :base border :border square :square} (get-index board move)
        player (board :player)
        borders   (assoc (board :borders) border player)
        complete  (cond (>= (walk #(if (or (= % (change-player player)) (= % BORDER)) player %) 
                                  #(apply + %) 
                                  (subvec borders base (+ base 4)))
                            (* player 4)) 
                        player 
                        :else 0)]
    (assoc board :borders 
           (assoc borders square complete))))

(defn side-effect-move
  "TODO: HELP"
  [board move]
  (let [{x :x y :y pos :position} move
        {cols :cols rows :rows} board]
  (cond 
    (and (= pos UP) (> y 0))              {:x x, :y (- y 1), :position BOTTOM}  
    (and (= pos RIGTH) (< x (- cols 1)))  {:x (+ 1 x), :y y, :position LEFT}
    (and (= pos BOTTOM) (< y (- rows 1))) {:x x, :y (+ 1 y), :position UP}
    (and (= pos LEFT) (> x 0))            {:x (- x 1), :y y, :position RIGTH}
    :else nil)))

(defn get-score
  ""
  [board player]
  (->> (board :borders) 
             (into [0]) 
             (take-nth SQ_SIZE) 
             (next) 
             (filter #(= % player)) 
             (count)))

(defn can-move? [board move]
  (let [{border :border} (get-index board move)
        value ((board :borders) border)]
    (= 0 value)))


(defn draw-line
  "TODO: HELP"
  [board move]
  (if (can-move? board move)
    (let [player          (board :player)
          score           (get-score board player)
          board-after     (set-index board move)
          move-c          (side-effect-move board move)
          board-after-c   (if move-c 
                            (set-index board-after move-c) 
                            board-after)
          score-after     (get-score board-after-c player)]
      (if (= score-after score)
        (assoc board-after-c :player (change-player (board :player)))
        board-after-c))
    board))

(defn pos-to-xy [board pos]
  (let [
        y (int (/ pos (board :cols)))
        x (- pos (* y (board :cols)))]
    [x, y]))


(defn get-square
  "TODO: HELP"
  [board index]
  (let [new-index (* index SQ_SIZE)]
  (subvec (board :borders) new-index (+ new-index SQ_SIZE))))

(defn complete? 
  [board]
  (let [borders (board :borders)]
    (= 0 (count (filter #(= % 0) borders)))))


