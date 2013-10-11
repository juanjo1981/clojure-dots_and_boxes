(ns dots-and-boxes.game)

(def COLS (atom 4))
(def ROWS (atom 4))

(def UP 0)
(def RIGTH 1)
(def BOTTOM 2)
(def LEFT 3)
(def SQ_SIZE 4)

(defn init-board-v1
  "TODO: HELP"
  [x y]
  {:pre [(> x 0) (> y 0)]
  :post [(vector? %) (= (* x y) (count %))]}
  (let [set-border
        #(vector 
           (if (= %2 0) -1 0)
           (if (= %1 (- x 1)) -1 0)
           (if (= %2 (- y 1)) -1 0)
           (if (= %1 0) -1 0)
           0)]
       (loop [cols 0 rows 0 
              result []]
         ;(print (v (+ i (* i j))))
         ;(print " | " i " > " j " | ")
         (if (= rows y)
           result
           (recur (if (< cols (- x 1)) (inc cols) 0)
                  (if (= cols (- x 1)) (inc rows) rows)
                  (into result (vector (set-border cols rows))))))))

(defn init-board
  "TODO: HELP"
  [x y]
  {:pre [(> x 0) (> y 0)]
   :post [(vector %) (= (count %) (+ (* y x 4) (* y x)))]}
  (let [set-border
        #(vector 
           (if (= %2 0) -1 0)
           (if (= %1 (- x 1)) -1 0)
           (if (= %2 (- y 1)) -1 0)
           (if (= %1 0) -1 0))]
       (loop [cols 0 rows 0 
              result []]
         (if (= rows y)
           (into result (take (* x y) (repeat 0)))
           (recur (if (< cols (- x 1)) (inc cols) 0)
                  (if (= cols (- x 1)) (inc rows) rows)
                  (into result (set-border cols rows)))))))
(defn draw-line
  "TODO: HELP"
  [board x y pos player cols rows]
  {:pre [(>= x 0) (>= y  0) (< x cols) (< y rows)]
   :post [(vector? %) ]}
  (assoc board (get-index x y pos cols) player))

(defn get-index 
  "TODO: HELP"
  [x y pos cols rows]
  {:pre [(>= x 0) (>= y 0) (>= pos UP) (<= pos LEFT)]
   :post [(>= % 0 ) (<  % (* cols rows 4))]}
  (+ pos (* x SQ_SIZE) (* y (* SQ_SIZE cols))))


(defn get-indexes
  "TODO: HELP"
  [x y pos cols rows]
  (cond 
    (and (= pos UP) (> y 0)) (into [] [(get-index x y pos cols rows) (get-index x (- 1 y) BOTTOM cols rows)]) 
    (and (= pos RIGTH) (< x (- cols 1))) (into [] [(get-index x y pos cols rows) (get-index (+ 1 x)  y LEFT cols rows)])
    (and (= pos BOTTOM) (< y (- rows 1)))  (into [] [(get-index x y pos cols rows) (get-index x (+ 1 y) UP cols rows)])
    (and (= pos LEFT) (> x 0)) (into [] [(get-index x y pos cols rows) (get-index (- 1 x)  y RIGTH cols rows)])
    :else (into [] [(get-index x y pos cols rows)]))
  )
