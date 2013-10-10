(ns dots-and-boxes.game)

(def COLS (atom 4))
(def ROWS (atom 4))

(defn init-board 
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

(defn init-board-v2
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
         ;(print (v (+ i (* i j))))
         ;(print " | " i " > " j " | ")
         (if (= rows y)
           (into result (take (* x y) (repeat 0)))
           (recur (if (< cols (- x 1)) (inc cols) 0)
                  (if (= cols (- x 1)) (inc rows) rows)
                  (into result (set-border cols rows)))))))
(defn move
  "TODO: HELP"
  [board x y pos player]
  {:pre [(>= x 0) (>= y  0) (< x @COLS) (< y @ROWS)]
   :post [(vector? %) ]}
  board)



