(ns tictactoe.core)

(defrecord Board [matrix turn ai-move])
(def board (->Board (ref [[0 0 0] [0 0 0] [0 0 0]]) (ref 1) (ref nil)))

(def char-map {:-1 "X" :0 "_" :1 "O"})

(defn check-diags
  [v]
  (let [m @(get board :matrix)
        sum1 (+ ((m 0) 0) ((m 1) 1) ((m 2) 2))
        sum2 (+ ((m 0) 2) ((m 1) 1) ((m 2) 0))]
    (or (= sum1 v) (= sum2 v))))

(defn check-rows
  [v]
  (let [m @(get board :matrix)
        row1 (apply + (m 0))
        row2 (apply + (m 1))
        row3 (apply + (m 2))]
    (not (empty? 
           (filter #(= v %) (list row1 row2 row3))))))

(defn check-columns
  [v]
  (let [m @(get board :matrix)
        col1 (apply + (for [i m] (i 0)))
        col2 (apply + (for [i m] (i 1)))
        col3 (apply + (for [i m] (i 2)))]
    (not (empty? 
           (filter #(= v %) (list col1 col2 col3))))))

(defn print-board
  []
  (let [m @(get board :matrix)
        s (str (clojure.string/join "|" (m 0)) "\n"
               (clojure.string/join "|" (m 1)) "\n"
               (clojure.string/join "|" (m 2)))]
    (println (-> s
                 (clojure.string/replace #"0" "_")
                 (clojure.string/replace #"1" "O")
                 (clojure.string/replace #"-" "X")
                 (clojure.string/replace #"XO" "X")))))

(defn add-to-board!
  [x y value]
  (let [m-ref (get board :matrix)]
    (dosync (ref-set
              m-ref
              (assoc-in @m-ref [x y] value)))))

(defn check-win
  [player]
  (let [v (if (= -1 player) -3 3)]
    (or (check-diags v)
        (check-rows v)
        (check-columns v))))

(defn swap-turn!
  []
  (dosync (ref-set (get board :turn) (- @(get board :turn)))))

(defn tie?
  []
  (let [m @(get board :matrix)]
    (empty? (flatten (for [i m] (filter #(= 0 %) i))))))

(defn legal-moves
  []
  (let [m @(get board :matrix)]
    (if (or (check-win -1) (check-win 1)) '()
      (filter
        #(not (= nil %))
        (for [i (range 3) j (range 3)]
          (when (= ((m i) j) 0) (list i j)))))))

(defn static-eval 
  [maximizing-for depth]
  (cond
    (check-win (- maximizing-for)) -100
    (check-win maximizing-for) (- 100 depth)
    :else 0))

(defn minimax
  [alpha beta maximizing depth value maximizing-for]
  (cond 
    (empty? (legal-moves)) (static-eval maximizing-for depth)
    maximizing (let [max-ev (ref -100000)]
                 (loop [moves (legal-moves)
                        alpha alpha
                        beta beta]
                   (when-not (empty? moves)
                     (let [move (first moves)
                           x (first move)
                           y (second move)
                           rest-lst (rest moves)]
                       (add-to-board! x y value)
                       (let [ev (minimax 
                                  alpha
                                  beta
                                  (not maximizing)
                                  (inc depth)
                                  (- value)
                                  maximizing-for)]
                         (add-to-board! x y 0)
                         (when (and (> ev @max-ev) (= depth 0))
                           (dosync 
                             (ref-set (get board :ai-move) move)))
                         (when (> ev @max-ev)
                           (dosync 
                             (ref-set max-ev ev)))
                         (when-not (<= beta (max alpha @max-ev))
                           (recur rest-lst 
                                  (max alpha @max-ev) beta)))))) @max-ev)
    :else (let [min-ev (ref 100000)]
            (loop [moves (legal-moves)
                   alpha alpha
                   beta beta]
              (when-not (empty? moves)
                (let [move (first moves)
                      x (first move)
                      y (second move)
                      rest-lst (rest moves)]
                  (add-to-board! x y value)
                  (let [ev (minimax 
                             alpha
                             beta
                             (not maximizing)
                             (inc depth)
                             (- value)
                             maximizing-for)]
                    (add-to-board! x y 0)
                    (when (< ev @min-ev)
                      (dosync 
                        (ref-set min-ev ev)))
                    (when-not (<= (min beta @min-ev) alpha)
                      (recur rest-lst alpha (min beta @min-ev))))))) @min-ev)))

(defn end?
  []
  (let [over? (check-win @(get board :turn))]
    (if (tie?) (do (print-board) (println "Tie") (System/exit 0))
      (if over? (do (print-board) (println 
                                    (str "Game Over!!!\nAI player won"))
                    (System/exit 0)) false))))

(defn validate-num
  [bound1 bound2]
  (try
    (def n (read-string (read-line)))
    (catch Exception e (def n -1)))
  (if-not (and (number? n) (>= n bound1) (<= n bound2))
    (do (println (str "Choose a number from " bound1 " - " bound2))
        (recur bound1 bound2)) n))

(defn player-turn
  []
  (def character (if (= -1 @(get board :turn)) "X" "O"))
  (println (str "Your turn (you are " character ")"))
  (print-board)
  (let [n (dec (validate-num 1 9))
        x (quot n 3)
        y (mod n 3)
        legal? (some #(= (list x y) %) (legal-moves))]
    (if-not legal? 
      (do (println "Illegal move") (recur))
      (do
        (add-to-board! x y @(get board :turn))
        (end?)
        (swap-turn!)))))

(defn ai-turn
  []
  (print-board)
  (println "AI playing")
  (let [turn @(get board :turn)]
    (minimax -10000 10000 true 0 turn turn)
    (def mv @(get board :ai-move))
    (add-to-board! (first mv) (second mv) turn)
    (end?)
    (swap-turn!)))

(defn game-loop
  [player]
  (if player
    (do
      (player-turn)
      (ai-turn))
    (do 
      (ai-turn)
      (player-turn)))
  (recur player))

(defn main
  []
  (println "Tic tac toe against an AI")
  (println "Type 1 for you to start, 2 for AI to start")
  (def start (= 1 (validate-num 1 2)))
  (println "Choose square to play using the following numbers:")
  (println "1 2 3")
  (println "4 5 6")
  (println "7 8 9")
  (game-loop start)
  (print-board))
