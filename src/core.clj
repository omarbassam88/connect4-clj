(ns core
  (:require [clojure.string :as string]
            [clojure.edn :as edn])
  (:gen-class))

;; Board

(def num-rows 6)
(def num-cols 7)

(def board (atom
            (into [] (repeat num-rows (into [] (repeat num-cols 0))))))

(def col-filled-index (atom (into [] (repeat num-cols 0))))

(def human "P")
(def ai "A")

(defn clear-board []
  (reset! col-filled-index (into [] (repeat num-cols 0)))
  (reset! board (into [] (repeat num-rows (into [] (repeat num-cols 0))))))

(defn draw-board
  "Draws the Board in the Console"
  []
  (println)
  (print "  |")
  (dotimes [i num-cols] (print  (inc i) ""))
  (println)
  (dotimes [_ (* 2 (inc num-cols))] (print "-"))
  (println)
  (doseq [[i row] (map-indexed vector @board)]
    (print (inc i) "|")
    (doseq [col row] (print col ""))
    (println)))

(defn update-board [row col turn]
  (swap! board assoc-in [row col] turn)
  (swap! col-filled-index #(assoc % col (inc (get % col))))
  @board)

(defn undo-move [row col]
  (swap! board assoc-in [row col] 0)
  (swap! col-filled-index #(assoc % col (dec (get % col))))
  @board)

(defn board-full? []
  (let [full (atom true)]
    (doseq [row @board col row :when (= col 0)]
      (reset! full false))
    @full))

(defn score-direction [player direction row col]
  (let [count (atom 0)]
    (dotimes [n 4]
      (let  [[i j] (case direction
                     :horizontal [row (+ col n)]
                     :vertical [(+  row n) col]
                     :left-diagonal [(+  row n) (+  col n)]
                     :right-diagonal [(+  row n) (- col n)])]
        (when (and (< i num-rows)
                   (< j num-cols)
                   (= (get-in @board [i j]) player))
          (swap! count inc))))
    @count))

(defn check-win [player]
  (let [win (atom false)]
    (doseq [i (range num-rows) j (range num-cols)
            direction [:horizontal :vertical :left-diagonal :right-diagonal]
            :when  (not= (get-in @board [i j]) 0)
            :while (not @win)]
      (when (=  (score-direction player direction i j) 4)
        (reset! win true)))
    @win))

;; AI

(defn score [player]
  (let [directions [:horizontal :vertical :left-diagonal :right-diagonal]
        points [0 1 2 25 100]
        total (atom 0)]
    (doseq [row (range num-rows) col (range num-cols)
            dir directions
            :when (= (get-in @board [row col]) player)]
      (swap! total #(+ % (get points (score-direction player dir row col))))
      (swap! total #(- % 3)))
    @total))

(defn minimax
  ([depth] (minimax depth true))
  ([depth maximize]
   (cond
     (check-win human) -200
     (check-win ai) 200
     (board-full?) 0
     (>= depth 5) (- (score ai) (score human))
     maximize (let [max-val (atom ##-Inf)]
                (doseq [j (range num-cols)
                        :let [i (- (dec num-rows) (get @col-filled-index j))]
                        :while (>= i 0)]
									;; Try move at i, j
                  (update-board i j ai)
									;;  Calculate MiniMax
                  (let [val (minimax (inc depth) false)]
                    (swap! max-val #(max %  val)))
									;; Undo Last Move
                  (undo-move i j))
                @max-val)
     :else
     (let [mini-val (atom ##Inf)]
       (doseq [j (range num-cols)
               :let [i (- (dec num-rows) (get @col-filled-index j))]
               :while (>= i 0)]
				 ;; Try move at i, j
         (update-board i j human)
				 ;;  Calculate MiniMax
         (let [val (minimax (inc depth) true)]
           (swap! mini-val #(min % val)))
				 ;; Undo Last Move
         (undo-move i j))
       @mini-val))))

(defn play-ai []
  (let [max-val		(atom ##-Inf)
        best-row	(atom nil)
        best-col	(atom nil)]
    (doseq [j			(range num-cols)
            :let	[i (- (dec num-rows) (get @col-filled-index j))]
            :when (>= i 0)]
      (println i j)
      ;; Try move at i, j
      (update-board i j ai)
      ;; Calculate MiniMax
      (let [val (minimax 1 false)]
        (when (> val @max-val)
          (reset! max-val val)
          (reset! best-row i)
          (reset! best-col j)))
			;; Undo Last Move
      (undo-move i j)
      (println @max-val))
    (println "AI Chose : " (inc @best-row) (inc @best-col))
    (update-board @best-row @best-col ai)))

;; Human

(defn read-column []
  (println "Please Choose a Column (1-7) :")
  (let [in (second (read+string))]
    (if (string/blank? in)
      (recur)
      (edn/read-string in))))

(defn play-human []
  (loop [choice (read-column)]
    (cond
      ;; Check if the input is not number
      (not (int? choice))
      (do
        (println "You must Enter a Valid Number :")
        (recur (read-column)))
      ;; Check if the input is a valid column
      (not (<= 1 choice num-cols))
      (do
        (println "The number you entered is not between 1 and " num-cols)
        (recur (read-column)))
      ;; Check if Col is filled
      (> (get @col-filled-index (dec choice)) (dec num-rows))
      (do (println "That Column " choice " is already Full")
          (recur (read-column)))

			;; Afer the input is validated
			;; Perform the move
      :else (do
              (println "Your Choice is " choice)
              (update-board (- (dec num-rows)
                               (get @col-filled-index (dec choice)))
                            (dec choice)
                            human)))))

;; Main Game Loop

(defn win-message [player]
  (case player
    "P" (println "Congratulations You Won!!!")
    "A" (println "I can't believe you were beaten by a piece of metal :D.")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "\nWelcome to Connect Four!\n")
  (draw-board)
  (loop [turn  true]
    (println (if turn "It's Your turn." "It's AI turn now."))
    (if turn (play-human) (play-ai))
    (draw-board)
    (if (board-full?)
      (let [player (if turn human ai)]
        (if (check-win player) (win-message player)
            (println "I guess it's a tie")))
      (let [player (if turn human ai)]
        (if (check-win player)
          (win-message player)
          (recur (not turn)))))))

#_(-main)
