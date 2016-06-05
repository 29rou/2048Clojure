(ns game-2014.core
  (:gen-class))

(defn init_table []
  (let [table (vec (repeat 4 (vec (repeat 4 0))))]
    (assoc-in table [(rand-int 4) (rand-int 4)] 2)))

(defn shift [col]
  (if-not(< 1 (count col)) col
    (let [merge_c #(let [tmp (nth %1 %2)]
                     (if (= tmp (nth %1 (inc %2)))
                       (assoc (assoc %1 %2 (* tmp 2)) (inc %2) 0) %1))] 
      (last (reductions #(merge_c %1 %2) col (range (- (count col) 1)))))))
       
(defn rot [table n]
  (let [rotation (fn [x](vec (map #(vec (reverse %)) (apply map vector x))))]
    (nth (iterate rotation table ) n)))

(defn move [table n]
  (let [rm_0 #(vec (remove zero? %)),
        fill_0 #(take 4 (concat % (repeat 0)))]
    (rot (map #(fill_0 (rm_0 (shift (rm_0 %)))) (rot table n)) (- 4 n))))

(defn set_rnd [table]
  (let [grid_list (for [x (range 0 4) y (range 0 4)] [x y]),
        check_empty #(zero? (get-in table [(first %)(second %)])),
        emp (rand-nth (filter check_empty grid_list))]
   (assoc-in table [(first emp) (second emp)] (* (+ (rand-int 2) 1) 2))))

(defn cmd_dir [cmd]
  (let [dir_map (find (hash-map :Left 0,:Down 1,:Right 2,:Up 3,:Exit 4) cmd)]
    (if(nil? dir_map) nil (val dir_map))))

(defn output [table]
  (let [str_table (fn [i] (map #(format "%4d" % ) i))]
    (doseq [ n (map str_table table)] (println (apply str n)))(println)))

(defn -main [& args]
  (def init (init_table))
  (output init)
  (loop[table init]
    (when(reduce #(and %1 %2) (map #(= table (move table %))(range 4)))
      (do (println "Game Over!!") (System/exit 0)))
    (let [dir (cmd_dir (keyword (read-line)))]
      (when (= dir 4)(System/exit 0))
      (let [moved (if(nil? dir) table(move table dir)),
            new_table (if(= table moved)table(set_rnd moved))]
        (when-not(= table moved)(do (output new_table)))(recur new_table)))))
