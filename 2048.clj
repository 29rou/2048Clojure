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
      (reduce #(merge_c %1 %2) col (range (- (count col) 1))))))
       
(defn rot [table n]
  (let [rotation (fn [x](mapv #(vec (rseq %)) (apply mapv vector x)))]
    (nth (iterate rotation table ) n)))

(defn move [table n]
  (let [rm_0 #(vec (remove zero? %)),
        fill_0 #(take 4 (concat % (repeat 0)))]
    (rot (map #(fill_0 (rm_0 (shift (rm_0 %)))) (rot table n)) (- 4 n))))

(defn set_rnd [table]
  (let [chk_emp #(zero? (get-in table [(first %)(second %)])),
        emp (rand-nth (filter chk_emp (for [x (range 4) y (range 4)] [x y])))]
   (assoc-in table [(first emp) (second emp)] (* (+ (rand-int 2) 1) 2))))

(defn cmd_dir [cmd]
  (if-let [dir_map (find (hash-map :Left 0,:Down 1,:Right 2,:Up 3,:Exit 4) cmd)]
    (val dir_map) nil))

(defn output [table]
  (let [str_table (fn [i] (map #(format "%4d" % ) i))]
    (doseq [ n (map str_table table)] (println (apply str n)))(println))table)

(defn game [table cmd]
  (let [dir (cmd_dir (keyword cmd))]
    (cond
      (every? #(= table (move table %)) (range 4))(System/exit 0)
      (= dir 4)(System/exit 0)
      (nil? dir) table
      (not= table (move table dir)) (output (set_rnd (move table dir)))
      :else table)))

(defn -main [& args]
  (reduce game (output (init_table))(repeatedly read-line)))
