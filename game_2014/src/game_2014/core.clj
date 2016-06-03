(ns game-2014.core
  (:gen-class))

(defn init_table []
  (let [table (vec (repeat 4 (vec (repeat 4 0)))),
        i (rand-int 4),
        j (rand-int 4)]
    (assoc table i (assoc (nth table i) j 2))))

(defn shift [col]
  (loop [result col i 0]
    (if (>= i (- (count result) 1)) result
      (let [tmp (nth result i),
            tmp_n (nth result (inc i)),
            merge_col #(if (= tmp tmp_n)
                        (assoc (assoc % i (* tmp 2)) (inc i) 0) %1)] 
      (recur (merge_col result) (inc i))))))  
       
(defn rot [table n]
  (loop[result table i 0]
    (let [to_vec_rev #(vec (reverse %)),
          rotation #(vec (map to_vec_rev (apply map vector %)))]
      (if(>= i n)
        (vec (map vec result))
        (recur (rotation result)(inc i)))))) 

(defn move [table n]
  (let [rm_zero #(vec (remove zero? %)),
        zero_fill #(concat %  (repeat (- 4 (count %)) 0)),
        shift_and_fill_rm_zero #(zero_fill (rm_zero (shift (rm_zero %))))]
    (rot (map shift_and_fill_rm_zero (rot table n)) (- 4 n))))

(defn set_random [table]
  (let [grid_list (for [x (range 0 4) y (range 0 4)] [x y]),
        check_empty #(zero? (nth (nth table (second %)) (first %))),
        emp_grid (rand-nth (filter check_empty grid_list)),
        emp_x (first emp_grid),
        emp_y (second emp_grid),
        rand_v (* (+ (rand-int 2) 1) 2)]
   (assoc table emp_y (assoc (nth table emp_y) emp_x rand_v))))

(defn update_table [table command]
  (let [direction #(find (hash-map :Left 0,:Down 1,:Right 2,:Up 3) %)]
    (if(nil? (direction command)) table
      (move table (val (direction command))))))

(defn output [table]
  (let [str_table (fn [i] (map #(format "%4d" % ) i))]
    (doseq [ n (map str_table table)] (println (apply str n)))(println)))

(defn -main [& args]
  (def init (init_table))
  (output init)
  (loop[table init]
     (let [dir (keyword (read-line)),
           new_table (update_table table dir)]
      (if (= table new_table)
        (if(reduce #(and %1 %2) (map #(= table (move table %))(range 0 4)))
          (do (println "Game Over!!"))
          (if(not= dir :Exit)(recur table)))
        (let [next_table (set_random new_table)]
          (do (output next_table)
              (recur next_table)))))))
