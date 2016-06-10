(ns web-2048.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [hiccup.form :refer :all]
            [ring.util.response :refer :all]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))

(defn view-layout [& content]
  (html
    (doctype :html)
    (xhtml-tag "en"
               [:head
                [:meta {:http-equiv "Content-type"
                        :content "text/html; charset=utf-8"}]
                [:title "2048"]]
               [:body content])))


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
  (if-let [dir_map (find (hash-map :Left 0,:Down 1,:Right 2,:Up 3,:Reset 4) cmd)]
    (val dir_map) nil))

(defn output [table]
  (view-layout
    (let [to_cell (fn [x] (map #(html (conj [:td] %)) x)),
          to_table (fn [x] (html (map #(conj [:tr] (to_cell %)) x)))]
    [:table {:border = "1" :bordercolor = "#333333"} (to_table table)])
    [:form {:method "post" :action "/"}
     [:input {:type "hidden" :name "tbl" :value table}]
     [:input {:type "submit" :name "cmd" :value "Up"}]
     [:input {:type "submit" :name "cmd" :value "Down"}]
     [:input {:type "submit" :name "cmd" :value "Left"}]
     [:input {:type "submit" :name "cmd" :value "Right"}]
     [:input {:type "submit" :name "cmd" :value "Reset"}]
     ]))

(defn game [table cmd]
  (let [dir (cmd_dir (keyword cmd))]
    (cond
      (every? #(= table (move table %)) (range 4))(view-layout [:h1 "Game Over!"])
      (= dir 4)(output (init_table))
      (nil? dir) (output table)
      (not= table (move table dir)) (output (set_rnd (move table dir)))
      :else (output table))))

(defroutes app-routes
  (GET "/" [] (output (init_table)))
  (POST "/" [tbl cmd] (game (load-string tbl) cmd))
  (ANY "/*" [path] (redirect "/")))

(def app
  (handler/site app-routes))
