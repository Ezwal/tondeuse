(ns tondeuse.core
  (:gen-class))

(def regex-map #"(\d+) (\d+)")
(def regex-tondeuse #"(\d+) (\d+) (\w)")
(def Orientation [{:dir "N" :d [0 1]}
                  {:dir "E" :d [1 0]} 
                  {:dir "S" :d [0 -1]}
                  {:dir "W" :d [-1 0]}])
(def x 0)
(def y 0)

(defn outMap?
  [tondeuse-info]
  (let [xDest (first tondeuse-info)
        yDest (second tondeuse-info)]
  (or (or (> xDest x) (> yDest y))
          (or (neg? xDest) (neg? yDest)))))

(defn advance
  "avance la tondeuse d'une case si possible"
  [tondeuse-info]
  (let [future-tondeuse 
        (map + tondeuse-info 
             (:d (first (filter #(= (:dir %) (last tondeuse-info)) Orientation))))]
    (if (outMap? future-tondeuse)
      tondeuse-info
      (conj (vec future-tondeuse) (last tondeuse-info)))))

(defn rotate
  "change orientation"
  [tondeuse-info clockwise]
  (let [inf-or (flatten (repeat 2 Orientation))
        curOrient (last tondeuse-info)]
    (->> (drop-while 
           #(not= curOrient (:dir %)) 
                (if (pos? clockwise) 
                  inf-or
                  (reverse inf-or)))
         (second)
         (:dir)
         (conj (vec (butlast tondeuse-info))))))

(defn move
  "si donne vector avec xyo et nouvel ordre alors replace la tondeuse"
  [tondeuse-info ordre]
    (if (= ordre "A")
      (advance tondeuse-info) 
     (if (= ordre "D")
       (rotate tondeuse-info 1)
       (rotate tondeuse-info -1))))

(defn str-tondeuse-info-to-vector
  "parse a string into a vec"
  [s]
  (let [parsed-s (re-find regex-tondeuse s)
        [_ xt yt ot] parsed-s]
    (conj [] (Integer/parseInt xt) (Integer/parseInt yt) ot)))

(defn parse-tondeuse
  [lines]
  (let [tondeuse-info (str-tondeuse-info-to-vector (first lines))
          orders (second lines)]
      (reduce #(move %1 (str %2)) tondeuse-info orders)))

(defn consume-tondeuse
  [lines]
  (do 
    (println (parse-tondeuse (take 2 lines)))
    (if (not (empty? (nnext lines)))
      (consume-tondeuse (nnext lines)))))

(defn size-map!
  "init la carte et renvoie le reste des args non utilisé"
  [lines]
  (let [parsedSize (re-find regex-map (first lines))
        [width length] (rest parsedSize)]
   (def x (Integer/parseInt width))
   (def y (Integer/parseInt length))
  (rest lines)))

(defn -main
  "mange le fichier d'input et calcul le mouvement des tondeuses"
  [path]
  (-> (slurp path)
      (clojure.string/split-lines)
      (size-map!)
      (consume-tondeuse)))

