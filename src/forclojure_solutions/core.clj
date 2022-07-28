(ns forclojure-solutions.core)


(defn problem21 [coll item]
  (filter (fn [[x y]]
            (= x item))
          (map-indexed vector coll)))

(defn problem23 [x]
  (reduce (fn [acc item]
            (cons item acc
                  ))
          (empty x)
          x
          ))

(defn problem25 [x]
  (reduce (fn [acc item]
            (if (odd? item)
              (conj acc item)
              acc))
          []
          x))

(defn problem26
  ([x] (problem26 x [1 1]))
  ([x y] (if (= (count y) x)
           y
           (recur x (conj y (apply + (take-last 2 y)))))))

(defn problem27 [x]
  (if (string? x)
    (= (clojure.string/reverse x) x)
    (= (reverse x) x)))

(defn problem28 [x]
  (reverse (reduce
            (fn rec-flatten [acc item]
              (if (coll? item) (reduce rec-flatten acc item)
                  (conj acc item)))
            '()
            x)))

(defn problem29 [x]
  (apply str(filter (set (map char (range 65 91))) x)))

(defn problem30 [x]
  (map first (partition-by identity x)))

(defn problem31 [x]
  (partition-by identity x))

(defn problem32 [x]
  (seq (reduce (fn [acc item]
                 (-> acc
                     (conj item)
                     (conj item)))
               []
               x)))

(defn problem33 [x y]
  (mapcat (fn [item]
            (take y (repeat item)))
          x))

(defn problem34 [x y]
  (take (- y x) (iterate inc x)))

(defn problem38 [& args]
  (last (sort args)))

(defn problem39 [x y]
  (flatten (map (fn [f s]
                  (conj '() s f))
                x y)))

(defn problem40 [x y]
  (take (- (* 2 (count y)) 1)
        (interleave y (repeat x))))

(defn problem41 [x y]
  (mapcat (fn [a]
            (take (- y 1) a))
          (partition-all y x)))

(defn problem42 [x]
  (apply * (range 1 (+ 1 x))))

(defn problem43 [x y]
  (apply map (fn [& args]
               args)
         (partition y x)))

(defn problem44 [x y]
  (let [new-coll (if (pos? x)
                   y
                   (reverse y))
        new-position (if (pos? x)
                       x
                       (+ x (* -2 x)))
        real-position (mod new-position (count y))
        rotated (flatten
                 (reduce
                  (fn [[acc-x acc-y] [x y]]
                    (if (< x real-position)
                      [acc-x (conj acc-y y)]
                      [(conj acc-x y) acc-y]))
                  [[] []]
                  (map-indexed vector new-coll)))]
    (if (pos? x)
      rotated
      (reverse rotated))))

(defn problem46 [input-fn]
  (fn [x y]
    (input-fn y x)))

(defn problem49 [x y]
  (vector (take x y) (drop x y)))

(defn problem50 [x]
  (vals (group-by type x)))

(defn problem53 [coll]
  (let [longest-seq (->> (reduce (fn [acc item]
                                   (cond
                                     (= (ffirst acc) nil) (conj (rest acc) (conj (first acc) item))
                                     (= (inc (ffirst acc)) item) (conj (rest acc) (conj (first acc) item))
                                     :else (conj acc (list item))))
                                 '(())
                                 coll)
                         (sort-by count)
                         last
                         reverse
                         vec)]
    (if (> (count longest-seq) 1)
      longest-seq
      [])))

(defn problem54 [par-num coll]
  (->> (reduce (fn [acc item]
                 (if (= (count (first acc)) par-num)
                   (conj acc (list item))
                   (conj (rest acc) (conj (first acc) item))))
               '(())
               coll)
       (filter #(= par-num (count %)))
       (map reverse)
       reverse))

(defn problem55 [X]
  (into (sorted-map)
        (map (fn [[k v]]
               {k (count v)})
             (group-by identity X))))

(defn problem56 [coll]
  (reduce (fn [acc item]
            (if ((set acc) item)
              acc
              (conj acc item)))
          []
          coll))

(defn problem58 [& fns]
  (fn [& args]
    (if (= (count fns) 1)
      (apply (first fns) args)
      ((apply problem58 (butlast fns))
       (apply (last fns) args)))))

(defn problem59 [& fns]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fns)))

(defn problem60
  ([f coll]
   (problem60 f (first coll) (rest coll)))
  ([f init coll]
   (println init)
   (println coll)
   (if (seq coll)
     (cons init (lazy-seq (problem60 f (f init (first coll)) (rest coll))))
     (list init))))

(defn problem61 [x y]
  (apply merge (map (fn [k v]
                      {k v})
                    x
                    y)))

(defn problem62 [fnc x]
  (cons x (lazy-seq (problem62 fnc (fnc x)))))


(defn problem63 [x y]
  (apply merge-with concat
         (map (fn [item]
                (hash-map (x item) [item]))
              y)))

(defn problem66 [x y]
  (loop [a (max x y)
         b (min x y)
         r (rem a b)]
    (if (= r 0)
      b
      (recur b r (rem b r)))))

(defn problem70 [s]
  (sort-by clojure.string/lower-case (-> s
                                         (clojure.string/replace #"[.!?]" "")
                                         (clojure.string/split #" "))))


(defn problem81 [x y]
  (apply sorted-set
         (filter
          (fn [z]
            (contains? x z))
          y)))

(defn problem83 [& args]
  (and (contains? (set args) false)
       (contains? (set args) true)))

(defn problem88 [x y]
  (set (concat (apply disj x y) (apply disj y x))))


(defn problem90 [x y]
  (set
   (for [a x
         b y]
     [a b])))

(defn problem95 [coll]
  (let [inner-coll (first (filter coll? coll))
        is-binary (odd? (count (filter #(and (not= % true) (not= % false)) (flatten coll))))]
    (if (and is-binary (= (count inner-coll) 3))
      (problem95 inner-coll)
      is-binary)))

(defn problem97 [row]
  (map (comp last take)
       (reverse (range 1 (+ 1 row)))
       (take row (iterate (partial reductions +) (take row (repeat 1))))))

(defn problem99 [x y]
  (->> (* x y)
       str
       seq
       (map (comp read-string str))))

(defn problem100 [& args]
  (letfn [(gcd [x y]
            (if (= x y)
              x
              (if (< x y)
                (recur x (- y x))
                (recur y (- x y)))))
          (lcm [x y]
            (/ (* x y) (gcd x y)))]
    (reduce lcm args)))

(defn problem107 [x]
  (fn [y]
    (apply * (repeat x y))))

(defn problem118 [f s]
  (when (seq s)
    (cons (f (first s))
          (lazy-seq (problem118 f (rest s))))))

(defn problem120 [seq]
  (-> (fn [item]
        (if (> 10 item)
          (< item (* item item))
          (< item (apply + (map (comp #(* % %) read-string str first)
                                (split-at 1 (str item)))))))
      (filter seq)
      count))

(defn problem122 [s]
  (apply + (map (fn [x]
                  (apply * (repeat x 2)))
                (keep-indexed #(if (= 1 %2) %1)
                              (mapv (comp read-string str) (reverse s))))))

(defn problem128 [x]
  (let [cards-suits {"S" :spade
                     "H" :heart
                     "D" :diamond
                     "C" :club}
        cards-rank (zipmap
                    (conj (->> (range 2 10)
                               (map str)
                               (vec))
                          "T" "J" "Q" "K" "A")
                    (range 13))
        card-data (map str (seq x))
        suit (cards-suits (first card-data))
        rank (cards-rank (last card-data))]
    {:suit suit :rank rank}))

(defn problem134 [k m]
  (and (contains? m k)
       (nil? (m k))))

(defn problem135 [& args]
  (let [[first-operand operator second-operand] (take 3 args)
        result (operator first-operand second-operand)]
    (if (= (count args) 3)
      result
      (apply problem135 (cons result (drop 3 args))))))

(defn problem143 [x y]
  (apply + (map * x y)))

(defn problem156 [x y]
  (zipmap y (repeat x)))

(defn problem157 [x]
  (map-indexed (fn [itm idx]
                 [idx itm]) x))

(defn problem166 [x y z]
  (cond
    (x y z) :lt
    (y z y) :gt
    :else :eq))
