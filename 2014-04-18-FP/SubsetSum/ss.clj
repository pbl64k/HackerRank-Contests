(require 'clojure.string)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord St [l ^long n ^long s r])
(defrecord Pair [fst snd])

(defn mktree [^long n xs]
    (cond
        (zero? n) (Pair. nil xs)
        (= n 1) (Pair. (St. nil (:fst (first xs)) (:snd (first xs)) nil) (rest xs))
        :true (let [midp (quot (+ 1 n) 2)
            ^St a (mktree (- midp 1) xs)
            xs1 (:snd a)
            ^St b (mktree (- n midp) (rest xs1))]
            (Pair. (St. (:fst a) (:fst (first xs1)) (:snd (first xs1)) (:fst b)) (:snd b)))))

(defn tfind [^St t ^long x c]
    (cond
        (nil? t) c
        (== x (:s t)) (:n t)
        (< x (:s t)) (recur (:l t) x (:n t))
        :true (recur (:r t) x c)))

(defn tst [^St t]
    (let [^long s (read-string (read-line))]
        (println (tfind t s -1))))

(read-line)
(def xs (map (fn [^String x] (long (read-string x))) (clojure.string/split (read-line) #" ")))
(def ys (sort > xs))
(def y0s (:snd (reduce
    (fn [^Pair z ^long x] (Pair. (+ (:fst z) x) (list* (+ (:fst z) x) (:snd z))))
    (Pair. 0 (list)) ys)))
(def y1s (map (fn [^long a ^long b] (Pair. a b)) (range 1 (+ 1 (count y0s))) (reverse y0s)))
(def ^St tree (:fst (mktree (count y1s) y1s)))
(def ^long t (read-string (read-line)))
(doall (map (fn [^long n] (tst tree)) (range t)))
