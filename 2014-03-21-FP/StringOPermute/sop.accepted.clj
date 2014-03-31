(defn ff [x acc]
    (if (empty? x)
        (reverse acc)
        (recur (rest (rest x)) (cons (first x) (cons (first (rest x)) acc)))))
(defn f [x] (ff x nil))
(doall
    (map (fn [x] (println (clojure.string/join (f (read-line))))) (range (read-string (read-line)))))
