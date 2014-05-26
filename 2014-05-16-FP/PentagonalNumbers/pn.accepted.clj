(defn pn [x] (/ (- (* 3 x x) x) 2))
(doall
    (map (fn [x] (println (pn (read-string (read-line))))) (range 0 (read-string (read-line)))))
