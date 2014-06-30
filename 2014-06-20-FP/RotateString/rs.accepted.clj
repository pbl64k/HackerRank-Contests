(require 'clojure.string)

(defn g [acc f b]
    (if (empty? f)
        acc
        (recur (concat acc f (reverse b) " ") (rest f) (cons (first f) b))))

(defn f [cs]
    (concat (g '() (rest cs) (list (first cs))) cs))

(doall
    (map
        (fn [x]
            (println (clojure.string/join (f (read-line)))))
        (range (read-string (read-line)))))

