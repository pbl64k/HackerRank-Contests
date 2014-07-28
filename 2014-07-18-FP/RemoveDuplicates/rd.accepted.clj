(require 'clojure.string)

(defn f [acc c]
    (if (contains? (first acc) c)
        acc
        (cons (conj (first acc) c) (cons c (rest acc)))))

(defn nb [str]
    (clojure.string/join (reverse (rest (reduce f (cons #{} "") str)))))

(println (nb (read-line)))
