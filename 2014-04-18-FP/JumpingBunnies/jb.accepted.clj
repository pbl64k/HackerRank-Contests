(require 'clojure.string)

(defn gcd [a b]
    (cond
        (zero? b) a
        (< a b) (recur b a)
        :true (recur b (mod a b))))

(defn lcm [a b] (* (/ a (gcd a b)) b))

(read-line)
(println (reduce lcm 1 (map read-string (clojure.string/split (read-line) #" "))))
