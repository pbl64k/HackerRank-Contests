(defun pn (n) (/ (- (* 3 n n) n) 2))
(loop for i from 1 to (parse-integer (read-line)) do
    (princ (pn (parse-integer (read-line))))
    (terpri))
