(defun f (acc c)
    (if (member c (car acc))
        acc
        (cons (adjoin c (car acc)) (cons c (cdr acc)))))

(defun nb (str)
    (reverse (cdr (reduce #'f str :initial-value (cons '() '())))))

(princ (coerce (nb (coerce (read-line) 'list)) 'string))
(terpri)
