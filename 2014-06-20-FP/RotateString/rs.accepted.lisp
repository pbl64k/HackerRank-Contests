(defun g (acc f b)
    (if f
        (g (append acc f (reverse b) (list #\Space)) (cdr f) (cons (car f) b))
        acc))

(defun f (s)
    (append (g '() (cdr s) (list (car s))) s))

(setq tests (parse-integer (read-line)))
(loop for i from 1 to tests do
    (princ (coerce (f (coerce (read-line) 'list)) 'string))
    (terpri))
