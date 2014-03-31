(defun ff (x acc)
    (if x
        (ff (cddr x) (cons (car x) (cons (cadr x) acc)))
        (reverse acc)))
(defun f (x) (ff x nil))
(setq worgl (parse-integer (read-line)))
(loop for i from 1 to worgl do
    (princ (coerce (f (coerce (read-line) 'list)) 'string))
    (terpri))
