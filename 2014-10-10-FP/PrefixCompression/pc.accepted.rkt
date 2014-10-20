#lang racket

(define [slv pfx a b]
  (if (or (null? a) (null? b) (not (char=? (first a) (first b))))
      (cons pfx (cons a b))
      (slv (cons (first a) pfx) (rest a) (rest b))))

(define [main]
  (let* [(astr (string->list (read-line)))
        (bstr (string->list (read-line)))
        (res (slv null astr bstr))]
    (begin
      (display (string-append (~a (length (first res))) " " (list->string (reverse (first res))) "\n"))
      (display (string-append (~a (length (first (rest res)))) " " (list->string (first (rest res))) "\n"))
      (display (string-append (~a (length (rest (rest res)))) " " (list->string (rest (rest res))) "\n")))))

(main)