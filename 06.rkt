#lang racket/base

(require racket/set)

(module+ test
  (require racket/port
           racket/sequence
           rackunit)
  (define test-input-data
    #<<DATA
abc

a
b
c

ab
ac

a
a
a
a

b
DATA
))

(define (empty-line? s)
  (regexp-match? #px"^\\s*$" s))

(define (read-group inp)
  (cond
    [(eof-object? (peek-byte inp)) null]
    [else
      (define line (read-line inp))
      (cond
        [(empty-line? line) null]
        [else
          (cons (list->set (string->list line))
                (read-group inp))])]))

(module+ test
  (call-with-input-string test-input-data
    (lambda (inp)
      (check-equal? (read-group inp)
                    (list (set #\a #\b #\c)))
      (check-equal? (read-group inp)
                    (list (set #\a)
                          (set #\b)
                          (set #\c)))
      (check-equal? (read-group inp)
                    (list (set #\a #\b)
                          (set #\a #\c))))))

(define (total-answers a-group)
  (set-count
    (for/fold ([st (set)]) ([i (in-list a-group)])
      (set-union st i))))

(module+ test
  (define test-groups
    (call-with-input-string test-input-data
      (lambda (inp) (sequence->list (in-producer read-group null? inp)))))
  (check-equal? (for/list ([a-group (in-list test-groups)])
                  (total-answers a-group))
                '(3 3 3 1 1)))

(module* part-1 #f
  (call-with-input-file "inputs/06.txt"
    (lambda (inp)
      (for/sum ([a-group (in-producer read-group null? inp)])
        (total-answers a-group)))))

(define (all-yes-answers a-group)
  (set-count
    (for/fold ([st #f]) ([i (in-list a-group)])
      (set-intersect (or st i) i))))

(module+ test
  (check-equal? (for/list ([a-group (in-list test-groups)])
                  (all-yes-answers a-group))
                '(3 0 1 1 1)))

(module* part-2 #f
  (call-with-input-file "inputs/06.txt"
    (lambda (inp)
      (for/sum ([a-group (in-producer read-group null? inp)])
        (all-yes-answers a-group)))))
