#lang racket/base

(require racket/sequence)

(module+ test
  (require rackunit))

(module+ test
  (define test-nums
    (list 1721 979 366 299 675 1456)))

(define (find-summands nums sum count)
  (cond
    [(and (zero? count) (zero? sum))           null]
    [(or (zero? count) (<= sum 0) (null? nums)) #f]
    [else
      (define hd (car nums))
      (define new-sum (- sum hd))
      (define summands (find-summands (cdr nums) new-sum (sub1 count)))
      (if summands
          (cons hd summands)
          (find-summands (cdr nums) sum count))]))

(module+ test
  (check-equal? (find-summands (sort test-nums >) 2020 2) '(1721 299))
  (check-equal? (find-summands (sort test-nums >) 2020 3) '(979 675 366)))

(define (find-pair nums)
  (define summands (find-summands (sort nums >) 2020 2))
  (and summands (apply * summands)))

(define (find-triple nums)
  (define summands (find-summands (sort nums >) 2020 3))
  (and summands (apply * summands)))

(module+ test
  (check-equal? (find-pair test-nums) 514579)
  (check-equal? (find-triple test-nums) 241861950))

(module* part-1 #f
  (require racket/file)
  (find-pair (file->list "inputs/01.txt")))

(module* part-2 #f
  (require racket/file)
  (find-triple (file->list "inputs/01.txt")))

