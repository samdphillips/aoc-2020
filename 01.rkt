#lang racket/base

(require racket/sequence)

(module+ test
  (require rackunit))

(module+ test
  (define test-nums
    (list 1721
          979
          366
          299
          675
          1456)))

(define (in-tails a-list)
  (make-do-sequence
    (lambda ()
      (values cdr cdr a-list #f #f (lambda (pos val) (not (null? val)))))))

(module+ test
  (check-equal? (sequence->list (in-tails '(1))) '(()))
  (check-equal? (sequence->list (in-tails '(1 2 3 4)))
                (list '(2 3 4) '(3 4) '(4) '())))

(define (find-pairs-1 nums)
  (let ([nums (sort nums <)])
    (for/first ([a (in-list nums)]
                [d (in-tails nums)]
                #:when (member (- 2020 a) d))
      (* a (- 2020 a)))))

(module+ test
  (check-equal? (find-pairs-1 test-nums) 514579))

(module* part-1 #f
  (require racket/file)
  (find-pairs-1 (file->list "inputs/01.txt")))

(define (find-min-index nums v i)
  (for/first ([j (in-naturals i)] [n (in-vector nums i)] #:when (< n v)) j))

(module+ test
  (check-equal? (find-min-index '#(10 9 8 7 6 5) 7 0) 4)
  (check-equal? (find-min-index '#(20 10 9 8 7 6 5) 15 0) 1)
  (check-equal? (find-min-index '#(20 10 9 8 7 6 5) 7 3) 5)
  (check-equal? (find-min-index '#(100 20 10) 5 0) #f))

(define (vector-member? vec i v)
  (for/or ([e (in-vector vec i)]) (equal? e v)))

(define (find-pairs nums i s)
  (for/first ([a (in-vector nums i)]
              [j (in-naturals i)]
              #:when (vector-member? nums j (- s a)))
    (* a (- s a))))

(module+ test
  (check-equal? (find-pairs '#(1721 1456 979 675 366 299) 0 2020) 514579))

(define (find-triples nums)
  (let ([nums (list->vector (sort nums >))])
    (let/ec return
      (for ([i (in-naturals)]
            [a (in-vector nums)])
        (define s (- 2020 a))
        (define j (find-min-index nums s i))
        (when j
          (define b (find-pairs nums j s))
          (when b (return (* a b))))))))

(module+ test
  (check-equal? (find-triples test-nums) 241861950))

(module* part-2 #f
  (require racket/file)
  (find-triples (file->list "inputs/01.txt")))
