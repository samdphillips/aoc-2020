#lang racket/base

(require racket/match)

(module+ test
  (require rackunit))

(define (decode-seat-id s)
  (for/fold ([seat-id 0]) ([ch (in-string s)])
    (+ (* 2 seat-id)
       (match ch
         [(or #\F #\L) 0]
         [(or #\B #\R) 1]))))

(module+ test
  (check-equal? (decode-seat-id "BFFFBBFRRR") 567)
  (check-equal? (decode-seat-id "FFFBBBFRRR") 119)
  (check-equal? (decode-seat-id "BBFFBBFRLL") 820))

(module* part-1 #f
  (require racket/port)
  (call-with-input-file "inputs/05.txt"
    (lambda (inp)
      (for/fold ([v 0]) ([seat (in-lines inp)])
        (max v (decode-seat-id seat))))))

(module* part-2 #f
  (require racket/port
           data/integer-set)

  (call-with-input-file "inputs/05.txt"
    (lambda (inp)
      (for/fold ([s (make-integer-set null)]) ([seat (in-lines inp)])
        (union s (make-range (decode-seat-id seat)))))))
