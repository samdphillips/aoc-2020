#lang racket/base

(require racket/match
         memoize)

(module+ test
  (require racket/port
           racket/sequence
           rackunit)

  (define test-policies-str
    "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"))

(struct policy-check [policy password] #:transparent)
(struct policy [min max char] #:transparent)

(define policy-check-pat
  #px"^(\\d+)-(\\d+) (.): ([^\n]+)\n")

(define (read-policy-checks inp)
  (match (regexp-match policy-check-pat inp)
    [#f (if (eof-object? (peek-byte inp))
            eof
            (error 'read-policy-checks
                   "no match for: ~s"
                   (read-line inp)))]
    [(list _ min-b max-b char passwd-b)
     (policy-check
       (policy (string->number (bytes->string/latin-1 min-b))
               (string->number (bytes->string/latin-1 max-b))
               (integer->char (bytes-ref char 0)))
       (bytes->string/latin-1 passwd-b))]))


(module+ test
  (check-equal? (call-with-input-string test-policies-str
                  (lambda (i) (sequence->list (in-port read-policy-checks i))))
                (list (policy-check (policy 1 3 #\a) "abcde")
                      (policy-check (policy 1 3 #\b) "cdefg")
                      (policy-check (policy 2 9 #\c) "ccccccccc"))))

(define/memo (policy->check-proc a-policy)
  (match-define (policy minv maxv the-char) a-policy)
  (define (the-char=? ch)
    (char=? the-char ch))
  (define (<minv chars count)
    (match chars
      [(? null?) #f]
      [(list (? the-char=?) chars ...)
       (let ([count (add1 count)])
         (if (= minv count)
             (minv/maxv chars count)
             (<minv chars count)))]
      [(cons _ chars) (<minv chars count)]))
  (define (minv/maxv chars count)
    (match chars
      [(? null?) #t]
      [(list (? the-char=?) chars ...)
       (let ([count (add1 count)])
         (if (> count maxv)
             #f
             (minv/maxv chars count)))]
      [(cons _ chars) (minv/maxv chars count)]))
  (lambda (s)
    (<minv (string->list s) 0)))

(module+ test
  (check-true  ((policy->check-proc (policy 1 3 #\a)) "abcde"))
  (check-false ((policy->check-proc (policy 1 3 #\b)) "cdefg"))
  (check-true  ((policy->check-proc (policy 2 9 #\c)) "ccccccccc")))

(define (check-password a-policy-check)
  (define valid-password?
    (policy->check-proc (policy-check-policy a-policy-check)))
  (valid-password? (policy-check-password a-policy-check)))

(module* part-1 #f
  (call-with-input-file "inputs/02.txt"
    (lambda (inp)
      (for/sum ([p (in-port read-policy-checks inp)])
        (if (check-password p) 1 0)))))


