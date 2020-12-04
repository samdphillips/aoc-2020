#lang racket/base

(require racket/hash
         racket/match
         racket/set
         racket/string)

(module+ test
  (require rackunit
           racket/port)

  (define test-input-data
    #<<DATA
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
DATA
))

(define (empty-line? s)
  (zero? (string-length (string-trim s))))

(define (read-passport/1 inp)
  (define line (read-line inp))
  (cond
    [(or (eof-object? line) (empty-line? line)) #f]
    [else
      (for/hash ([tok (in-list (string-split line))])
        (match-define (list k v) (string-split tok ":"))
        (values k v))]))

(module+ test
  (call-with-input-string test-input-data
    (lambda (inp)
      (check-equal? (read-passport/1 inp)
                    '#hash(("ecl" . "gry")
                           ("eyr" . "2020")
                           ("hcl" . "#fffffd")
                           ("pid" . "860033327")))
      (check-equal? (read-passport/1 inp)
                    '#hash(("byr" . "1937")
                           ("cid" . "147")
                           ("hgt" . "183cm")
                           ("iyr" . "2017")))
      (check-false (read-passport/1 inp))))

  (call-with-input-string "    \n"
    (lambda (inp)
      (check-false (read-passport/1 inp))
      (check-false (read-passport/1 inp)))))

(define (read-passport inp)
  (cond
    [(eof-object? (peek-byte inp)) eof]
    [else
      (for/fold ([p (hash)]) ([p1 (in-producer read-passport/1 #f inp)])
        (hash-union p p1))]))

(module+ test
  (call-with-input-string test-input-data
    (lambda (inp)
      (check-equal? (read-passport inp)
                    '#hash(("ecl" . "gry")
                           ("eyr" . "2020")
                           ("hcl" . "#fffffd")
                           ("pid" . "860033327")
                           ("byr" . "1937")
                           ("cid" . "147")
                           ("hgt" . "183cm")
                           ("iyr" . "2017")))
      (check-equal? (peek-string 3 0 inp) "iyr")))

  (check-equal? (call-with-input-string "" read-passport) eof))

(define required-keys
  (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))
(define optional-keys
  (set "cid"))
(define (valid-passport? p)
  (define pkeys (list->set (hash-keys p)))
  (define missing
    (set-subtract required-keys pkeys))
  (or (set-empty? missing)
      (equal? optional-keys missing)))

(module+ test
  (check-equal?
    (call-with-input-string test-input-data
      (lambda (inp)
        (for/list ([p (in-port read-passport inp)])
          (valid-passport? p))))
    '(#t #f #t #f)))

(module* part-1 #f
  (require racket/port)
  (call-with-input-file "inputs/04.txt"
    (lambda (inp)
      (for/sum ([p (in-port read-passport inp)])
        (if (valid-passport? p) 1 0)))))

