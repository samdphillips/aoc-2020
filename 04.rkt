#lang racket/base

(require racket/hash
         racket/match
         racket/set
         racket/string

         fancy-app)

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

(define required-fields
  (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))
(define optional-fields
  (set "cid"))
(define (valid-passport-fields? p)
  (define present-fields (list->set (hash-keys p)))
  (define missing-fields
    (set-subtract required-fields present-fields))
  (or (set-empty? missing-fields)
      (equal? optional-fields missing-fields)))

(module+ test
  (check-equal?
    (call-with-input-string test-input-data
      (lambda (inp)
        (for/list ([p (in-port read-passport inp)])
          (valid-passport-fields? p))))
    '(#t #f #t #f)))

(module* part-1 #f
  (require racket/port)
  (call-with-input-file "inputs/04.txt"
    (lambda (inp)
      (for/sum ([p (in-port read-passport inp)])
        (if (valid-passport-fields? p) 1 0)))))

(define ((make-passport-field-check field conv valid?) p)
  (valid? (conv (hash-ref p field))))

(define check-byr (make-passport-field-check "byr" string->number (<= 1920 _ 2002)))
(define check-iyr (make-passport-field-check "iyr" string->number (<= 2010 _ 2020)))
(define check-eyr (make-passport-field-check "eyr" string->number (<= 2020 _ 2030)))

(module+ test
  (check-true  (check-byr (hash "byr" "2002")))
  (check-false (check-byr (hash "byr" "2003"))))

(define check-hgt
  (make-passport-field-check "hgt"
                             (lambda (s)
                               (match s
                                 [(regexp #px"^(\\d+)(in|cm)$" (list _ q m))
                                  (cons (string->number q) m)]
                                 [_ #f]))
                             (lambda (v)
                               (match v
                                 [(cons q "cm") (<= 150 q 193)]
                                 [(cons q "in") (<= 59 q 76)]
                                 [_ #f]))))

(module+ test
  (check-true (check-hgt (hash "hgt" "60in")))
  (check-true (check-hgt (hash "hgt" "190cm")))
  (check-false (check-hgt (hash "hgt" "190in")))
  (check-false (check-hgt (hash "hgt" "190"))))

(define check-hcl
  (make-passport-field-check "hcl" values (regexp-match-exact? #px"#[0-9a-f]{6}" _)))

(module+ test
  (check-true (check-hcl (hash "hcl" "#123abc")))
  (check-false (check-hcl (hash "hcl" "#123abz")))
  (check-false (check-hcl (hash "hcl" "123abc"))))

(define check-ecl
  (make-passport-field-check "ecl" values (regexp-match-exact? #px"(amb|blu|brn|gry|grn|hzl|oth)" _)))

(define check-pid
  (make-passport-field-check "pid" values (regexp-match-exact? #px"\\d{9}" _)))

(define passport-checks
  (list check-byr
        check-iyr
        check-eyr
        check-hgt
        check-hcl
        check-ecl
        check-pid))
(define (valid-passport? p)
  (and (valid-passport-fields? p)
       (for/and ([chk (in-list passport-checks)]) (chk p))))

(module+ test
  (define invalid-passports-data
    #<<DATA
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
DATA
)
  (check-equal?
    (call-with-input-string invalid-passports-data
      (lambda (inp)
        (for/list ([p (in-port read-passport inp)])
          (valid-passport? p))))
    '(#f #f #f #f))

  (define valid-passports-data
    #<<DATA
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
DATA
)
  (check-equal?
    (call-with-input-string valid-passports-data
      (lambda (inp)
        (for/list ([p (in-port read-passport inp)])
          (valid-passport? p))))
    '(#t #t #t #t)))

(module* part-2 #f
  (require racket/port)
  (call-with-input-file "inputs/04.txt"
    (lambda (inp)
      (for/sum ([p (in-port read-passport inp)])
        (if (valid-passport? p) 1 0)))))
