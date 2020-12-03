#lang racket/base

(require racket/match)

(module+ test
  (require racket/port
           rackunit)

  (define test-map-data #<<MAP
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
MAP
  ))

(struct toboggan-map [width height cells])

(define (toboggan-map-load inp)
  (define cells-list
    (let* ([width #f]
           [validate!
             (lambda (w)
               (if width
                   (unless (= w width)
                     (error 'toboggan-map-load
                            "map row length mismatch: expected ~a != ~a"
                            width w))
                   (set! width w)))])
      (for/list ([line (in-bytes-lines inp)])
        (validate! (bytes-length line))
        line)))
  (define height (length cells-list))
  (when (zero? height)
    (error 'toboggan-map-load "empty map"))
  (define width
    (bytes-length (car cells-list)))
  (define cells
    (let* ([size (* width height)]
           [cells (make-bytes size)])
      (for ([r (in-list cells-list)]
            [i (in-range 0 size width)])
        (bytes-copy! cells i r))
      cells))
  (toboggan-map width height cells))

(define (coord->index a-map x y)
  (let* ([width (toboggan-map-width a-map)]
         [x (modulo x width)])
    (unless (and (>= y 0) (< y (toboggan-map-height a-map)))
      (error 'toboggan-map-ref
             "coordinate out of bounds: expected ~a < ~a"
             y (toboggan-map-height a-map)))
    (+ x (* y width))))

(define (toboggan-map-ref m x y)
  (define i (coord->index m x y))
  (match (bytes-ref (toboggan-map-cells m) i)
    [46 'clear]
    [35 'tree]))

(module+ test
  (define test-map
    (call-with-input-string test-map-data
      toboggan-map-load))

  (check-equal? (toboggan-map-ref test-map 0 0) 'clear)
  (check-equal? (toboggan-map-ref test-map 1 0) 'clear)
  (check-equal? (toboggan-map-ref test-map 2 0) 'tree)

  (check-equal? (toboggan-map-ref test-map 0 1) 'tree)
  (check-equal? (toboggan-map-ref test-map 0 2) 'clear)
  (check-equal? (toboggan-map-ref test-map 0 3) 'clear)

  (let ([w (toboggan-map-width test-map)])
    (check-equal? (toboggan-map-ref test-map (+ w 0) 0) 'clear)
    (check-equal? (toboggan-map-ref test-map (+ w 1) 0) 'clear)
    (check-equal? (toboggan-map-ref test-map (+ w 2) 0) 'tree)))

(define (count-trees a-map dx dy)
  (define (do-count x y count)
    (define (next d)
      (do-count (+ dx x) (+ dy y) (+ count d)))
    (cond
      [(>= y (toboggan-map-height a-map)) count]
      [(eq? 'tree (toboggan-map-ref a-map x y)) (next 1)]
      [else (next 0)]))
  (do-count 0 0 0))

(module+ test
  (check-equal? (count-trees test-map 1 1) 2)
  (check-equal? (count-trees test-map 3 1) 7)
  (check-equal? (count-trees test-map 5 1) 3)
  (check-equal? (count-trees test-map 7 1) 4)
  (check-equal? (count-trees test-map 1 2) 2))

(module* part-1 #f
  (count-trees (call-with-input-file "inputs/03.txt" toboggan-map-load)
               3 1))

(module* part-2 #f
  (define the-map
    (call-with-input-file "inputs/03.txt" toboggan-map-load))
  (* (count-trees the-map 1 1)
     (count-trees the-map 3 1)
     (count-trees the-map 5 1)
     (count-trees the-map 7 1)
     (count-trees the-map 1 2)))

