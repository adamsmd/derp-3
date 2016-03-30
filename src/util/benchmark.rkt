#lang racket

(provide benchmark)

(require srfi/48)
(require racket/unsafe/ops)
(require math/statistics)

(define (eng num)
  (if (= num 0)
      "   0.00e+0"
      (let ()
        (define n (abs (+ 0.0 num)))
        (define e (let loop ([e 0])
                    (cond
                     [(>= n (expt 10 (+ e 3))) (loop (+ e 3))]
                     [(< n (expt 10 e)) (loop (- e 3))]
                     [else e])))
        (format "~a~6,2Fe~a~s"
                (if (< num 0) "-" " ")
                (/ n (expt 10 e))
                (if (>= e 0) "+" "")
                e))))

(define (benchmark rounds drops min-cpu-time f)
  (define (run f n)
    (if (< n 10)
        (let loop ([i n])
          (unless (eq? i 0)
            (f)
            (loop (unsafe-fx- i 1))))
        (let loop ([i n])
          (unless (eq? i 0)
            (f) ; 1
            (f) ; 2
            (f) ; 3
            (f) ; 4
            (f) ; 5
            (f) ; 6
            (f) ; 7
            (f) ; 8
            (f) ; 9
            (f) ; 10
            (loop (unsafe-fx- i 10))))))

  (define (choose-iteration-count f n)
    (let-values ([(res cpu real gc) (time-apply run (list f n))])
      (if (and (> (/ cpu 1000) min-cpu-time)
               ;; Confirm it wasn't a timing fluke
               (let-values ([(res1 cpu1 real1 gc1) (time-apply run (list f n))]
                            [(res2 cpu2 real2 gc2) (time-apply run (list f n))]
                            [(res3 cpu3 real3 gc3) (time-apply run (list f n))])
                 (and (> (/ cpu1 1000) min-cpu-time)
                      (> (/ cpu2 1000) min-cpu-time)
                      (> (/ cpu3 1000) min-cpu-time))))
          n
          (choose-iteration-count f
            (case n
              [(1) 3]
              [(3) 10]
              [else (* 2 n)])))))

  (define results '())

  (printf "Choosing iteration count ...") (flush-output)

  (define iteration-count (choose-iteration-count f 1))

  (printf " ~s~n" iteration-count)

  (printf "Calculating benchmark overhead ...") (flush-output)

  (let-values ([(res1 cpu1 real1 gc1) (time-apply run (list f iteration-count))]
               [(res2 cpu2 real2 gc2) (time-apply run (list (lambda () 0) iteration-count))])
    (display (format " ~0,2F%" (* 100 (/ cpu2 cpu1)))) (newline))

  (printf "Running benchmarks ...\n") (flush-output)

  (let loop ([i 0])
        (unless (= i rounds)
          (let-values ([(res cpu real gc) (time-apply run (list f iteration-count))])
            (set! results (cons (map (lambda (x) (exact->inexact (/ x 1000 iteration-count))) (list cpu real gc)) results))
            (printf "~s of ~s: cpu=~a real=~a gc=~a cpu-gc=~a [seconds]\n" (+ 1 i) rounds (eng (caar results)) (eng (cadar results)) (eng (caddar results)) (eng (- (caar results) (caddar results)))) (flush-output)
            (loop (+ 1 i)))))

  (define (minimum xs) (apply min xs))
  (define (maximum xs) (apply max xs))
  (define (spread xs) (- (maximum xs) (minimum xs)))
  (define (dropping xs)
    (define ys (sort xs <))
    (mean (take (drop ys drops) (- (length xs) drops drops))))

  (define (map-results f)
    (define (g x) (- (car x) (caddr x)))
    (list (eng (f (map car results))) (eng (f (map cadr results))) (eng (f (map caddr results))) (eng (f (map g results)))))

  (printf "--------------------------------------------------------------------------------~n")
  (apply printf "mean:   cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results mean))
  (apply printf "drop ~a: cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" drops (map-results dropping))
  (apply printf "median: cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results (lambda (xs) (median < xs))))
  (apply printf "min:    cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results minimum))
  (apply printf "max:    cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results maximum))
  (apply printf "stddev: cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results stddev))
  (apply printf "spread: cpu=~a real=~a gc=~a cpu-gc=~a [seconds]~n" (map-results spread)))
