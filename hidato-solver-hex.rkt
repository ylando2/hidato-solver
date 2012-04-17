#lang slideshow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programmer: Yuval Lando ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;I marked the changes from the square hidato solver with *

(require racket/draw)

(require racket/require (for-syntax racket/base)
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define HOLE 2000) ;; Put a big number here, bigger then the greatest number on the board.

(define BASE-SIZE 6)
(define MAX-SIZE 11)
(define *puzzle* 
  '(
         00 00 00 00 00 00
        00 00 00 82 00 47 46
       00 55 00 87 00 80 00 00
      00 57 91 00 00 00 00 00 00
     00 59 00 00 00 00 00 34 00 00
    00 61 00 00 74 00 76 00 00 41 40
     00 00 71 00 00 00 00 00 37 00
      00 00 00 22 00 25 27 00 00
       00 00 00 21 01 00 00 00
        00 16 00 12 08 00 00
         00 00 00 00 00 00
         ))

;;Pad the puzzle to fit a square shape.
(define (square-shape puzzle base-size max-size)
  (define (upper-side i lst acc) 
    (if (< i max-size)
        (let-values ([(row rest) (split-at lst i)])
          (upper-side 
           (+ i 1)
           rest
           (append acc row (make-list (- max-size i) HOLE))))
        (values i lst acc)))
  (define (lower-side i lst acc) 
    (if (>= i base-size)
        (let-values ([(row rest) (split-at lst i)])
          (lower-side
           (- i 1)
           rest
           (append acc (make-list (- max-size i) HOLE) row)))
        acc))
  (let-values ([(i rest acc) (upper-side base-size puzzle '() )])
    (lower-side i rest acc)))

;;Turn the data of the puzzle to vector.
(define (build-board puzzle base-size max-size)
  (let* ([puzzle (square-shape puzzle base-size max-size)] ;; *
         [row-num (+ 2 (fxquotient (length puzzle) max-size))]
         [col-num+2 (+ max-size 2)]
         [len (fx* col-num+2 row-num)]        
         [vec (make-vector len HOLE)]
         [len2 (- len col-num+2)])
    (let loop ([i col-num+2] [lst puzzle])
      (when (fx< i len2)
        (if (and (fx> (fxremainder i col-num+2) 0) 
                 (fx< (fxremainder i col-num+2) (fx- col-num+2 1)))
          (begin 
            (if (eq? (car lst) 'xx)
                (fxvector-set! vec i HOLE)
                (fxvector-set! vec i (car lst)))
            (loop (fx+ i 1) (cdr lst)))
          (loop (fx+ i 1) lst))))
    vec))

;;Print for debug.
(define (print-board board base-size max-size)
  ;;print upper part
  (for ([j (in-range base-size max-size)]) 
    (for ([i (in-range (- max-size j))])
      (display "  "))
    (for ([i (in-range j)])
      (let ([val (vector-ref board (+ 1 i (* (- j base-size -1) (+ max-size 2))))])
        (if (< val 10)
            (printf "0~a  " val)
            (printf "~a  " val))))
    (newline))
  ;;print lower part
  (for ([j (in-range max-size (- base-size 1) -1)])
    (for ([i (in-range (- max-size j))])
      (display "  "))
    (for ([i (in-range j)])
      (let ([val (vector-ref board (+ (- max-size j) 1 i (* (+ (- max-size j) base-size) (+ max-size 2))))])
        (if (< val 10)
            (printf "0~a  " val)
            (printf "~a  " val))))
    (newline)))
    

;;Print for debug, see how it put the hexagon data on a vector
;;that represent 2d matrix.
(define (print-board2 vec max-size)
  (let ([row-num (fxquotient (vector-length vec) (+ max-size 2))])
    (for ([j (in-range row-num)])
      (for ([i (in-range (+ max-size 2))])
        (let ([e (fxvector-ref vec (fx+ i (fx* j (+ max-size 2))))])
          (if (fx= e HOLE)
              (display "xx ")
              (if (fx< e 10)
                  (printf "0~a " e)
                  (printf "~a " e)))))
      (newline))))

;;Find the position of a number on the board
(define (board-find vec val)
  (for/or ([i (in-range (vector-length vec))])
    (if (fx= (fxvector-ref vec i) val)
        i
        #f)))

;;Distance witout obstacle.
;; *
(define (dist pos1 pos2 col-num+2)
  (let ([x1 (fxremainder pos1 col-num+2)]
        [y1 (fxquotient pos1 col-num+2)]
        [x2 (fxremainder pos2 col-num+2)]
        [y2 (fxquotient pos2 col-num+2)])
    (cond
      [(and (fx> y1 y2) (fx< x1 x2)) (fx+ (fx- y1 y2) (fx- x2 x1))]
      [(and (fx< y1 y2) (fx> x1 x2)) (fx+ (fx- y2 y1) (fx- x1 x2))]
      [else (max (abs (fx- x1 x2)) (abs (fx- y1 y2)))])))

;;Return all the numbers on the board
(define (puzzle-numbers vec)
  (sort
   (for/list ([e (in-vector vec)] #:when (and (fx> e 0) (fx< e HOLE)))
     e) 
   fx<))

;;Return every path for a number "num" in position "pos" next number in "next-pos"
;;It need to move "energy" moves.
;;"cur" is the current path, "all" are all the paths it find.
;;When calling this function from outside "cur" and "all" variables must be equal to null.
(define (every-path board col-num+2 num pos next-pos energy cur all)
  (if (fx= energy 1)
      (if (fx= (dist pos next-pos col-num+2) 1)
          (cons (reverse cur) all)
          all)
      (let ([num+1 (fx+ num 1)] [energy-1 (fx- energy 1)])
        (define (check-move move old-all)
          (if (fx= (fxvector-ref board move) 0)
              (begin
                (fxvector-set! board move num+1)
                (let ([new-all (every-path board  col-num+2 num+1 move next-pos energy-1
                                           (cons move cur) old-all)])  
                  (fxvector-set! board move 0)
                  new-all))
              old-all))
        (if (fx< energy (dist pos next-pos col-num+2))
            all
            (let* 
                ([all1 (check-move (fx- pos 1) all)]
                 [all2 (check-move (fx+ pos 1) all1)]
                 [all3 (check-move (fx+ pos col-num+2) all2)]
                 [all4 (check-move (fx- pos col-num+2) all3)]
                 [all5 (check-move (fx+ pos (fx+ col-num+2 1)) all4)]
                 ;; * 
                 ;[all6 (check-move (fx+ pos (fx- col-num+2 1)) all5)] 
                 [all7 (check-move (fx- pos (fx+ col-num+2 1)) all5)]) ;all6)]
                 ;[all8 (check-move (fx- pos (fx- col-num+2 1)) all7)])
              all7))))) ;8)))))

;;Find every path start with the numbers in nums by calling every-path.
;;It skip paths that are longer then max-energy.
;;It return list of list of paths; a list for every number in nums.
;;Every path is represent as a list of numbers so it return list of list of list of numbers.
(define (every-choice board col-num+2 nums max-energy)
  (let loop ([lst nums] [acc '()])
    (if (null? (cdr lst))
        (reverse acc)
        (let ([cur (car lst)] [next (cadr lst)])
          (loop (cdr lst) 
                (cons 
                 (if (< (- next cur) max-energy)
                     (every-path board 
                                 col-num+2
                                 cur 
                                 (board-find board cur)
                                 (board-find board next)
                                 (- next cur)
                                 '() '() )
                     '(()) );;place holder list of paths
                 acc))))))

;;Return true if the list "lst" has only one element.
(define (length=1 lst)
  (and (not (null? lst)) (null? (cdr lst))))

;;Build a list of pairs (start-num . path) for every choice with only one possible path 
;;where path is a list of numbers.
(define (only-one-path choices nums)
  (if (null? choices)
      #f
      (if (and (length=1 (car choices)) (not (null? (caar choices))))
          (cons (car nums) (caar choices))
          (only-one-path (cdr choices) (cdr nums)))))

;;Get list of pairs (start-num . path ) put the numbers of the paths on the board.
(define (update-board board lst)
  (for ([e (in-list lst)])
    (let loop ([num (fx+ (car e) 1)]
               [path (cdr e)])
      (unless (null? path)
        (fxvector-set! board (car path) num)
        (loop (fx+ num 1) (cdr path))))))

;(define (remove-choices nums choices)
;  (map
;   (lambda (lst)
;     (filter
;      (lambda (path-lst)
;        (not (memf (lambda (e) (memv e nums)) path-lst)))
;      lst))
;   choices))

;;Remove paths from the list of list of paths "choices" that contain the number of "nums".
(define (remove-choices2 nums choices)
  (let loop ([choices choices] [acc '()])
    (if (null? choices)
        (reverse acc)
        (let ([new-choice (filter
                           (lambda (path-lst)
                             (not (memf (lambda (e) (memv e nums)) path-lst))) 
                           (car choices) )])
          (if (null? new-choice)
              (append (reverse (cons '() acc)) (cdr choices)) ;;Fail
              (loop (cdr choices) (cons new-choice acc)))))))

;;Turn the choices for the path that start at n to solved ones.
;;This is done so it will not remove its paths and
;;calculate it as failure.
(define (remove-one-path-rec choices n nums)
  (if (fx= n (car nums)) 
      (cons '(()) (cdr choices)) 
      (cons (car choices) (remove-one-path-rec (cdr choices) n (cdr nums))))) ;)

;;Solve the problem for the case where the a number in nums
;;has only one path to go to.
;;It return a cons ( remaining-choices . solution-to-update ) 
;;where "solution-to-update" is a list of ( start-number . path )
(define (solve-one-path choices nums acc)
    (let loop ([c choices]
               [acc acc])
      (let* ([one-path (only-one-path c nums)])
        (if (not one-path)
            (cons c acc)
            (loop (remove-choices2 (cdr one-path) (remove-one-path-rec c (car one-path) nums)) (cons one-path acc))))))

;;If a there is a number in the board,
;;that do not have any path. It return true.
;;This is a way to know that it have to try other ways.
(define (fail? choices)
  (memf null? choices))

;;Return a copy of the list "lst" that does not contain
;;the last element of the list.
(define (but-last-rec lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (but-last-rec (cdr lst)))))

;;Search for a solution by trying every combination of paths.
;;It return a list of ( first-number . path ).
(define (solve-all choices nums acc)
  (if (null? choices)
      acc
      (if (fail? choices)
          #f
          (let* 
              ([choices2+acc2 (solve-one-path choices nums acc)]
               [choices2 (car choices2+acc2)]
               [acc2 (cdr choices2+acc2)])
            (if (fail? choices2)
                #f
                (let ([n (car nums)])
                  (let loop ([paths (car choices2)])
                    (if (null? paths) 
                        #f
                        (let* ([choices3 (remove-choices2 (car paths) (cdr choices2))]
                               [sol (solve-all choices3 (cdr nums) (cons (cons n (car paths)) acc2))])
                          (if sol
                              sol
                              (loop (cdr paths))))))))))))
                    
;;One more elimination is to search all the paths that start at a given number
;;If there is a number x at position p for all those paths it must be in the solution
;;So the program update the board to contain that number.
(define (update-num-in-all-paths board choices nums)
  (for ([c (in-list choices)] [n (in-list nums)])
    (let ([h (make-hash)])
      (for ([p (in-list c)])
        (for ([e (in-list p)] [n2 (in-naturals (+ n 1))])
          (hash-set! h (cons n2 e) (+ (hash-ref h (cons n2 e) 0) 1))))
      (let ([len (length c)])
        (for ([(k v) (in-hash h)])
          (when (= v len) (vector-set! board (cdr k) (car k))))))))

;;Solve the puzzle.   
(define (solve board max-size)
  (let* ([nums (puzzle-numbers board)]
         [col-num+2 (+ 2 max-size)]
         [choices (every-choice board col-num+2 nums 6)])
    (update-board board (cdr (solve-one-path choices nums '())) )
    (update-num-in-all-paths board choices nums)
    (let* ([nums (puzzle-numbers board)]
           [choices (every-choice board col-num+2 nums 6)])
      (update-board board (cdr (solve-one-path choices nums '())))  
      (let* ([nums (puzzle-numbers board)] 
             [choices (every-choice board col-num+2 nums HOLE)]
             [nums- (but-last-rec nums)]
             [nums.choices (map (lambda (x y) (cons x y)) nums- choices)]
             [nums.choices- (filter (lambda (x) (not (and (length=1 (cdr x)) (null? (cadr x))))) nums.choices)]
             [sorted (sort nums.choices- 
                           (lambda (x y)
                             (let ([len1 (length (cdr x))]
                                   [len2 (length (cdr y))])
                               (if (= len1 len2)
                                   (> (length (car (cdr x))) 
                                      (length (car (cdr y))))
                                   (< len1 len2)))))]
             [nums2 (map car sorted)]
             [choices2 (map cdr sorted)])
        (update-board board (solve-all choices2 nums2 '()))))))

;;;;;;;;;;; Helper drawing functions ;;;;;;;;;;;;;;;;;;;; 
(define (draw-string str xoff yoff size dc)
    (send dc set-font (make-object font% (- size 18) 'roman 'normal))
    (send dc draw-text str xoff yoff))

(define (draw-number num xoff yoff dc)
  (when (> num 0)
    (let ([str (number->string num)])
      (cond
        [(< num 10) (draw-string str (+ 12 xoff) (+ 11 yoff) 30 dc)]
        [(< num 100) (draw-string str (+ 9 xoff) (+ 11 yoff) 30 dc)]
        [else (draw-string str (+ 5 xoff) (+ 11 yoff) 30 dc)]))))

(define (draw-hex edge xoffset yoffset dc)
    (let* ([sq3 (sqrt 3)]
           [width (floor (* sq3 edge))]
           [1/2edge (floor (* 1/2 edge))]
           [points (map (lambda (x) (cons (inexact->exact (car x))  
                                          (inexact->exact (cdr x)))) 
                        (list (cons 0 1/2edge) (cons (/ width 2) 0) 
                              (cons width 1/2edge) (cons width (* 3 1/2edge))
                              (cons (/ width 2) (* 4 1/2edge)) (cons 0 (* 3 1/2edge)) ))])
    (send dc draw-polygon points xoffset yoffset)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Draw the board on the repl.
(define (draw-solution board edge base-size max-size)
  (let* ([sq3 (sqrt 3)]
         [width (inexact->exact (floor (* sq3 edge)))]
         [yadd (inexact->exact (floor (* 3/2 edge)))]
         [target (make-bitmap (+ (* max-size width) 1)  
                              (+ (* 2 edge) (* 2 yadd (- max-size base-size)) 1))]
         [dc (new bitmap-dc% (bitmap target))])
    (for ([j (in-range base-size max-size)])
      (for ([i (in-range j)])
        (let ([xoff (+ (* (- max-size j) width 1/2) (* i width))]
              [yoff (* (- j base-size) yadd)]
              [val (vector-ref 
                    board 
                    (+ i 1 (* (- j base-size -1) (+ max-size 2))))])
          (draw-hex edge xoff yoff dc)
          (draw-number val xoff yoff dc))))
    (for ([j (in-range max-size (- base-size 1) -1)])
      (for ([i (in-range j)])
        (let ([xoff (+ (* (- max-size j) width 1/2) (* i width))]
              [yoff (* (+ (- max-size base-size) (- max-size j)) yadd)]
              [val (vector-ref 
                    board 
                    (+ (+ (- max-size j) 1 i) 
                       (* (+ base-size (- max-size j)) (+ max-size 2))))])
          (draw-hex edge xoff yoff dc)
          (draw-number val xoff yoff dc))))
    (bitmap target)))

(time
 (let ([board (build-board *puzzle* BASE-SIZE MAX-SIZE)])
   (vc-append
   (hc-append (blank 200 10) (draw-solution board 20 BASE-SIZE MAX-SIZE))
   (blank 10 50)
   (begin
     (solve board MAX-SIZE)
     (hc-append (blank 200 10) (draw-solution board 20 BASE-SIZE MAX-SIZE))))))
