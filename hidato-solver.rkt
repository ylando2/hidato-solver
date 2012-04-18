#lang slideshow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programmer: Yuval Lando ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/draw)

(require racket/require (for-syntax racket/base)
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define HOLE 2000) ;; Put a big number here, bigger then the greatest number on the board.

(define COL-NUM 10)
(define *puzzle*
  '(
       xx xx 00 00 00 00 00 00 xx xx
       xx 00 04 00 48 47 00 54 00 xx
       01 02 07 xx 44 45 xx 38 00 00
       78 00 00 00 00 42 00 00 00 57
       00 00 00 72 00 40 00 00 00 00
       00 00 00 00 00 00 00 59 31 00
       75 00 00 xx 00 61 xx 30 00 00
       12 00 00 63 xx xx 00 24 00 27
       xx xx 00 15 17 20 00 25 xx xx
       xx xx xx 00 18 00 00 xx xx xx
       ))

;;Turn the data of the puzzle to vector.
(define (build-board puzzle col-num)
  (let* ([row-num (+ 2 (fxquotient (length puzzle) col-num))]
         [col-num+2 (+ col-num 2)]
         [len (fx* col-num+2 row-num)]        
         [vec (make-vector len HOLE)]
         [len2 (- len col-num+2)])
    (let loop ([i col-num+2] [lst puzzle])
      (when (fx< i len2)
        (if (and (fx> (fxremainder i col-num+2) 0) (fx< (fxremainder i col-num+2) (fx- col-num+2 1)))
          (begin 
            (if (eq? (car lst) 'xx)
                (fxvector-set! vec i HOLE)
                (fxvector-set! vec i (car lst)))
            (loop (fx+ i 1) (cdr lst)))
          (loop (fx+ i 1) lst))))
    vec))

;;Print for debug
(define (print-board vec col-num)
  (let* ([col-num+2 (+ col-num 2)]
         [row-num (fxquotient (vector-length vec) col-num+2)])
    (for ([j (in-range row-num)])
      (for ([i (in-range col-num+2)])
        (let ([e (fxvector-ref vec (fx+ i (fx* j col-num+2)))])
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
(define (dist pos1 pos2 col-num+2)
  (let ([x1 (fxremainder pos1 col-num+2)]
        [y1 (fxquotient pos1 col-num+2)]
        [x2 (fxremainder pos2 col-num+2)]
        [y2 (fxquotient pos2 col-num+2)])
    (max (abs (fx- x1 x2)) (abs (fx- y1 y2)))))

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
                (let ([new-all (every-path board col-num+2 num+1 move next-pos energy-1
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
                 [all6 (check-move (fx+ pos (fx- col-num+2 1)) all5)]
                 [all7 (check-move (fx- pos (fx+ col-num+2 1)) all6)]
                 [all8 (check-move (fx- pos (fx- col-num+2 1)) all7)])
              all8)))))

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
              (append (reverse (cons '() acc)) 
                      (cdr choices)) ;;If it discover that it fail stop.
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

;;If there is a number in the board,
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
  (let ([col-num+2 (+ 2 max-size)])
    (let loop ()
      (let* ([nums (puzzle-numbers board)]
             [choices (every-choice board col-num+2 nums 6)]
             [sol (cdr (solve-one-path choices nums '()))])
        (unless (null? sol)
          (update-board board sol)
          (let* ([nums (puzzle-numbers board)]
                 [choices (every-choice board col-num+2 nums 6)])
            (update-num-in-all-paths board choices nums)
            (loop)))))
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
      (update-board board (solve-all choices2 nums2 '())))))

;;;;;;;;;;; Helper drawing functions ;;;;;;;;;;;;;;;;;;;; 
(define (square size)
  (filled-rectangle size size))

(define (black-square size)
  (frame (colorize (square size) "black") #:color "black"))

(define (red-square size)
  (frame (colorize (square size) "red") #:color "black"))

(define (brown-square size)
  (frame (colorize (square size) "brown") #:color "black"))

(define (white-square size)
  (frame (colorize (square size) "white") #:color "black"))

(define (draw-string str xoff yoff size)
  (let* ([text-target (make-bitmap size size)]
         [dc (new bitmap-dc% [bitmap text-target])])
    (send dc set-font (make-object font% (- size 18) 'roman 'normal))
    (send dc draw-text str xoff yoff)
    (frame (bitmap text-target) #:color "black")))

(define (draw-number num)
  (let ([str (number->string num)])
    (cond
      [(< num 10) (draw-string str 10 6 30)]
      [(< num 100) (draw-string str 7 6 30)]
      [else (draw-string str 3 6 30)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Draw the board on the repl.
(define (draw-board board col-num)
  (let* ([col-num+2 (+ col-num 2)]
         [row-num (- (fxquotient (vector-length board) col-num+2) 2)])
    (apply
     vc-append
     (for/list ([j (in-range 1 (+ 1 row-num))])
       (apply 
        hc-append
        (for/list ([i (in-range 1 (+ 1 col-num))])
          (let ([val (vector-ref board (+ i (* j col-num+2)))])
            (cond 
              [(= val HOLE) (brown-square 30)]
              [(= val 0) (white-square 30)]
              [else (draw-number val)]))))))))

(time
 (let ([board (build-board *puzzle* COL-NUM)])
   (vc-append
   (draw-board board COL-NUM)
   (blank 10 50)
   (begin
     (solve board COL-NUM)
     (draw-board board COL-NUM)))))                     