;;; -----------------------------------------------
;;;  Sudoku Solver Simulated Annealing
;;;  Riik Acharya
;;; -----------------------------------------------


; starting temperature
(defconstant start-temp 10)

; cooling factor
(defconstant cooling-factor 0.8)

; constant representing infinity
(defconstant *INFINITY* 10000000)

; constraint array
(defparameter *cs* (make-array '(27 9)))

(dotimes (r 9)
  (dotimes (c 9)
    
    (setf (aref *cs* r c) 
      (+ (* 9 r) c))
    
    (setf (aref *cs* (+ r 9) c)
      (+ (* 9 c) r))
    
    (setf (aref *cs* (+ r 18) c)
      (+ (* 9 (+ (* 3 (floor (/ r 3))) (floor (/ c 3))))
	 (+ (mod c 3) (* 3 (mod r 3)))))
    ))

; represents a blank space, used as a default
(defconstant *blank* '_)


;;;  The GAME struct
;;; ------------------------------------------------------
;;;  Fields:
;;;     SLOTS  --  a vector of length 81, each element 
;;;                 representing one of the squares in a 9-by-9
;;;                 sudoku puzzle
;;;     NEW-INDEX1 -- first index to be switched next
;;;     NEW-INDEX2 -- second index to be switched
;;;     DUPLICATES -- the number of duplicates in the game
;;;


(defstruct (game)
  (slots (make-array 81 :initial-element *blank*))
  (new-index1 0)
  (new-index2 0)
  (duplicates *INFINITY*)
  )


;;;  INIT-GAME
;;; ----------------------------------------------------------
;;;  INPUT:  GRID, an optional list-of-lists representation of a sudoku puzzle
;;;  OUTPUT:  A GAME struct with the 81 slots initialized to the
;;;             values in the GRID, and the 81 domains initialized
;;;             to (1 2 3 4 5 6 7 8 9).

(defun init-game (&optional (grid nil))
  (let* ((g (make-game))
	 
	 ;; The 81-element vector of slots
	 (slots (game-slots g)))
    ;; use the GRID if provided...
    (when grid
      ;; First, convert the list-of-lists GRID into a 9-by-9 array, HARRY
      (let ((harry (make-array '(9 9) :initial-contents grid)))
	;; Then walk through the 9-by-9 array...
	(dotimes (r 9)
	  (dotimes (c 9)
	    ;; Set the SLOT to the corresponding element of the
	    ;; 9-by-9 array
	    (setf (aref slots (+ (* 9 r) c))
	      (aref harry r c))))))
   
    ;; Return the GAME struct
    g))

;;;  SCRAMBLE-LIST
;;; -----------------------------------------------------------
;;;  INPUT:  LISTY, a list
;;;  OUTPUT:  A list containing the same elements as LISTY, but
;;;           in a randomly scrambled order.
;;;  NOTE:  This function is provided for SCRAMBLE-ARRAY

(defun scramble-list (listy)
  ;; Base Case:  LISTY is empty
  (if (null listy)
      ;; So return the empty list
      ()
    ;; Recursive Case:  LISTY is non-empty
    (let* ((len (length listy))
	   (rnd (random len))
	   (item (nth rnd listy)))
      ;; Cons a randomly selected ITEM onto the scrambling of the
      ;; remaining elements
      (cons item
	    (scramble-list (remove item listy :count 1)))))
)

;;;  SCRAMBLE-ARRAY
;;; ----------------------------------------------------------
;;;  INPUT:  ARRAY, an array
;;;  OUTPUT:  a scrambled array
;;;
;;;  NOTE: Used to create new GAME structs for testing
;;;           purposes
;;;

(defun scramble-array (array)
  (setf listy '())
  (setf scrambled-array (make-array (length array) :initial-element nil))

  (dotimes (i (length array))
    (setf listy (cons (aref array i) listy))
  )

  (setf listy (scramble-list listy))
  
  (dotimes (i (length array))
    (setf (aref scrambled-array i) (first listy))
    (setf listy (rest listy))
  )

  scrambled-array
)

;;;  CONSTRAINT-NUM-DUPLICATES
;;; ----------------------------------------------------------
;;;  INPUTS:  GAME, a sudoku game
;;;           CONSTRAINT-NUM, the constraint which duplicates 
;;;               are being checked in
;;;  OUTPUT:  A number of the total amount of duplicates
;;;             in this constraint
;;;

(defun constraint-num-duplicates (game constraint-num)
   (setf duplicate? (possible-duplicate-array game constraint-num))
   (setf constraint-duplicate-counter 0)
    (dotimes (i 9)
      (when (= 0 (aref duplicate? i))
        (incf constraint-duplicate-counter)
      )
    )
  constraint-duplicate-counter
)

;;;  POSSIBLE-DUPLICATE-ARRAY
;;; ---------------------------------------------------------------
;;;  INPUT:  GAME, a sudoku game
;;;          CONSTRAINT-NUM, the number of a specific constraint
;;;  OUTPUT:  An array with the amount of times each index + 1
;;;             occurs in the constraint
;;; 

(defun possible-duplicate-array (game constraint-num)
  (setf duplicate? (make-array 9 :initial-element 0))
  (dotimes (i 9)
      (incf (aref duplicate? (- (aref (game-slots game) (aref *cs* constraint-num i)) 1)))
    )
  duplicate?
)

;;;  NUM-DUPLICATES
;;; ---------------------------------------------------------------
;;;  INPUT:  GAME, a sudoku game
;;;  OUTPUT:  A number of the amount of duplicates in all
;;;             constraints in total
;;; 

(defun num-duplicates (game)
  (let ((duplicate-counter 0))
    (dotimes (i 27)
      (incf duplicate-counter (constraint-num-duplicates game i))
    )
    duplicate-counter
  )
)

; Sample games for testing
; --------------------------------

(defparameter *t1* 
    '((2 9 1 5 4 3 7 8 9)
      (1 3 2 2 6 2 4 5 3)
      (3 2 3 9 3 4 3 2 1)
      (7 4 2 3 4 5 1 4 9)
      (5 1 2 6 5 4 5 7 5)
      (6 1 2 6 9 6 6 5 6)
      (8 7 8 7 4 7 9 6 5)
      (8 9 8 7 1 8 7 8 8)
      (8 7 1 4 9 3 9 6 1)))

(defparameter *t2* 
    '((1 1 1 1 1 1 1 1 1)
      (2 2 2 2 2 2 2 2 2)
      (3 3 3 3 3 3 3 3 3)
      (4 4 4 4 4 4 4 4 4)
      (5 5 5 5 5 5 5 5 5)
      (6 6 6 6 6 6 6 6 6)
      (7 7 7 7 7 7 7 7 7)
      (8 8 8 8 8 8 8 8 8)
      (9 9 9 9 9 9 9 9 9)))

(defparameter *t3* 
    '((1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)
      (1 2 3 4 5 6 7 8 9)))

;;;  UNSOLVABLE?
;;; ---------------------------------------------------------------
;;;  INPUT:  GAME, a sudoku game
;;;  OUTPUT:  T if there is more than 9 of any number on the board,
;;;             NIL otherwise
;;;  

(defun unsolvable? (game)
  (let ((num-numbers (make-array 10 :initial-element 0)))
    (dotimes (i 81)
      (if  (equal (aref (game-slots game) i) *blank*)
        (incf (aref num-numbers 0))
        (progn 
          (incf (aref num-numbers (aref (game-slots game) i)))
          
          (when (> (aref num-numbers (aref (game-slots game) i)) 9) 
            (return-from unsolvable? t)
          )
        )
      )
    )
  )
  nil
)

;;;  SWITCH-TWO-NUMBERS
;;; ------------------------------------------------------------------
;;;  INPUT:  GAME, a sudoku game
;;;          
;;;  SIDE-EFFECT:  GAME is updated so that the numbers at NEW-INDEX1
;;;                   and NEW-INDEX2 are swapped
;;;  

(defun switch-two-nums (game)
  (let ((new-num1 (aref (game-slots game) (game-new-index2 game))) 
        (new-num2 (aref (game-slots game) (game-new-index1 game))))
    (setf (aref (game-slots game) (game-new-index1 game)) new-num1)
    (setf (aref (game-slots game) (game-new-index2 game)) new-num2)
  )
)

;;;  PRINT-GAME
;;; ---------------------------------------------------------
;;;  INPUTS:  G, a GAME struct
;;;           STR, D -- ignored parameters
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Displays the sudoku game in the interactions window


(defun print-game (game)
  (dotimes (r 9)
    (dotimes (c 9)
      (format t "~A " (aref (game-slots game) (+ (* 9 r) c))))
    (format t "~%")))

;;;  CHOOSE-TWO-INDICES
;;; ---------------------------------------------------------
;;;  INPUTS:  GAME, a game struct
;;;           
;;;  SIDE EFFECT:  Sets NEW-INDEX1 and NEW-INDEX2 of
;;;                   GAME to new values 
;;;

(defun choose-two-indices (game)
  (let ((first-num 10) 
        (second-num 10)
        (first-constraint 27)
        (second-constraint 27)
        (index1 81)
        (index2 81)
        (best-value (num-duplicates game)))

    (setf first-constraint-list (generate-random-permutation 27))
    
    (setf second-constraint-list (generate-random-permutation 27))

      ; find the constraint with the first duplicate
    (dotimes (constraint 27)

        (setf i (aref first-constraint-list constraint))

        (dotimes (j 9)

          (when (> (aref (possible-duplicate-array game i) j) 1)
            
            (setf first-num (+ j 1))
          
            (setf first-constraint i)

            ; find an occurence of first-num in this constraint
            (dotimes (k 9)

              (when (= first-num (aref (game-slots game) (aref *cs* i k))) 
                  (setf index1 (aref *cs* i k))
                  (setf (game-new-index1 game) index1)
                 
                  ; so it doesn't always choose first occurence of the number
                  (when (= 0 (random 2))
                    (return)
                  )
                  
                )
                
            )

            ; if the index has been found, exit the loop
            (when (not (= 81 index1))
              (return)
            )
          )
        )

        (when (not (= 81 index1))
            (return)
          )
    )

    ; try to find the best constraint which has at least 2 of a missing number from the earlier
    ; constraint, and 0 of first-num
    (dotimes (constraint 27)

        (setf i (aref second-constraint-list constraint))

        (dotimes (j 9)
          (when (and (= 0 (aref (possible-duplicate-array game i) (- first-num 1))) 
                      (> (aref (possible-duplicate-array game i) j) 1)
                      (= 0 (aref (possible-duplicate-array game first-constraint) j))
                  )

                (setf second-num (+ j 1))

                (setf second-constraint i)

                ; find the index of the first instance-of second-num in second-constraint
                (dotimes (x 9)
                  
                  (when (= second-num (aref (game-slots game) (aref *cs* second-constraint x)))
                        
                    (setf (game-new-index2 game) (aref *cs* second-constraint x))

                    ; tries move
                    (switch-two-nums game)

                    ; updates index2 based on if it's less than the best value
                    (when (> best-value (num-duplicates game))
                      (setf best-value (num-duplicates game))
                      (setf index2 (aref *cs* second-constraint x))
                    )

                    ; undoes the move
                    (switch-two-nums game)

                  )
                )
            )
        )
        
    )

    ; if it found any move that would yield a better result, return
    (when (not (or (= 81 index1) (= 81 index2)))
        (setf (game-new-index2 game) index2)
        (return-from choose-two-indices)
      )

    (setf index2 (game-new-index2 game))

    ; if not, find the best move that switches the duplicated number with any 
    ; number which has a duplicate in another constraint
    (dotimes (constraint 27)

      (setf i (aref second-constraint-list constraint))

          (when (= 0 (aref (possible-duplicate-array game i) (- first-num 1))) 
             
            ; when you find the second constraint with none of first-num, 
            ; find the first number that has a duplicate
            (dotimes (k 9)
              (when (and (> (aref (possible-duplicate-array game i) k) 1)

                  ; make sure first-num and second-num are not equal
                  (not (= k (- first-num 1))))

                (setf second-num (+ k 1))
                (setf second-constraint i)

               
                ; find the index of the first instance-of second-num in second-constraint
                (dotimes (x 9)
                  

                  (when (= second-num (aref (game-slots game) (aref *cs* second-constraint x)))
                          
                        (setf (game-new-index2 game) (aref *cs* second-constraint x))
                        
                        ; tries move
                        (switch-two-nums game)
                      
                        ; updates index2 based on if it yields best value
                        (when (> best-value (num-duplicates game))
                          (setf best-value (num-duplicates game))
                          (setf index2 (aref *cs* second-constraint x))

                        )

                        ; undoes the move
                        (switch-two-nums game)
                  )
                )
                
                
              )
            )
          )
    )

    ; if better move was found update index2 of game to do it
    ; if no better move was found keep the same index2
    (setf (game-new-index2 game) index2)
   
  )
)

;;;  SIMULATED-ANNEALING-SOLVE
;;; ----------------------------------------------------------------
;;;  INPUTS:  GAME, a game struct
;;;           NUM-ITERATIONS, the number of times this
;;;                   function has been called previously
;;;  OUTPUT: GAME if the game has been solved or if enough
;;;                   moves have been made
;;;
;;;  SIDE EFFECT:  Switches numbers in SLOTS, prints NUM-DUPLICATES,
;;;                   prints game as 9 by 9 board
;;; 

(defun simulated-annealing-solve (game temp cool num-iterations)

  (cond
    ((unsolvable? game)
      nil)

    ((<= (- (num-duplicates game) (* 0.001 num-iterations)) 0) 
      (format t "~%")
      (format t "Duplicates: ~A" (num-duplicates game))
      (format t "~%")
      (print-game game) 
      game)

    (t 

        (if (and (>= (num-duplicates game) (game-duplicates game)) 
              (> temp (random start-temp))
              )
            (progn 
              (dotimes (i 5)

                (setf (game-new-index1 game) (random 81))
                (setf (game-new-index2 game) (random 81))
                
                (setf (game-duplicates game) (num-duplicates game))

                (switch-two-nums game)
              )
            )

            (progn 
                (when (>= (num-duplicates game) (game-duplicates game)) 
                  (switch-two-nums game)
                )

                (choose-two-indices game)
                
                (setf (game-duplicates game) (num-duplicates game))
                (switch-two-nums game)
            )
        )

      (setf temp (* cool temp))

      (simulated-annealing-solve game temp cool (+ 1 num-iterations))
    )
  )
)

;;;  GENERATE-RANDOM-PERMUTATION
;;; ----------------------------------------------------------------
;;;  INPUT:  N, an integer
;;;  OUTPUT: PERMUTATION, a random list of integers from 
;;;              0 through N-1
;;;
;;;  NOTE:  Function only used to generate random constraints
;;;              for choosing numbers to swap
;;;


(defun generate-random-permutation (n)
  (setf range '())
  (setf permutation (make-array n :initial-element nil))

  (dotimes (i n)
    (setf range (cons i range))
  )

  (dotimes (i n)
    (setf rand-idx (random (length range)))
    (setf (aref permutation i) (elt range rand-idx))
    (setf range (remove (elt range rand-idx) range))
  )

  permutation
)



; Testing
;-----------------------------------------

(print "Game 1:")
(setf g1 (init-game *t1*))
(print "Before")
(format t "~%")
(format t "Duplicates: ~A" (num-duplicates g1))
(format t "~%")
(print-game g1)
(print "After")
(simulated-annealing-solve g1 start-temp cooling-factor 0)

(print "----------------")
(format t "~%")

(print "Game 2:")
(setf g2 (init-game *t2*))
(print "Before")
(format t "~%")
(format t "Duplicates: ~A" (num-duplicates g2))
(format t "~%")
(print-game g2)
(print "After")
(simulated-annealing-solve g2 start-temp cooling-factor 0)

(print "----------------")
(format t "~%")

(print "Game 3:")
(setf g3 (init-game *t3*))
(print "Before")
(format t "~%")
(format t "Duplicates: ~A" (num-duplicates g3))
(format t "~%")
(print-game g3)
(print "After")
(simulated-annealing-solve g3 start-temp cooling-factor 0)

(print "----------------")
(format t "~%")

(print "Game 4:")
(setf g4 (make-game :slots (scramble-array (game-slots g1))))
(print "Before")
(format t "~%")
(format t "Duplicates: ~A" (num-duplicates g4))
(format t "~%")
(print-game g4)
(print "After")
(simulated-annealing-solve g4 start-temp cooling-factor 0)

(print "----------------")
(format t "~%")

(print "Game 5:")
(setf g5 (make-game :slots (scramble-array (game-slots g2))))
(print "Before")
(format t "~%")
(format t "Duplicates: ~A" (num-duplicates g5))
(format t "~%")
(print-game g5)
(print "After")
(simulated-annealing-solve g5 start-temp cooling-factor 0)

; to compile and run file:
;(compile-file "sudoku-solver-simulated-annealing.lisp") (load "sudoku-solver-simulated-annealing")
