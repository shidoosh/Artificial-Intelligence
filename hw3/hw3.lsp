;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; Returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

;
; contains (item row)
; Returns T if item is found in the row, else NIL
; 
(defun contains (item row)
	(cond
		((null row) NIL)
		((equal item (car row)) t)
		(t (contains item (cdr row)))
	)
)

; 
; goal-test (s)
; Returns NIL if there is at least one box that's not on a star, 
; otherwise T (we have reached our goal). 
;
(defun goal-test (s)
  (cond
  	((null s) t)
  	((contains box (car s)) NIL)
  	(t (goal-test (cdr s)))
  )
 );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Checks that position is not invalid, such that no row or column is less than 0
; or now coordinate is greater than given board dimensions
(defun isInvalidPos (s pos)
  (or 
  	(< (car pos) 0) 
	(> (car pos) (- (length (first s)) 1))
	(< (cadr pos) 0) 
  	(> (cadr pos) (- (length s) 1)) 

  )
)

; Searches row, returns the item at column
(defun getRow (r c)
	(cond
		((null r) nil)
		((= 0 c) (car r))
		(t (getRow (cdr r) (- c 1)))
	)
)


; Searches for item at (r, c) 
(defun getCoor (s pos)
	(cond
		((isInvalidPos s pos) wall) ; return a number, can't return NIL
		((= 0 (cadr pos)) (getRow (car s) (car pos)))
		(t (getCoor (cdr s) (list (car pos) (- (cadr pos) 1))))
	)
)

; Checks that position is viable for a move (next cannot be a taken item)
(defun isValidPos (curr next)
	(and (or (isBox curr) (isBoxStar curr)) (or (isStar next) (isBlank next)))
)


; Searches row to find the corresponding column index for item to be assigned in assign Coor
(defun getColItem (r c item)
	(cond
		((null r) nil)
		((= 0 c) (cons item (cdr r)))
		(t (cons (car r) (getColItem (cdr r) (- c 1) item)))
	)
)

 

; Assigns the coordinate (r, c) with specified item, given a state, returns new state
(defun assignCoor (s col row item)
	(cond
		((null s) nil)
		((= 0 row) (cons (getColItem (car s) col item) (cdr s)))
		(t (cons (car s) (assignCoor (cdr s) col (- row 1) item)))
	)

)

; moves player or box from curr to next. Updates current coordinate if curr is a star
; and move occurs. 
(defun move (s curr next onstar nostar)
        (cond
                ((= nostar (getCoor s curr))
                 (cond
                  ((isStar (getCoor s next)) (assignCoor (assignCoor s (car curr) (cadr curr) star) (car next) (cadr next) onstar))
                  (t (assignCoor (assignCoor s (car curr) (cadr curr) blank) (car next) (cadr next) nostar))
                 )
                )

                (t 
                 (cond
                  ((isStar (getCoor s next)) (assignCoor (assignCoor s (car curr) (cadr curr) blank) (car next) (cadr next) onstar))
                  (t (assignCoor (assignCoor s (car curr) (cadr curr) star) (car next) (cadr next) nostar))
                 )
                )
        )
)


; moves player from current position to next desired position 
(defun movePlayer (s curr next) 
	(move s curr next keeperstar keeper)
)

; moves box  from current position to next desired position 
(defun moveBox (s curr next)
	(move s curr next boxstar box)
)



; messy code, I know, but my let statement was adding time so I just replaced vars 
; corresponding function calls 
; Tries to move player in direction specified. Considers valid moves, and pushes box in
; the direction the player moved. If move is invalid, returns nil. 
(defun try-move (s direction)
	(let* ((pos (getKeeperPosition s 0)))															   
	      
	(cond
		((or (isStar (getCoor s (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos))))) (isBlank (getCoor s (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos)))))) (movePlayer s (getKeeperPosition s 0) (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos)))))
		((isValidPos (getCoor s (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos)))) (getCoor s (list (+ (car direction) (car (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos))))) (+ (cadr direction) (cadr (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos)))))))) (movePlayer (moveBox s (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos))) (list (+ (car direction) (car (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos))))) (+ (cadr direction) (cadr (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos))))))) (getKeeperPosition s 0) (list (+ (car direction) (car pos)) (+ (cadr direction) (cadr pos)))))
		(t nil)
	)
	)
)

; Returns list of successor states as a result of given move
(defun next-states (s)
  (cleanUpList 
  	(list 
		; up
	  	(try-move s '(0 -1)) 	
	  	; down
		(try-move s '(0 1))
		; left  	
	  	(try-move s '(-1 0))	
		; right
		(try-move s '(1 0)) 	
		
  	)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Returns the trivial admissible heuristic.
(defun h0 (s) 0)


; Counts the number of boxes in a given row not on a star in the given list
(defun rowBoxes-row (row)
	(cond
		((null row) 0)
		((isBox (car row)) (+ 1 (rowBoxes-row (cdr row))))
		(t (rowBoxes-row (cdr row)))
	)
)


; Computes the number of misplaced boxes in s
(defun h1 (s)
	(cond
		((null s) 0)
		((null (car s)) (h1 (cdr s)))
		((list (car s)) (if (isBox (caar S)) (+ 1 (h1 (cons (cdar S) (cdr S)))) (h1 (cons (cdar S) (cdr S)))))
	)
)



;; my heuristic functions!

; Gets coordinates, appending rows and columns
(defun coords (r c)
	(cond 
		((null c) nil)
		(t (append (list (list r (car c))) (coords r (cdr c))))
	)
)

; Searches for columns that contain boxes in given row 
(defun boxCols (r c)
	(cond 
		((null r) nil)
		(t (if (isBox (car r))
	       		(cons c (boxCols (cdr r) (+ c 1))) (boxCols (cdr r) (+ c 1))
	     	   )
	   	)
	)
  )

; Searches for coordinates that contain boxes
(defun boxPos (s row)
	(cond ((null s) nil)
		(t (let ((cols (boxCols (car s) 0)))
	     		(if cols
				(coords row cols) (boxPos (cdr s) (+ row 1)))
			)
	 	   )
		)
)

; Searches for columns that contain goal (star) in given row 
(defun goalCols (r c)
  (cond ((null r) nil)
	(t (if (isStar (car r))
	       (cons c (goalCols (cdr r) (+ c 1)))
				 (goalCols (cdr r) (+ c 1))
	     )
	   )
	)
  )

; Searches for coordinates that contain goals
(defun goalPos (s r)
  (cond ((null s) nil)
	(t (let ((c (goalCols (car s) 0)))
	     (if c (coords r c) (goalPos (cdr s) (+ r 1)))))
	)
)

; Computes the manhattan distance between 2 coordinates
(defun getDistance (p1 p2)
	(cond 
		((null p1) 0)
		((null p2) 0)
		(t (let ((x1 (car p1)) (y1 (cadr p1)) (x2 (car p2)) (y2 (cadr p2)))
			(cond 
				((> x1 x2)
					(if (> y1 y2)
						(+ (- x1 x2) (- y1 y2))
						(+ (- x1 x2) (- y2 y1))))
			(t (if (> y1 y2)
				(+ (- x2 x1) (- y1 y2))
				(+ (- x2 x1) (- y2 y1)))))
			)			
		)
	)
)

; Computes minimum distance from a box to the possible goals, then recursively 
; searches for closest goal
(defun getMinDistance (box goals)
	(cond 
		((null box) nil)
		((null goals) nil)
		(t 
			(let ((first-dist (getDistance box (car goals)))
			(min-rest (getMinDistance box (cdr goals))))
			(if (null min-rest) first-dist
			(if (< first-dist min-rest) first-dist min-rest)))
		)
	)
)

; Sum of manhattan distances for every box to get to every goal 
(defun manhattan (boxes goals)
	(cond 
		((null boxes) 0)
		((null goals) 0)
		((atom boxes) (getMinDistance boxes goals))		
		(t (+ (getMinDistance (car boxes) goals) (manhattan (cdr boxes) goals)))	
	)
)

; Finds the boxes and nearest goals, and compute manhattan distance based on returned
; coordinates and corresponding distances
(defun h804794484 (s)
	(let ((boxes (boxPos s 0)) (goals (goalPos s 0)))
				(manhattan boxes goals)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
