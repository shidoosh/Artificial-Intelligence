;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS takes a single argument FRINGE that represents a list of search trees
; Returns a top-level list of leaf nodes, in the order they are visited by left-to-right breadth-first search. 
; The initial call to BFS passes a FRINGE list with a single element: the root of the search tree.
(defun BFS (FRINGE)
        (cond
                ((null FRINGE) nil)
		; BFS on rest of the list if first element is an atom
                ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
		; else, first element is a list, BFS with rest of the list with first element appended
                (t (BFS (append (cdr FRINGE) (car FRINGE))))
        )
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

(defun FINAL-STATE (S)
        (cond
                ((equal S '(T T T T)) t)
                (t nil)
        )
)




; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

; h b d p 
; car = homer, cadr = baby, caddr = dog, cadddr = poison 

(defun NEXT-STATE (S A)
	(cond 
	  ; Homer only 
		((equal A 'h)
                ; if results in baby alone with dog or poison, nil
		(cond ((and (equal (car S) (cadr S)) (or (equal (car S) (caddr S)) (equal (cadr S) (cadddr S)))) nil)
                ; else, valid state
                  (t (list (cons (NOT (car S)) (cdr S))))))
          
          ; Homer with baby
          	((equal A 'b) 
           	; switch Homer and baby's respective states 
      		(cond ((equal (car S) (cadr S)) (list (list (not (car S)) (not (cadr S)) (caddr S) (cadddr S))))
                  ; else, Homer is not with baby, invalid state 
		      (t nil)))
          
          ; Homer with dog
          ((equal A 'd)
                  ; Homer not with dog, invalid state
           (cond ((not (equal (car S) (caddr S))) nil)
                  ; Baby alone with poison, invalid state 
                  ((equal (cadr S) (cadddr S)) nil)
                  ; else, valid state 
                  (t (list (list (NOT (car S)) (cadr S) (NOT (caddr S)) (cadddr S))))))
          
          ; Homer with poison
          ((equal A 'p) ;
                  ; Homer not with poison, invalid state
           (cond ((not (equal (car S) (cadddr S))) nil)
                  ; dog and baby left unsupervised, invalid state 
                  ((equal (cadr S) (caddr S)) nil)
                  ; else, valid state 
                  (t (list (list (NOT (car S)) (cadr S) (caddr S) (NOT (cadddr S)))))))
           
    )
)










; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    ; append next possible legal successor states for the current state 
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)





; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond
		; nil if s is not member of states
   		((null STATES) nil)
		; s is member of states 
   		((equal S (car STATES)) t)
		; continue search for s
   		(t (ON-PATH S (cdr STATES)))
	)
)






; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    	; STATES holds all valid successor states for last state in PATH 
	(cond 
		; else, nil 
		((null STATES) nil)

		; path exists from initial to goal state 
		((DFS (car STATES) PATH) (DFS (car STATES) PATH))

		; continue for all possible solutions
		((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
	)
)





; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond  
	; check if in goal state
    	((FINAL-STATE S) (append PATH (list S)))

    	; check for cycle 
    	((ON-PATH S PATH) nil)
	
	; continue search
    	(t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)   
