
;1. PAD computes the Nth number in the Padovan sequence 
(defun PAD(N)
	(cond
	; set initial values PAD(0), PAD(1), PAD(2) = 0 
		((< N 3) 1)  
	; recursively compute PAD(N) = PAD(N-2) + (PAD(N-3)
	; which is equivalent to PAD(N+1) = (PAD(N-1) + PAD(N-2) 
	      (t (+ (PAD (- N 2)) (PAD (- N 3))))))

;2. SUMS computes the number of additions required by PAD function to compute
;the Nth Padovan number 
(defun SUMS(N)
	(cond 
	;no additions required for N < 3, as P(0), P(1), P(2) = 0. 
		((< N 3) 0)
	;count each addition that PAD would execute, 
	;plus 1 to account for 0th index 
		(t (+ (SUMS (- N 2)) (SUMS (- N 3)) 1))))
;3. ANON returns an anonymized version of TREE such that all atoms are 
;represented by ? 
(defun ANON(TREE)
	(cond 
		; empty tree returns empty list
		((null TREE) nil)
		; atoms in TREE replaced with ? 
		((atom TREE) '?)
		; repeat for next item in TREE, constructing new list 
		(t (cons (ANON (car TREE)) (ANON (cdr TREE))))))
