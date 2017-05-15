;; ====================================
;;  CMPU-365, Spring 2017
;;  alpha-beta.lisp
;;  Lilian Zhao and Hao Wu
;; ====================================

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "COMPUTE-MOVE (cutoff: ~A)~%" cutoff-depth)
  (cond
   ;; Case 1:  Game already over, nothing to do
   ((game-over? g)
    (format t "Game is over! ~%")
    nil)
   ;; Case 2:  Game still on, compute best move...
   (t
    ;; Call COMPUTE-MAX with init alpha/beta values
    (let* ((statty (make-stats))
	   (best-move (compute-max g 0 *neg-inf* *pos-inf*
				   statty cutoff-depth)))
      ;; Report number of moves considered...
      (format t "   NUM-MOVES-DONE: ~A, NUM-MOVES-PRUNED: ~A~%"
	      (stats-num-moves-done statty)
	      (- (stats-num-potential-moves statty)
		 (stats-num-moves-done statty)))
      (format t "   BEST MOVE: ~A~%" best-move)
      best-move))))


;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)
  (let ((best-move-so-far nil))
    (cond
     ;; Base Case 1:  Game over
     ((game-over? g)
      ;; we're at a max node, and we just lost!
      ;; we prefer deeper depth!
      (+ *loss-value* curr-depth))
     
     ;; Base Case 2:  We're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Use the static evaluation func
      (eval-func g))
     
     ;; Recursive Case:  Need to do minimax with alpha-beta pruning
     (t
      (let* ((moves (gomoku-legal-moves g))
	     (orig-legal-moves (copy-list moves))) 
	(incf (stats-num-potential-moves statty) (length moves))
	(dolist (mv moves)
	  (incf (stats-num-moves-done statty))
	  (apply #'do-move! g nil mv)
	  (let ((child-val (compute-min g (1+ curr-depth) alpha beta
					statty cutoff-depth)))
	    (undo-move! g)
	    (setf (gomoku-legal-moves g) (copy-list orig-legal-moves))
	    ;; Check for updating ALPHA value...
	    (when (> child-val alpha)
	      (setf alpha child-val)
	      (setf best-move-so-far mv)
	      ;; Check for pruning ...
	      (when (<= beta alpha)
		;; Hey! PRUNE!  Forget about any remaining moves in 
		;;  this DOLIST... We're outta here!!
		(return-from compute-max 
			     ;; Need to return BEST-MOVE if we're at depth 0
			     ;; Otherwise return ALPHA value
			     (cond ((zerop curr-depth)
				    (format t "   ROOT NODE ALPHA: ~A~%" alpha)
				    best-move-so-far)
				   (t
				    alpha)))))))
	;; return alpha or best-move-so-far, depending on whether
	;; we're at depth 0 or not
	(cond
	 ((zerop curr-depth)
	  (format t "   ROOT NODE ALPHA: ~A~%" alpha)
	  best-move-so-far)
	 (t
	  alpha)))))))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)
  (cond
   ;; Game Over!
   ((game-over? g)
    ;; And the good guys just won! (A win for MAX node)
    ;;   -- prefer shallower win depth!
    (- *win-value* curr-depth))
   
   ;; We've hit the cutoff depth
   ((>= curr-depth cutoff-depth)
    ;; Let static eval func do its thing
    (eval-func g))
   
   ;; Otherwise, we need to use recursion!
   (t
    (let* ((moves (gomoku-legal-moves g))
	   (orig-legal-moves (copy-list moves))) 
      (incf (stats-num-potential-moves statty) (length moves))
      (dolist (mv moves)
	(incf (stats-num-moves-done statty))
	(apply #'do-move! g nil mv)
	(let ((child-val (compute-max g (1+ curr-depth) alpha beta
				      statty cutoff-depth)))
	  (undo-move! g)
	  (setf (gomoku-legal-moves g) (copy-list orig-legal-moves))
	  ;; Update BETA value if necessary...
	  (when (< child-val beta)
	    (setf beta child-val)
	    ;; Check for PRUNING!
	    (when (<= beta alpha)
	      ;; Hey, we're pruning... that's it! we're outta here!!
	      (return-from compute-min beta)))))
      ;; return beta 
      ;;  NOTE:  Depth can't be zero for a MIN node
      beta))))



;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;  OUTPUT:  don't care
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun compute-do-and-show-n-moves
    (g n cutoff-depth)
  (let ((mv nil))
    (dotimes (i n)
      (format t "~%~A~%" g)
      (setf mv (compute-move g cutoff-depth))
      (apply #'do-move! g nil mv))
    (format t "~%~A~%" g)))

