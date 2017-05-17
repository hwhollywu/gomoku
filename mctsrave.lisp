;; ========================================
;;  CMPU-365, Spring 2017
;;  Monte Carlo Tree Search - RAVE
;;  Lilian Zhao and Hao Wu
;; ========================================

(defconstant *bias* 10)
(defconstant *min* 0.0000001)

;;  RAVEMC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct ravemc-node
  key             
  whose-turn      
  (num-visits 0)  
  veck-moves      
  veck-visits     
  veck-scores    
  veck-nhat
  veck-qhat
  )

;;  RAVEMC-TREE struct -- the RAVEMCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct ravemc-tree
  (hashy (make-hash-table :test #'equal))      
  root-key)

;;  GET-ROOT-NODE-RAVE
;; ------------------------------------------------------
;;  INPUT:   TREE, a RAVEMCTS struct
;;  OUTPUT:  The RAVEMC-NODE corresponding to the root of the TREE

(defun get-root-node-rave
    (tree)
  (gethash (ravemc-tree-root-key tree) (ravemc-tree-hashy tree)))

;; -------------------------------------------------
;;  Easiest to define the necessary functions
;;  in the following order
;; -------------------------------------------------

;;  NEW-RAVEMC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-ravemc-tree
    (game)
  (make-ravemc-tree :root-key (make-hash-key-from-game game)))


;;  INSERT-NEW-NODE-RAVE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node-rave
    (game tree key)
  (let* ((moves (gomoku-legal-moves game))
	 (num-moves (length moves))
	 (heuristic 0)
	 (hc 1)
	 (nodey (make-ravemc-node 
		 :key key
		 :veck-moves  (make-array num-moves :initial-contents moves)
		 :veck-visits (make-array num-moves :initial-element 0)
		 :veck-scores (make-array num-moves :initial-element 0)
		 :veck-qhat (make-array num-moves :initial-element 0)
		 :veck-nhat (make-array num-moves :initial-element 0)
		 :whose-turn (whose-turn game))))

  	(dotimes (i num-moves)
  		(apply #'do-move! game nil (nth i moves))
  		(setf heuristic (/ (eval-func game) 100000))
  		(undo-move! game)
  		(setf (svref (ravemc-node-veck-visits nodey) i) hc)
  		(setf (svref (ravemc-node-veck-scores nodey) i) heuristic)
  		(setf (svref (ravemc-node-veck-qhat nodey) i) heuristic)
  		(setf (svref (ravemc-node-veck-nhat nodey) i) hc))

    ;; insert nodey into tree
    (setf (gethash key (ravemc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))

;;  SELECT-MOVE-RAVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an RAVEMC-NODE struct
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move-rave
    (nodey)
  (let* ((player (ravemc-node-whose-turn nodey))
	 (moves (ravemc-node-veck-moves nodey))
	 (num-moves (length moves)))
    (cond
     ;; No legal moves!
     ((= num-moves 0)
      ;; signal failure
      nil)
     ;; Only one legal move
     ((= num-moves 1)
      ;; return it
      0)
     ;; Two or more moves
     (t
      ;; Need to find argmax/argmin of 
      ;;   evaluation based on RAVE algorithm

      (let (
	    (move-visits (ravemc-node-veck-visits nodey))
	    (move-scores (ravemc-node-veck-scores nodey))
	    (move-qhats (ravemc-node-veck-qhat nodey))
	    (move-nhats (ravemc-node-veck-nhat nodey))
	    (best-move-so-far nil)
	    (best-score-so-far (if (eq player *black*)
				   *neg-inf* 
				 *pos-inf*))
	    (temp 0))
	(dotimes (i num-moves)
	  (setf temp (+ (svref move-visits i) (svref move-nhats i) 
							(* 4 (svref move-visits i) 
								(svref move-nhats i) *bias* *bias*)))

	  ;; When temp = 0, and player is black
	  ;; Then we want to select it immediately!
	  (when (and (= *black* (ravemc-node-whose-turn nodey)) (= temp 0))
	    (return-from select-move-rave i))
	  ;; When temp = 0, and player is white, we want to skip this round
	  (when (not (= temp 0))
	    ;; Fetch score for this move
	    (let* ((beta (/ (svref move-nhats i) temp))
	    		(score (+ (* (- 1 beta) (svref move-scores i)) 
	    			(* beta (svref move-qhats i)))))

	      ;; When SCORE is better than best-score-so-far...
	      (when (or (and (eq player *black*)
			     (> score best-score-so-far))
			(and (eq player *white*)
			     (< score best-score-so-far)))
		;; Update best-score/move-so-far
		(setf best-score-so-far score)
		(setf best-move-so-far i)))))
	;; Return best-move-so-far or (if NIL) a random move
	best-move-so-far)))))


;;  SIM-TREE-RAVE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

(defun sim-tree-rave
    (game tree)
  (let (;; KEY-MOVE-ACC:  accumulator of KEYs and MOVEs
	(key-move-acc nil)
	(hashy (ravemc-tree-hashy tree)))
    (while (not (game-over? game))
      (let* (;; KEY:  Hash key for current state of game
	     (key (make-hash-key-from-game game))
	     ;; NODEY:  The RAVEMC-NODE corresponding to KEY (or NIL if not in tree)
	     (nodey (gethash key hashy)))
	;; Case 1:  When key not yet in tree...
	(when (null nodey)
	  ;; Create new node and insert it into tree
	  (setf nodey (insert-new-node-rave game tree key))
	  (let* (
		 (move-veck (ravemc-node-veck-moves nodey))
		 (mv-index (random (length move-veck)))
		 (move (svref move-veck mv-index)))
	    (apply #'do-move! game nil move)
	    (push key key-move-acc)
	    (push move key-move-acc)
	    ;; return the accumulator prepended with random MOVE
	    ;; and KEY for current state
	    (return-from sim-tree-rave (reverse key-move-acc))))

	;; Case 2:  Key already in tree!
	(let* ((mv-index (select-move-rave nodey))
	       (move-veck (ravemc-node-veck-moves nodey))
	       (move (svref move-veck mv-index)))
	  (apply #'do-move! game nil move)
	  (push key key-move-acc)
	  (push move key-move-acc))))
    
    ;; After the WHILE... return the accumulated key/move list
    (reverse key-move-acc)))

;;  SIM-DEFAULT-RAVE
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default-rave
    (game) 
  ;; Do random moves until the game is over
  (let ((winner (game-over? game))
	(move-acc nil)
	(val 0)
	(mov nil))
    ;; return the winner of the game
    (while (not winner)
      (setf mov (do-random-move! game))
      (push mov move-acc)
      (setf winner (game-over? game)))
    (setf val (eval-func game))
    (list (reverse move-acc) (/ val 1000000))))



;;  BACKUP-RAVE
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup-rave
    (hashy key-move-acc default-move-acc result)
    (let* (
	   (states nil)
	   (moves nil)
	   (len (length key-move-acc)))

      (dotimes (k len)
      	(if (evenp k)
          	(push (pop key-move-acc) states)
			(push (pop key-move-acc) moves)))

      (setf states (reverse states))
      (setf moves (reverse moves))
      (setf moves (append moves default-move-acc))

      (while states
      	(let* ((key (pop states))
	   			(nodey (gethash key hashy))
	   			(move (pop moves))
	   			(node-move (ravemc-node-veck-moves nodey))
      			(mv-index (position move node-move :test #'equal))
      			(nodey (gethash key hashy))
	   			(visitz (ravemc-node-veck-visits nodey))
	   			(scorez (ravemc-node-veck-scores nodey))
	   			(nhatz (ravemc-node-veck-nhat nodey))
	   			(qhatz (ravemc-node-veck-qhat nodey))
	   			(temp-movlist nil)
	   			(pos nil)
	   			(currmov nil))

	      ;; increment num times did this move from this state
	      (incf (svref visitz mv-index))

	      ;; increment the SCORE
	      (incf (svref scorez mv-index)
		    (/ (- result (svref scorez mv-index))   
		       (svref visitz mv-index)))

	      (when (= 0 (svref nhatz mv-index))
	      	(format t "ZERRRRO")
	      	(setf (svref nhatz mv-index) *min*))

	      (dotimes (i (length moves))
	      	(setf currmov (nth i moves))
	      	(when (not (member currmov temp-movlist :test #'equal))
	      		(setf pos (position currmov node-move :test #'equal))
      			(when pos
      				(incf (svref nhatz pos))
      				(incf (svref qhatz pos)
	    				(/ (- result (svref qhatz mv-index))   
	       				   (svref nhatz mv-index)))))
	      	(push currmov temp-movlist)
	      	(incf i))))))



;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

(defun mc-rave
    (orig-game num-sims)
  ;; Want to use COPY of GAME struct for simulations...
  ;; That way, can reset game struct before each simulation...

  (let* ((tree (new-ravemc-tree orig-game))
	 (hashy (ravemc-tree-hashy tree)))
    (dotimes (i num-sims)
      ;;	(format t "~A" i)
      (let* (;; Work with a COPY of the original game struct
	     (game (copy-game orig-game))
	     ;; Phase 1:  SIM-TREE-RAVE Destructively modifies game
	     (key-move-acc (sim-tree-rave game tree))
	     ;; Phase 2:  SIM-DEFAULT-RAVE returns result 
	     (default-result (sim-default-rave game)))
	;; Finally, backup the results
	(backup-rave hashy key-move-acc (first default-result) (second default-result))))
    ;; Select the best move (using c = 0 because we are not exploring anymore)
     (let* ((rootie (get-root-node-rave tree))
	   (mv-index (select-move-rave rootie))
	   (move (svref (ravemc-node-veck-moves rootie) mv-index)))
      move)))



