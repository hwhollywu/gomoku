;; ========================================
;;  CMPU-365, Spring 2017
;;  Monte Carlo Tree Search 
;;  Lilian Zhao and Hao Wu
;; ========================================


;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key             
  whose-turn      
  (num-visits 0)  
  veck-moves      
  veck-visits     
  veck-scores    
  )

;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))      
  root-key)

;;  GET-ROOT-NODE
;; ------------------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

;; -------------------------------------------------
;;  Easiest to define the necessary functions
;;  in the following order
;; -------------------------------------------------

;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree
    (game)
  (make-mc-tree :root-key (make-hash-key-from-game game)))


;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node
    (game tree key)
  (let* ((moves (gomoku-legal-moves game))
	 (num-moves (length moves))
	 (nodey (make-mc-node 
		 :key key
		 :veck-moves  (make-array num-moves :initial-contents moves)
		 :veck-visits (make-array num-moves :initial-element 0)
		 :veck-scores (make-array num-moves :initial-element 0)
		 :whose-turn (whose-turn game))))
    ;; insert nodey into tree
    (setf (gethash key (mc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move
    (nodey c)
  (let* ((player (mc-node-whose-turn nodey))
	 (moves (mc-node-veck-moves nodey))
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
      ;;   Q(s,a)  +/-  c*sqrt(log(N(s))/N(s,a))
      ;; Note:  Can be called with c=0 or c>0.
      ;;        But if c=0, we can assume n>0 (i.e., *some*
      ;;          node has already been visited)
      (let ((n (mc-node-num-visits nodey))
	    (move-visits (mc-node-veck-visits nodey))
	    (move-scores (mc-node-veck-scores nodey))
	    (best-move-so-far nil)
	    (best-score-so-far (if (eq player *black*)
				   *neg-inf* 
				 *pos-inf*)))
	(dotimes (i num-moves)
	  ;; When c>0 and this move has not yet been visited
	  ;; Then we want to select it immediately!
	  (when (and (> c 0)
		     (= (svref move-visits i) 0))
	    (return-from select-move i))
	  ;; When c=0 and this move has not yet been visited
	  ;; Ignore this move!  (I.e., only proceed if it *has* 
	  ;; been visited at least once.)
	  (when (> (svref move-visits i) 0)
	    ;; Fetch average score for this move
	    (let ((score (svref move-scores i)))
	      ;; When C > 0, update score using UGLY term
	      (when (> c 0)
		(let ((ugly-term (* c (sqrt (/ (log n) 
					       (svref move-visits i))))))
		  (if (eq player *black*) 
		      (incf score ugly-term)
		    (decf score ugly-term))))
	      ;; When SCORE is better than best-score-so-far...
	      (when (or (and (eq player *black*)
			     (> score best-score-so-far))
			(and (eq player *white*)
			     (< score best-score-so-far)))
		;; Update best-score/move-so-far
		(setf best-score-so-far score)
		(setf best-move-so-far i)))))
	;; Return best-move-so-far or (if NIL) a random move
	(if best-move-so-far
	    best-move-so-far
	  (random num-moves)))))))


;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

(defun sim-tree
    (game tree c)
  (let (;; KEY-MOVE-ACC:  accumulator of KEYs and MOVEs
	(key-move-acc nil)
	(hashy (mc-tree-hashy tree)))
    (while (not (game-over? game))
      (let* (;; KEY:  Hash key for current state of game
	     (key (make-hash-key-from-game game))
	     ;; NODEY:  The MC-NODE corresponding to KEY (or NIL if not in tree)
	     (nodey (gethash key hashy)))
	;; Case 1:  When key not yet in tree...
	(when (null nodey)
	  ;; Create new node and insert it into tree
	  (setf nodey (insert-new-node game tree key))
	  (let* ((mv-index (select-move nodey c))
		 (move-veck (mc-node-veck-moves nodey))
		 (move (svref move-veck mv-index)))
	    (apply #'do-move! game nil move)
	    (push key key-move-acc)
	    (push mv-index key-move-acc)
	    ;; return the accumulator prepended with selected MOVE
	    ;; and KEY for current state
	    (return-from sim-tree (reverse key-move-acc))))
	
	;; Case 2:  Key already in tree!
	(let* ((mv-index (select-move nodey c))
	       (move-veck (mc-node-veck-moves nodey))
	       (move (svref move-veck mv-index)))
	  (apply #'do-move! game nil move)
	  (push key key-move-acc)
	  (push mv-index key-move-acc))))
    
    ;; After the WHILE... return the accumulated key/move list
    (reverse key-move-acc)))

;;  SIM-DEFAULT
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))


;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup
    (hashy key-move-acc result)
  (while key-move-acc
    (let* ((key (pop key-move-acc))
	   (nodey (gethash key hashy))
	   (mv-index (pop key-move-acc))
	   (visitz (mc-node-veck-visits nodey))
	   (scorez (mc-node-veck-scores nodey)))
      ;; incremenet node num visits
      (incf (mc-node-num-visits nodey))
      ;; increment num times did this move from this state
      (incf (svref visitz mv-index))
      ;; increment the SCORE
      (incf (svref scorez mv-index)
	    (/ (- result (svref scorez mv-index))   
	       (svref visitz mv-index))))))

;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

(defun uct-search
    (orig-game num-sims c)
  ;; Want to use COPY of GAME struct for simulations...
  ;; That way, can reset game struct before each simulation...

    	;;(format t "SHENMEGUI")
  (let* ((tree (new-mc-tree orig-game))
	 (hashy (mc-tree-hashy tree))
	 ;;(player (whose-turn orig-game))
	 )
    (dotimes (i num-sims)
      ;;	(format t "~A" i)
      (let* (;; Work with a COPY of the original game struct
	     (game (copy-game orig-game))
	     ;; Phase 1:  SIM-TREE Destructively modifies game
	     (key-move-acc (sim-tree game tree c))
	     ;; Phase 2:  SIM-DEFAULT returns result 
	     (playout-result (sim-default game))) 
	;; Finally, backup the results
	(backup hashy key-move-acc playout-result)))				   
    ;; Select the best move (using c = 0 because we are not exploring anymore)
     (let* ((rootie (get-root-node tree))
	   (mv-index (select-move rootie 0))
	   (move (svref (mc-node-veck-moves rootie) mv-index)))
      move)))



