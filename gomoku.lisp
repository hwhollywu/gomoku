;; ========================================
;;  CMPU-365, Spring 2017
;;  FILE:  gomoku.lisp
;;  Lilian Zhao and Hao Wu
;; ========================================


;;  The GOMOKU struct
;; --------------------------------------------------------

(defstruct (gomoku (:print-function print-gomoku))
  ;; BOARD:  a 15-by-15 array of *white*, *black* or *blank* 
  (board (make-array '(15 15) :initial-element *blank*))      
  ;; WHOSE-TURN:  either *BLACK* or *WHITE*
  whose-turn
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= 60)
  num-open
  ;; NEW-POSN: the latest move
  new-posn
  )


;;  WHOSE-TURN
;; -----------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The player whose turn it is (*black* or *white*)

(defmethod whose-turn
    ((game gomoku))
  (gomoku-whose-turn game))


;;  COPY-ARRAY
;; -------------------------------------------------
;;  INPUT:   HARRY, a 2-dimensional array
;;  OUTPUT:  A copy of HARRY

(defun copy-array
    (harry)
  (let* ((dims (array-dimensions harry))
	 (kopy (make-array dims)))
    (dotimes (r (first dims))
      (dotimes (c (second dims))
	(setf (aref kopy r c) (aref harry r c))))
    kopy))

;;  COPY-GAME
;; ------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
    ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
		:whose-turn (gomoku-whose-turn game)
		:num-open (gomoku-num-open game)))

;;  PRINT-GOMOKU
;; --------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           STR, output stream (or T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the GOMOKU game

(defun print-gomoku
    (g str d)
  (declare (ignore d))
  (let ((bored (gomoku-board g)))
    (format str "~%  | 0  1  2  3  4  5  6  7  8  9  10 11 12 13 14~%")
    (format str "------------------------------------------------~%")
    (dotimes (r 15)
      (if (< r 10)
        (format str "~A | " r)
        (format str "~A| " r))
      (dotimes (c 15)
	(let ((token (aref bored r c)))
	  (format str "~A  " (cond ((eq token *black*) *black-show*)
				  ((eq token *white*) *white-show*)
				  (t *blank-show*)))))
      (format str "~%"))
    (format str "~%")
    (format str "                   It is ~A's turn!~%" 
	    (if-black-turn g *black-show* *white-show*))
    ))


;;  NEW-GOMOKU
;; --------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  An GOMOKU struct representing a new game

(defun new-gomoku
    ()
  (let* ((game (make-gomoku :whose-turn *black*
			     :num-open 225
           :new-posn nil)))
    game))


;;  Defines the following METHODS 
;; ---------------------------------------------------------
;;     IS-LEGAL?        ok
;;     LEGAL-MOVES      ok
;;     DO-MOVE!         ok
;;     DO-RANDOM-MOVE!  ok  ??
;;     DEFAULT-POLICY   ??
;;     GAME-OVER?  
;;     EVAL-FUNC


;;  IS-LEGAL?
;; -----------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           ROW, COL, integers
;;  OUTPUT:  T if this is a legal move for PLAYER

(defun is-legal? (game row col)
  (let ((board (gomoku-board game)))
  (cond
     ;; Case 1: Check if it's on board 
     ((off-board? row col)
      NIL)
     ;; Case 2: check if the spot is taken already
     ((or (equal (aref board row col) *black*)
    (equal (aref board row col) *white*)) 
      NIL)
     (t t))))


;;  LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns a vector containing the *pass* move.

(defun legal-moves (game)
  ;; create an initial empty list of moves 
  (let ((moves NIL)) 
    ;; Go through board
    (dotimes (i 15)
      (dotimes (j 15)
  ;; If the move is legal 
  (if (is-legal? game i j)
      ;; add the position array to moves 
      (push (list i j)
      moves))))
    ;; if there are moves 
    (if moves 
  ;; return vector form of moves
  (make-array (length moves) :initial-contents moves)
      ;; else, return a vector containing the pass move 
      (vector *pass*))))


;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           CHECK-LEGAL?, T or NIL
;;           ROW, COL, two integers (between 0 and 14)
;;  OUTPUT:  The modified GAME
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if it
;;           passes the IS-LEGAL? check

(defun do-move! (game check-legal? row col)
  (let ((color (gomoku-whose-turn game))
    (board (gomoku-board game)))
    ;; Case 1: check if it's a legal move when check-legal is true
    (cond 
      (check-legal?
      (when (not (is-legal? game row col))
    (return-from do-move! game))))
    ;; Case 2: when the move is legal OR when we don't need to check
    ;; 1. add the new piece to the board
    (setf (aref board row col) color)
    ;; 2. decrease the number of empty slots available on the board
    (decf (gomoku-num-open game))
    ;; 3. toggle player
    (toggle-player! game)
    ;; 4. set the new-posn to this move
    (setf (gomoku-new-posn game) (list row col))
    ;; return the game
    game))


;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defun random-move (game)
  (let ((moves (legal-moves game)))
    ;; return at move at index 0 - (the length of moves available)
    (svref moves (random (length moves)))))


;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the 
;;   legal moves available to the current player, chosen randomly.

(defun do-random-move! (game)
  (let ((chosenmove (random-move game)))
    (do-move! game nil (first chosenmove) (second chosenmove))))


;;  GAME-OVER?
;; -----------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  T if the game is over (i.e., either no open slots in the
;;    game board, or one player has five-in-a-row).



;;  EVAL-FUNC
;; -------------------------------------------------
;;  INPUT:  GAME, a GOMOKU struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based on the difference in piece values held by 
;;           the two players.



