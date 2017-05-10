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
  ;; white-pieces
  white-pieces
  ;; black-pieces
  black-pieces
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= 60)
  num-open
  ;; NEW-POSN: the latest move
  new-posn
  )

;; scores for static evaluation function
(defconstant *one* 10)
(defconstant *two* 100)
(defconstant *three* 1000)
(defconstant *four* 100000)
(defconstant *five* 1000000)
(defconstant *block-one* 1)
(defconstant *block-two* 10)
(defconstant *block-three* 100)
(defconstant *block-four* 10000)


(defconstant *border* 100)


;;  array of directions
(defconstant *dirns* (make-array '(8 2)
                                 :initial-contents
				 '((1 1) (-1 -1) (1 -1)
				   (-1 1) (0 1) 
                                   (0 -1) (1 0) (-1 0))))

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
		:num-open (gomoku-num-open game)
    :white-pieces (gomoku-white-pieces game)
    :black-pieces (gomoku-black-pieces game)
    :new-posn (gomoku-new-posn game)))

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
           :new-posn nil
           :black-pieces 0
           :white-pieces 0)))
    game))


(defmethod make-hash-key-from-game
    ((game gomoku))
  (list (gomoku-white-pieces game)
  (gomoku-black-pieces game)
  (gomoku-whose-turn game)))





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
  (let ((plr (gomoku-whose-turn game))
    (board (gomoku-board game)))
    ;; Case 1: check if it's a legal move when check-legal is true
    (when (and check-legal? (not (is-legal? game row col)))
      (return-from do-move! game))
    ;; Case 2: when the move is legal OR when we don't need to check
    ;; 1. add the new piece to the board

    (setf (aref board row col) plr)

    (if (eq plr *black*)
      (incf (gomoku-black-pieces game)
        (ash 1 (row-col->posn row col)))
      (incf (gomoku-white-pieces game)
        (ash 1 (row-col->posn row col))))
    ;; 2. decrease the number of empty slots available on the board
    (decf (gomoku-num-open game))
    ;; 3. toggle player
    (toggle-player! game)
    ;; 4. set the new-posn to this move
    (setf (gomoku-new-posn game) (row-col->posn row col))
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
;;  OUTPUT:  If not over, return NIL. If game over, return the winner of the game.
;;           Return 0 if draw. 
(defun game-over? (game)
  (let ((current (gomoku-new-posn game))
        (plr (other-player (gomoku-whose-turn game)))
        (count 0)
        (sum 0)
        (board (gomoku-board game)))
    (when (not current)
      (return-from game-over? nil))
    ;; counting tokens of current color in 8 directions
    ;; and adding the number of two opposite directions together
    (dotimes (dirn 8)
           (let ((count 0)
                 (cr (posn->row current))
                 (cc (posn->col current))
                 (dr (aref *dirns* dirn 0))
                 (dc (aref *dirns* dirn 1)))
             ;; record number of consecutive token of plr color
             (while (and (not (off-board? cr cc)) (= (aref board cr cc) plr))
                    (incf count)
                    (setf cr (+ cr dr))
                    (setf cc (+ cc dc)))
             ;; if it is a new direction, record sum
             (if (= sum 0) 
                 (setf sum count)
                 ;; otherwise we have already the number of tokens in the opposite 
                 ;; directions. Thus we add them together
                 ;; -1 because we double count the token in the middle
                 (setf sum (+ (- sum 1) count)))
             (if (> sum 4)
                 (return-from game-over? plr)
                 (setf sum 0)))))

  (when (= (gomoku-num-open game) 0)
    (return-from game-over? 0))
  nil)


;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The amount of the
;;    win/loss is reported by the SQUARE-ROOT of the absolute difference
;;    in the number of tokens for the two players.  For example, if the
;;    game ends with WHITE having 25 tokens and BLACK having 31 tokens,
;;    then black wins by 6 tokens, and the output is approximately 2.45.

(defun default-policy
  (game)
  ;; Do random moves until the game is over
  (let ((winner (game-over? game)))
    ;; return the winner of the game
    (while (not winner)
           (do-random-move! game)
           (setf winner (game-over? game)))
    (* (- winner) (sqrt (sqrt (gomoku-num-open game))))))

;;  EVAL-FUNC
;; -------------------------------------------------
;;  INPUT:  GAME, a GOMOKU struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based on the difference in piece values held by 
;;           the two players.

;; count consecutive 3 & 4
;; and whether surrounded by open space or opposite token


;; eval-helper
;; return a 3-D array (PLR, BLOCK, NUM) of counter
;; plr - black(0) or white(1)
;; block - open(0), block on one side(1), block on both side(2)
;; num - num consecutive tokens
;; value is the number of n consecutive tokens
(defun eval-helper
       (game)
  (let ((board (gomoku-board game))
        (count (make-array '(2 3 5) :initial-element 0)))
    ;; horizontally
    (dotimes (i 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0))
        (dotimes (j 16)
          (if (= j 16)
              (setf curr *border*)
              (setf curr (aref board i j)))
          (if (= curr *blank*)
              (setf afterblock 0)
              (setf afterblock 1))
          (if (= curr last)
              (incf counter)
              (cond ((= last *white*) 
                     (incf (aref count 1 (+ beforeblock afterblock) counter))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *black*)
                     (incf (aref count 0 (+ beforeblock afterblock) counter))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *blank*)
                     (setf counter 1)
                     (setf beforeblock 0))))
          (setf last curr))))

    ;; vertically
    (dotimes (i 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0))
        (dotimes (j 16)
          (if (= j 16)
              (setf curr *border*)
              (setf curr (aref board j i)))
          (if (= curr *blank*)
              (setf afterblock 0)
              (setf afterblock 1))
          (if (= curr last)
              (incf counter)
              (cond ((= last *white*) 
                     (incf (aref count 1 (+ beforeblock afterblock) counter))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *black*)
                     (incf (aref count 0 (+ beforeblock afterblock) counter))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *blank*)
                     (setf counter 1)
                     (setf beforeblock 0))))
          (setf last curr))))
    ;; diagonally
    ;; letfdown from first row
    (dotimes (j 14)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0)
            (i 0))
        (while (not (off-board? i j))
               (setf curr (aref board i j))
               (if (= curr *blank*) 
                   (setf afterblock 0)
                   (setf afterblock 1))
               (if (= curr last)
                   (incf counter)
                   (cond ((= last *white*) 
                          (incf (aref count 1 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *black*)
                          (incf (aref count 0 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *blank*)
                          (setf counter 1)
                          (setf beforeblock 0))))
               (setf last curr)
               (decf i)
               (incf j))
        (incf counter)
        (setf afterblock 1)
        (cond ((= last *white*) 
               (incf (aref count 1 (+ beforeblock afterblock) counter)))
              ((= last *black*)
               (incf (aref count 0 (+ beforeblock afterblock) counter))))))

    ;; letfdown from last column
    (dotimes (i 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0)
            (j 14))
        (while (not (off-board? i j))
               (setf curr (aref board i j))
               (if (= curr *blank*) 
                   (setf afterblock 0)
                   (setf afterblock 1))
               (if (= curr last)
                   (incf counter)
                   (cond ((= last *white*) 
                          (incf (aref count 1 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *black*)
                          (incf (aref count 0 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *blank*)
                          (setf counter 1)
                          (setf beforeblock 0))))
               (setf last curr)
               (decf i)
               (incf j))
        (incf counter)
        (setf afterblock 1)
        (cond ((= last *white*) 
               (incf (aref count 1 (+ beforeblock afterblock) counter)))
              ((= last *black*)
               (incf (aref count 0 (+ beforeblock afterblock) counter))))))

    
    ;; rightup from last column
    (dotimes (i 14)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0)
            (j 15))
        (while (not (off-board? i j))
               (setf curr (aref board i j))
               (if (= curr *blank*) 
                   (setf afterblock 0)
                   (setf afterblock 1))
               (if (= curr last)
                   (incf counter)
                   (cond ((= last *white*) 
                          (incf (aref count 1 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *black*)
                          (incf (aref count 0 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *blank*)
                          (setf counter 1)
                          (setf beforeblock 0))))
               (setf last curr)
               (decf i)
               (decf j))
        (incf counter)
        (setf afterblock 1)
        (cond ((= last *white*) 
               (incf (aref count 1 (+ beforeblock afterblock) counter)))
              ((= last *black*)
               (incf (aref count 0 (+ beforeblock afterblock) counter))))))

    
    ;; rightup from last row
    (dotimes (j 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 0)
            (i 14))
        (while (not (off-board? i j))
               (setf curr (aref board i j))
               (if (= curr *blank*) 
                   (setf afterblock 0)
                   (setf afterblock 1))
               (if (= curr last)
                   (incf counter)
                   (cond ((= last *white*) 
                          (incf (aref count 1 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *black*)
                          (incf (aref count 0 (+ beforeblock afterblock) counter))
                          (setf counter 1)
                          (setf beforeblock 1))
                         ((= last *blank*)
                          (setf counter 1)
                          (setf beforeblock 0))))
               (setf last curr)
               (decf i)
               (decf j))
        (incf counter)
        (setf afterblock 1)
        (cond ((= last *white*) 
               (incf (aref count 1 (+ beforeblock afterblock) counter)))
              ((= last *black*)
               (incf (aref count 0 (+ beforeblock afterblock) counter))))))
    
    (make-array 2 :initial-contents 
                black-count 
                white-count)))



      
            
            




