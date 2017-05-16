;; ========================================
;;  CMPU-365, Spring 2017
;;  gomoku.lisp
;;  Lilian Zhao and Hao Wu
;; ========================================


;; scores for static evaluation function
(defconstant *one* 5)
(defconstant *two* 100)
(defconstant *three* 1000)
(defconstant *four* 100000)
(defconstant *five* 1000000)
(defconstant *block-one* 1)
(defconstant *block-two* 10)
(defconstant *block-three* 100)
(defconstant *block-four* 10000)

;;  array of directions
(defconstant *dirns* (make-array '(8 2)
                                 :initial-contents
				 '((1 1) (-1 -1) (1 -1)
				   (-1 1) (0 1) (0 -1) (1 0) (-1 0))))

(defconstant *neighbors* (make-array '(24 2) :initial-contents
				     '((-2 -2) (-2 -1) (-2 0) (-2 1) (-2 2)
				       (-1 -2) (-1 -1) (-1 0) (-1 1) (-1 2)
				       (0 -2) (0 -1) (0 1) (0 2)
				       (1 -2) (1 -1) (1 0) (1 1) (1 2)
				       (2 -2) (2 -1) (2 0) (2 1) (2 2))))


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
  ;; MOVE-HISTORY: a list of the moves that got us to the current state
  move-history
  ;; LEGAL-MOVES: a vector of all legal moves around the new-posn (+- 4 tokens)
  legal-moves
  )

;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))


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
;;  INPUT:   GAME, an gomoku struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
  ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
	       :whose-turn (gomoku-whose-turn game)
	       :num-open (gomoku-num-open game)
	       :white-pieces (gomoku-white-pieces game)
	       :black-pieces (gomoku-black-pieces game)
	       :move-history (gomoku-move-history game)
	       :legal-moves (gomoku-legal-moves game)))

;;  PRINT-gomoku
;; --------------------------------------------------
;;  INPUTS:  G, an gomoku struct
;;           STR, output stream (or T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the gomoku game

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


;;  NEW-gomoku
;; --------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  An gomoku struct representing a new game
;;  The game starts at white player's turn, the black always put
;;  the token in the middle of board

(defun new-gomoku
    ()
  (let ((game (make-gomoku :whose-turn *black* 
			   :num-open (* 15 15) 
			   :black-pieces 0
			   :white-pieces 0
			   :legal-moves nil
			   :move-history nil)))
    (do-move! game nil 7 7)
    game))


;;  MAKE-HASH-KEY-FROM-GAME
;; --------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT:  A list of the form (WHITE-PIECES BLACK-PIECES WHOSE-TURN)
;;    where the contents are as described in the STATE struct

(defmethod make-hash-key-from-game
  ((game gomoku))
  (list (gomoku-white-pieces game)
	(gomoku-black-pieces game)
	(gomoku-whose-turn game)))



;;  WHOSE-TURN
;; -----------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT: The player whose turn it is (*black* or *white*)

(defmethod whose-turn
  ((game gomoku))
  (gomoku-whose-turn game))


;;  IS-LEGAL?
;; -----------------------------------
;;  INPUTS:  GAME, a gomoku struct
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
     (t 
      t))))


;;; IS-LEGAL-33
;; -----------------------------------
;;  INPUTS:  GAME, a gomoku struct
;;           ROW, COL, integers
;;  OUTPUT:  T if this is a legal move for PLAYER
;;  Note: this is-legal function includes the three and three rule

(defun is-legal-33
    (game row col)
  (let ((plr (gomoku-whose-turn game))
        (sum 0)
        (num-of-three 0)
        (open t)
        (open-now t)
        (board (gomoku-board game)))
    (when (or (off-board? row col)
              (not (equal (aref board row col) *blank*)))
      (return-from is-legal-33 nil))
    (when (= plr *white*)
      (return-from is-legal-33 t))
    
    ;; counting tokens of current color in 8 directions
    ;; and adding the number of two opposite directions together
    (dotimes (dirn 8)
      (let* ((count 0)
	     (dr (aref *dirns* dirn 0))
	     (dc (aref *dirns* dirn 1))
	     (cr (+ dr row))
	     (cc (+ dc col)))
	;; record number of consecutive token of plr color
	
	(while (and (not (off-board? cr cc)) (= (aref board cr cc) plr))
	  (incf count)
	  (setf cr (+ cr dr))
	  (setf cc (+ cc dc)))
	;; the current cirection is open if next token is BLANK
	(if (and (not (off-board? cr cc)) (= (aref board cr cc) *blank*))
	    (setf open-now t)
	  (setf open-now nil))
	;; if it is a new direction, record sum, and set open to be open-now
	(cond
	 ((evenp dirn) 
	  (setf open open-now)
	  (setf sum count))
	 ;; otherwise we have already the number of tokens in the opposite 
	 ;; directions. Thus we add them together
	 ;; -1 because we double count the token in the middle
	 ;; this chain is open if and only if this and the 
	 ;; opposite direction are open
	 (t
	  (setf open (and open open-now))
	  (setf sum (+ sum 1 count))))
	;; if it is an open 3, increment the counter
	(when (and (= sum 3) open)
	  (incf num-of-three))
	;; if we have more than one open three, not legal
	(when (> num-of-three 1)
	  (return-from is-legal-33 nil))
	)))
  t)




;;  GEN-LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT:  A list of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns a vector containing the *pass* move.

(defun gen-legal-moves (game)
  ;; create an initial empty list of moves 
  (let* ((moves NIL)
	 (new-posn (first (gomoku-move-history game)))
	 (i (posn->row new-posn))
	 (j (posn->col new-posn)))
    ;; go through the +- 4 tokens around the new-posn
    (dotimes (token 24)
      (let* ((r-c (aref *neighbors* token 0))
	     (c-c (aref *neighbors* token 1))
	     (row (+ i r-c))
	     (col (+ j c-c)))
	;; If the move is legal 
	(when (is-legal? game row col)
	  ;; add the position array to moves 
	  (push (list row col) moves))
	))
    ;; return vector form of moves
    moves))


;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, a gomoku struct
;;           CHECK-LEGAL?, T or NIL
;;           ROW, COL, two integers (between 0 and 14)
;;  OUTPUT:  The modified GAME
;;  SIDE EFFECT:  Destructively modifies GAME by doing the specified move.
;;    Note:  If CHECK-LEGAL? is T, then it only does the move if it
;;           passes the IS-LEGAL? check

(defun do-move! (game check-legal? row col)
  (let ((plr (gomoku-whose-turn game))
	(board (gomoku-board game))
	(current-movs (gomoku-legal-moves game)))
    ;; Case 1: check if it's a legal move when check-legal is true
    (when (and check-legal? (not (is-legal? game row col)))
      (return-from do-move! game))
    ;; Case 2: when the move is legal OR when we don't need to check
    ;; 1. add the new piece to the board
    (place-token game board plr row col)
    ;; 2. decrease the number of empty slots available on the board
    (decf (gomoku-num-open game))
    ;; 3. toggle player
    (toggle-player! game)
    ;; 4. add the newest move to move-history
    (push (row-col->posn row col) (gomoku-move-history game))
    ;; 5. set the legal moves for the game
    (setf current-movs (remove (list row col) current-movs :test #'equal))
    ;; remove duplicated locations from the previous legal move
    (setf (gomoku-legal-moves game)
	  (remove-duplicates
	   (append (gen-legal-moves game) current-movs) :test #'equal))
    ;; return the game
    game))


;; UNDO-MOVE
;; -----------------------------
;;  INPUTS:  GAME, a gomoku struct
;;  OUTPUT:  The modified GAME
;;  SIDE EFFECT:  Destructively modifies GAME by undoing the specified move.

(defun undo-move! (game)
  (cond
   ;; Case 1:  No moves on move history!
   ((null (gomoku-move-history game))
    (format t "Can't undo move...empty history!~%")
    game)
   
   ;; Case 2:  There is a move to undo...
   (t
    (let ((new-posn (pop (gomoku-move-history game)))
	  (bored (gomoku-board game)))
      ;; 1. remove piece from board
      (place-token-at-posn bored *blank* new-posn)
      ;; 2. increase the number of empty slots available on the board
      (incf (gomoku-num-open game))
      ;; 3. toggle player
      (toggle-player! game)
      ;; 4. pop the latest move from move-history ;; DONE
      game))))


;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defun random-move (game)
  (let ((moves (gomoku-legal-moves game)))
    ;; return at move at index 0 - (the length of moves available)
    (nth (random (length moves)) moves)))



;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, a gomoku struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the 
;;   legal moves available to the current player, chosen randomly.

(defun do-random-move! (game)
  (let ((chosenmove (random-move game)))
    (do-move! game nil (first chosenmove) (second chosenmove))
    chosenmove))


;;  GAME-OVER?
;; -----------------------------
;;  INPUT:  GAME, an gomoku struct
;;  OUTPUT:  If not over, return NIL. If game over, return the winner of the game.
;;           Return 0 if draw. 

(defun game-over? (game)
  (let ((current (first (gomoku-move-history game)))
        (plr (other-player (gomoku-whose-turn game)))
        (sum 0)
        (board (gomoku-board game)))
    (when (not current)
      (format t "BUGGGGG")
      (return-from game-over? nil))
    ;; counting tokens of current color in 8 directions
    ;; and adding the number of two opposite directions together
    (dotimes (dirn 8)
      (let* ((count 0)
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
	(if (evenp dirn) 
	    (setf sum count)
	  ;; otherwise we have already the number of tokens in the opposite 
	  ;; directions. Thus we add them together
	  ;; -1 because we double count the token in the middle
	  (setf sum (+ (- sum 1) count)))
	(when (> sum 4)
	  (return-from game-over? plr)))))
  
  (when (= (gomoku-num-open game) 0)
    (return-from game-over? 0))
  nil)


;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The amount of the
;;    win/loss is reported by the SQUARE-ROOT of the absolute difference
;;    in the number of tokens for the two players.  For example, if the
;;    game ends with WHITE having 25 tokens and BLACK having 31 tokens,
;;    then black wins by 6 tokens, and the output is approximately 2.45.

(defun default-policy
    (game)
  ;; Do random moves until the game is over
  (let ((winner (game-over? game))
	(val 0))
    ;; return the winner of the game
    (while (not winner)
      (do-random-move! game)
      (setf winner (game-over? game)))
    (setf val (eval-func game))
    (/ val 1000000)))


;; constant array used in eval-helper
;; EVAL-HELPER scan the board in six direction:
;;      horizontally from left to right from the first column
;;      vertically downward from the first row
;;      diagonally to left down direction from the first row
;;      diagonally to left down direction from the last column
;;      diagonally to left up direction from the last row
;;      diagonally to left up direction from the last column

;; *SCAN-STARTS* records where to start in each of the six direction:
;;               (0 0) represents we start from (0 0): the first token 
;;               in the first row
;; *START-DIRNS* records the direction we move to next start point: (1 0) 
;;               representing moving to the next token in the first row
;;               - after scanning horizontally from (n 0), we scan 
;;                 horizontally from (n+1 0)
;; *SCAN-DIRNS* records the direction we scan: (0 1) represents 
;;              scanning horizontally)

(defconstant *scan-starts* (make-array '(6 2) :initial-contents
          '((0 0) (0 0) (0 0) (1 14) (14 0) (13 14))))

(defconstant *start-dirns* (make-array '(6 2) :initial-contents
          '((1 0) (0 1) (0 1) (1 0) (0 1) (-1 0))))

(defconstant *scan-dirns* (make-array '(6 2)
                                 :initial-contents
         '((0 1) (1 0) (1 -1) (1 -1) (-1 -1) (-1 -1))))

;;  EVAL-HELPER
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT: a 3-D array (PLR, BLOCK, LENGTH) of counter
;;          PLR - black(0) or white(1)
;;          BLOCK - open(0), block on one side(1), block on both side(2)
;;          LENGTH - length of consecutive tokens
;;          value is the number of n consecutive tokens
;; EXAMPLE: number of black open 3 consecutive is stored in (0,0,3)

(defun eval-helper
  (game)
  (let ((board (gomoku-board game))
    (count (make-array '(2 3 5) : initial-element 0)))
    ;; scan the board in 6 directions
    (dotimes (k 6)
      ;; initialize the start point, how to move the start point, 
      ;; and the direction of chain to scan
      (let ((start-r (aref *scan-starts* k 0))
            (start-c (aref *scan-starts* k 1))
            (start-dr (aref *start-dirns* k 0))
            (start-dc (aref *start-dirns* k 1))
            (dr (aref *scan-dirns* k 0))
            (dc (aref *scan-dirns* k 1)))
        ;; if the start point is not off-board, continue
        ;; (e.g, search from the first row)
        (while (not (off-board? start-r start-c))
             (let ((curr 0)
                   (last *border*)
                   ;; whether the chain is blocked before
                   ;; since start from the border, initialized as blocked
                   (beforeblock 1)
                   ;; whether the chain is blocked after
                   (afterblock 0)
                   ;; initialize counter
                   (counter 1)
                   ;; initialize position to start point,
                   (r start-r)
                   (c start-c))
                ;; if current position is not off-board, continue.
                (while (not (off-board? r c))
                      ;; get current token
                      (setf curr (aref board r c))
                      ;; if current token is same as before, 
                      ;; increment counter
                      (cond ((= curr last)
                             (incf counter)
                             ;; when there are more than 5, set it to 5
                             (when (> counter 5)
                             (setf counter 5)))
                            ;; otherwise update count to record the chain
                            (t                        
                              ;; The previous chain is not blocked after
                              ;; if and only if current token is *BLANK*
                              (if (= curr *blank*)
                                  (setf afterblock 0)
                                  (setf afterblock 1))
                              ;; update based on the color of the chain
                              ;; the next chain is blocked before if and
                              ;; only if the current token is not *BLANK*
                              (cond ((= last *white*) 
                                     (incf (aref count 1 
                                      (+ beforeblock afterblock) 
                                      (- counter 1)))
                                     (setf counter 1)
                                     (setf beforeblock 1))
                                    ((= last *black*)
                                     (incf (aref count 0 
                                      (+ beforeblock afterblock) 
                                      (- counter 1)))
                                     (setf counter 1)
                                     (setf beforeblock 1))
                                    ((= last *blank*)
                                     (setf counter 1)                                  
                                     (setf beforeblock 0)))
                              ;; update last token to current token
                              (setf last curr)))
                      ;; get next token
                      (setf r (+ r dr))
                      (setf c (+ c dc)))
                ;; record the last chain before we hit the border
                (setf afterblock 1)
                (cond ((= last *white*) 
                       (incf (aref count 1
                       (+ beforeblock afterblock) (- counter 1))))
                      ((= last *black*)
                       (incf (aref count 0
                       (+ beforeblock afterblock) (- counter 1))))))
             ;; update start point
             (setf start-r (+ start-r start-dr))
             (setf start-c (+ start-c start-dc)))))
  ;; return the 3-d array count
  count))


;;  EVAL-FUNC
;; -------------------------------------------------
;;  INPUT:  GAME, a gomoku struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based on the difference in piece values held by 
;;           the two players.

;; count consecutive 3 & 4
;; and whether surrounded by open space or opposite token


(defun eval-func
    (game)
  (let* ((count (eval-helper game))
	 (black-score
	  (+ (* *one* (aref count 0 0 0))
	     (* *two* (aref count 0 0 1))
	     (* *three* (aref count 0 0 2))
	     (* *four* (aref count 0 0 3))
	     (* *block-one* (aref count 0 1 0))
	     (* *block-two* (aref count 0 1 1))
	     (* *block-three* (aref count 0 1 2))
	     (* *block-four* (aref count 0 1 3))
	     (* *five* 
		(+ (aref count 0 0 4) (aref count 0 1 4) (aref count 0 2 4)))))
	 (white-score
	  (+ (* *one* (aref count 1 0 0))
	     (* *two* (aref count 1 0 1))
	     (* *three* (aref count 1 0 2))
	     (* *four* (aref count 1 0 3))
	     (* *block-one* (aref count 1 1 0))
	     (* *block-two* (aref count 1 1 1))
	     (* *block-three* (aref count 1 1 2))
	     (* *block-four* (aref count 1 1 3))
	     (* *five* 
		(+ (aref count 1 0 4) (aref count 1 1 4) (aref count 1 2 4))))))
    (- black-score white-score)))


    


;;  SETUP-GOMOKU
;; ---------------------------------------
;;  INPUT:  LIST-O-MOVES, a list of "moves" where each move is 
;;             a list of 4 numbers (of the kind used by do-move!)
;;  OUTPUT:  A GOMOKU struct representing a new game in which
;;    the moves in LIST-O-MOVES have been done.

(defun setup-gomoku (list-o-moves)
  (let ((g (new-gomoku)))
    ;; For each move in the LIST-O-MOVES...
    (mapcar #'(lambda (movie)
		;; Do the move...
		(apply #'do-move! g nil movie))
	    list-o-moves)
    g
    ))
 
