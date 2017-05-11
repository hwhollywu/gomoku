;; ========================================
;;  CMPU-365, Spring 2017
;;  FILE:  gomoku.lisp
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
  ;; NEW-POSN: the latest move
  new-posn
  ;; LEGAL-MOVES: a vector of all legal moves around the new-posn (+- 4 tokens)
  legal-moves
  ;; MOVE-HISTORY: a list of the moves that got us from initial state to the current state
  move-history
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
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
    ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
		:whose-turn (gomoku-whose-turn game)
		:num-open (gomoku-num-open game)
    :white-pieces (gomoku-white-pieces game)
    :black-pieces (gomoku-black-pieces game)
    :new-posn (gomoku-new-posn game)
    :legal-moves (gomoku-legal-moves game)
    :move-history (gomoku-move-history game)))

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
;;  The game starts at white player's turn, the black always put
;;  the token in the middle of board

(defun new-gomoku
    ()
  (let* ((game (make-gomoku :whose-turn *white*
			     :num-open 224
           :new-posn (row-col->posn 7 7)
           :black-pieces 0
           :white-pieces 0
           :legal-moves nil
           :move-history nil))
  (bored (gomoku-board game)))
  (place-token game bored *black* 7 7)
  (setf (gomoku-legal-moves game) (gen-legal-moves game))
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
;;     DO-RANDOM-MOVE!  ok  
;;     DEFAULT-POLICY   ok
;;     GAME-OVER?       ok
;;     EVAL-FUNC        ok
;;     UNDO-MOVE


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

;;  GEN-LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A list of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns a vector containing the *pass* move.

(defun gen-legal-moves (game)
  ;; create an initial empty list of moves 
  (let* ((moves NIL)
  (new-posn (gomoku-new-posn game))
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
      (setf moves 
        (cons (list row col) moves))
      )))
  ;; return vector form of moves
    moves))


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

  ;(format t "do-move!~%")
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
    ;; 4. set the new-posn to this move
    (setf (gomoku-new-posn game) (row-col->posn row col))
    ;; 5. set the legal moves for the game
    (setf (gomoku-legal-moves game) 
      (append (gen-legal-moves game) (gomoku-legal-moves game)))
    ;; 6. push move into move-history
    (push (list row col) (gomoku-move-history game))
    ;; return the game
    game))


;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defun random-move (game)
  (let ((moves (gomoku-legal-moves game)))
    ;; return at move at index 0 - (the length of moves available)
    (nth (random (length moves)) moves)))



;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the 
;;   legal moves available to the current player, chosen randomly.

(defun do-random-move! (game)
  (let ((chosenmove (random-move game)))
    (do-move! game nil (first chosenmove) (second chosenmove))))



;; UNDO-MOVE
;; -----------------------------

(defun undo-move! (g)
  (cond
   ;; Case 1:  No moves on move history!
   ((null (gomoku-move-history g))
    (format t "Umm... Can't undo move... empty history!~%")
    g)
   
   ;; Case 2:  There is a move to undo...
   (t
    (let* ((move (pop (gomoku-move-history g)))
     (bored (gomoku-board g))
     (r1 (first move))
     (c1 (second move))
     (r2 (third move))
     (c2 (fourth move))
     (piece (aref bored r2 c2))
     (destn (fifth move))
     (opponent (gomoku-whose-turn? g)))
      (when (aref bored r1 c1)
  (format t "Gonna undo move, but something was on source square!~%"))
      (when (null piece)
  (format t "Wanna undo a move, but there's no piece at destn!~%"))
      (when (and destn (not (eq opponent (piece-owner destn))))
  (format t "Umm... opponent not owner of captured piece~%"))
      ;; remove piece from (r2,c2)
      (setf (aref bored r2 c2) nil)
      ;; if necessary, restore previously captured piece to (r2,c2)
      (when destn (put-piece! g destn))
      ;; move piece back to (r1,c1)
      (setf (aref bored r1 c1) piece)
      (setf (piece-row piece) r1)
      (setf (piece-col piece) c1)
      ;; Toggle the turn!
      (toggle-turn! g)
      ;; Return the CHESS struct
      g))))


;;  GAME-OVER?
;; -----------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  If not over, return NIL. If game over, return the winner of the game.
;;           Return 0 if draw. 

(defun game-over? (game)
  (let ((current (gomoku-new-posn game))
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
;; EXAMPLE: number of black open 3 consecutive is stored in (0,0,3)

(defun eval-helper
       (game)
  (let ((board (gomoku-board game))
        (count (make-array '(2 3 5) :initial-element 0)))
    ;; horizontally
    ;;(format t "horizontally")
    (dotimes (i 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 1))
        (dotimes (j 16)
          (if (= j 15)
              (setf curr *border*)
              (setf curr (aref board i j)))
          ;;(format t "zuobiao 1~a ~a" i j)

          ;;(format t "zuobiao 2~a ~a" i j)
          (cond ((= curr last)
                (incf counter)
                (when (> counter 5)
                      (setf counter 5)))
            (t
              ;;(format t "zuobiao last ~a ~a~%" i j)
              (if (= curr *blank*)
                (setf afterblock 0)
                (setf afterblock 1))
              (cond ((= last *white*) 
                     (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *black*)
                     (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                     ;;(format t "~a ~a ~a ~%" i j counter)
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *blank*)
                     (setf counter 1)
                     (setf beforeblock 0)))
              (setf last curr))
          ;;(format t "zuobiao 3~a ~a" i j)
          ))))
    ;;(format t "counte ~a ~%" count)
    ;; vertically
        (dotimes (j 15)
      (let ((curr 0)
            (last *border*)
            (beforeblock 1)
            (afterblock 0)
            (counter 1))
        (dotimes (i 16)
          (if (= i 15)
              (setf curr *border*)
              (setf curr (aref board i j)))
          ;;(format t "zuobiao 1~a ~a" i j)

          ;;(format t "zuobiao 2~a ~a" i j)
          (cond ((= curr last)
                (incf counter)
                (when (> counter 5)
                      (setf counter 5)))
            (t
              ;;(format t "zuobiao last ~a ~a~%" i j)
              (if (= curr *blank*)
                (setf afterblock 0)
                (setf afterblock 1))
              (cond ((= last *white*) 
                     (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *black*)
                     (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                     ;;(format t "~a ~a ~a ~%" i j counter)
                     (setf counter 1)
                     (setf beforeblock 1))
                    ((= last *blank*)
                     (setf counter 1)
                     (setf beforeblock 0)))
              (setf last curr))
          ;;(format t "zuobiao 3~a ~a" i j)
          ))))
    ;; diagonally
    ;; letfdown from first row

    ;(format t "letfdown before ~a~%" count)
    (dotimes (k 14)
      ;;(format t "leftdown ~a th ~%" k)
    (let ((curr 0)
          (last *border*)
          (beforeblock 1)
          (afterblock 0)
          (counter 1)
          (i 0)
          (j k))
      (while (not (off-board? i j))
             ;;(format t "letfdown ~a ~a~%" i j)
             (setf curr (aref board i j))
              (cond ((= curr last)
                    (incf counter)
                    (when (> counter 5)
                      (setf counter 5)))
                (t
                  ;;(format t "zuobiao last ~a ~a~%" i j)
                  (if (= curr *blank*)
                    (setf afterblock 0)
                    (setf afterblock 1))
                  (cond ((= last *white*) 
                         (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *black*)
                         (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                         ;;(format t "~a ~a ~a ~%" i j counter)
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *blank*)
                         (setf counter 1)
                         (setf beforeblock 0)))
                  (setf last curr)))
             (incf i)
             (decf j))
      (setf afterblock 1)
      (cond ((= last *white*) 
             (incf (aref count 1 (+ beforeblock afterblock) (- counter 1))))
            ((= last *black*)
             (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))))))
    ;(format t "letfdown from first row ~a~%" count)

    ;; letfdown from last column    
    (dotimes (k 15)
      ;;(format t "leftdown ~a th ~%" k)
    (let ((curr 0)
          (last *border*)
          (beforeblock 1)
          (afterblock 0)
          (counter 1)
          (i k)
          (j 14))
      (while (not (off-board? i j))
             ;;(format t "letfdown ~a ~a~%" i j)
             (setf curr (aref board i j))
              (cond ((= curr last)
                    (incf counter)
                    (when (> counter 5)
                      (setf counter 5)))
                (t
                  ;;(format t "zuobiao last ~a ~a~%" i j)
                  (if (= curr *blank*)
                    (setf afterblock 0)
                    (setf afterblock 1))
                  (cond ((= last *white*) 
                         (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *black*)
                         (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                         ;;(format t "~a ~a ~a ~%" i j counter)
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *blank*)
                         (setf counter 1)
                         (setf beforeblock 0)))
                  (setf last curr)))
             (incf i)
             (decf j))
      (setf afterblock 1)
      (cond ((= last *white*) 
             (incf (aref count 1 (+ beforeblock afterblock) (- counter 1))))
            ((= last *black*)
             (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))))))
        ;(format t "letfdown from first row ~a~%" count)

    ;; leftup from last column
    (dotimes (k 14)
      ;;(format t "leftup ~a th ~%" k)
      (let ((curr 0)
          (last *border*)
          (beforeblock 1)
          (afterblock 0)
          (counter 1)
          (i k)
          (j 14))
      (while (not (off-board? i j))
             ;;(format t "letfdown ~a ~a~%" i j)
             (setf curr (aref board i j))
              (cond ((= curr last)
                    (incf counter)
                    (when (> counter 5)
                      (setf counter 5)))
                (t
                  (if (= curr *blank*)
                    (setf afterblock 0)
                    (setf afterblock 1))
                  (cond ((= last *white*) 
                         (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *black*)
                         (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                         ;;(format t "~a ~a ~a ~%" i j counter)
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *blank*)
                         (setf counter 1)
                         (setf beforeblock 0)))
                  (setf last curr)))
             (decf i)
             (decf j))
      (setf afterblock 1)
      (cond ((= last *white*) 
             (incf (aref count 1 (+ beforeblock afterblock) (- counter 1))))
            ((= last *black*)
             (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))))))
     ;(format t "leftup from last col ~a~%" count)

    ;; leftup from last row
    (dotimes (k 15)
      ;;(format t "leftup ~a th ~%" k)
      (let ((curr 0)
          (last *border*)
          (beforeblock 1)
          (afterblock 0)
          (counter 1)
          (i 14)
          (j k))
      (while (not (off-board? i j))
             ;;(format t "letfdown ~a ~a~%" i j)
             (setf curr (aref board i j))
              (cond ((= curr last)
                    (incf counter)
                    (when (> counter 5)
                      (setf counter 5)))
                (t
                  (if (= curr *blank*)
                    (setf afterblock 0)
                    (setf afterblock 1))
                  (cond ((= last *white*) 
                         (incf (aref count 1 (+ beforeblock afterblock) (- counter 1)))
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *black*)
                         (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))
                         ;;(format t "~a ~a ~a ~%" i j counter)
                         (setf counter 1)
                         (setf beforeblock 1))
                        ((= last *blank*)
                         (setf counter 1)
                         (setf beforeblock 0)))
                  (setf last curr)))
             (decf i)
             (decf j))
      (setf afterblock 1)
      (cond ((= last *white*) 
             (incf (aref count 1 (+ beforeblock afterblock) (- counter 1))))
            ((= last *black*)
             (incf (aref count 0 (+ beforeblock afterblock) (- counter 1)))))))
    count))




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
              (+ (aref count 0 0 4) (aref count 0 1 4) (aref count 0 1 4)))))
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
              (+ (aref count 1 0 4) (aref count 1 1 4) (aref count 1 1 4))))))
      (- black-score white-score)))
      

    


