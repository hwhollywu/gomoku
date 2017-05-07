;; ========================================
;;  CMPU-365, Spring 2017
;;  FILE:  gomoku.lisp
;;  Lilian Zhao and Hao Wu
;; ========================================


;;  The GOMOKU struct
;; --------------------------------------------------------

(defstruct (gomoku (:print-function print-gomoku))
  ;; BOARD:  a 19-by-19 array of *white*, *black* or *blank* 
  (board (make-array '(19 19) :initial-element *blank*))      
  ;; WHOSE-TURN:  either *BLACK* or *WHITE*
  whose-turn
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= 60)
  num-open
  ;; num of open 3, open 4, etc.
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
    (format str "~% |  0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18~%")
    (format str "---------------------------------------------------~%")
    (dotimes (r 19)
      (format str "~A| " r)
      (dotimes (c 19)
	(let ((token (aref bored r c)))
	  (format str "~A  " (cond ((eq token *black*) *black-show*)
				  ((eq token *white*) *white-show*)
				  (t *blank-show*)))))
      (format str "~%"))
    (format str "~%")
    (format str "                   It is ~A's turn!~%" 
	    (if-black-turn g *black-show* *white-show*))
    ))



;;  NEW-OTHELLO
;; --------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  An OTHELLO struct representing a new game

(defun new-gomoku
    ()
  (let* ((game (make-gomoku :whose-turn *black*
			     :num-open 60))
	 (bored (gomoku-board game)))
    game))

