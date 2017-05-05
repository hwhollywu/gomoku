
(defstruct (gomoku (:print-function print-gomoku))
  ;; BOARD:  an 8-by-8 array of *white*, *black* or *blank* 
  (board (make-array '(19 19) :initial-element *blank*))      
  ;; WHOSE-TURN:  either *BLACK* or *WHITE*
  whose-turn
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= 60)
  num-open
  ;; WHITE-PIECES/BLACK-PIECES:  64-bit integers (as described above)
  white-pieces 
  black-pieces 
  )


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
;;  INPUT:   GAME, an OTHELLO struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
    ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
		:whose-turn (gomoku-whose-turn game)
		:num-open (gomoku-num-open game)
		:white-pieces (gomoku-white-pieces game)
		:black-pieces (gomoku-black-pieces game)))
