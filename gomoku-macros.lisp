;; ===============================================
;;  CMPU-365, Spring 2017
;;  FILE:  gomoku-macros.lisp
;; ===============================================

;;  Some useful constants
;; -----------------------------------

(defconstant *blank* 0)

;;  How tokens of each color are displayed

(defconstant *black-show* 'B)
(defconstant *white-show* 'W)
(defconstant *blank-show* '_)

;;  The PASS move is represented thusly:

(defconstant *pass* '(nil nil))


;;  A POSN is just a number from 0 to 323, that refers to one of the
;;  squares on the 19-by-19 gomoku game board.  The following macros
;;  convert between the POSN and ROW/COL representations.

;;  POSN->ROW
;; ---------------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to 224)
;;  OUTPUT:  The corresponding ROW (an integer from 0 to 14)

(defmacro posn->row (posn) `(floor ,posn 15))

;;  POSN->COL
;; ---------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to 224)
;;  OUTPUT:  The corresponding COLUMN (an integer from 0 to 14)

(defmacro posn->col (posn) `(mod ,posn 15))

;;  ROW-COL->POSN
;; ------------------------------------------
;;  INPUTS:  ROW, COL, two integers each between 0 and 14
;;  OUTPUT:  The corresponding POSN (an integer between 0 and 224)

(defmacro row-col->posn (row col) `(+ (* 15 ,row) ,col))

;;  IF-BLACK-TURN
;; -------------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           THEN, ELSE, any two Lisp expressions
;;  OUTPUT:  If it's black's turn, then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black-turn (g then else)
  `(if (eq (gomoku-whose-turn ,g) ,*black*)
       ,then ,else))

;;  IF-BLACK
;; --------------------------------------------------------
;;  INPUTS:  PLR, either *BLACK* or *WHITE*
;;           THEN, ELSE, any Lisp expressions
;;  OUTPUT:  If PLR is *BLACK* then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black (plr then else)
  `(if (eq ,plr ,*black*) ,then ,else))

;;  OTHER-PLAYER
;; ----------------------------------------------------------
;;  INPUT:   PLR, either *BLACK* or *WHITE*
;;  OUTPUT:  The other player (i.e., either *WHITE* or *BLACK*)

(defmacro other-player (plr)
  `(if (= ,plr ,*black*) ,*white* ,*black*))

;;  TOGGLE-PLAYER!
;; ---------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The player whose turn it now is (either *BLACK* or *WHITE*)
;;  SIDE EFFECT:  Destructively modifies the game to toggle whose turn it is.

(defmacro toggle-player! (game)
  `(setf (gomoku-whose-turn ,game)
     (if-black (gomoku-whose-turn ,game) ,*white* ,*black*)))

;;  PLACE-TOKEN-AT-POSN
;; --------------------------------------------------------------
;;  INPUTS:  BOARD, a 15-by-15 array
;;           TOKEN, either *BLACK* or *WHITE*
;;           POSN, an integer from 0 to 224
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Destructively modifies BOARD by inserting TOKEN
;;                at the position determined by POSN

(defmacro place-token-at-posn
    (board token posn)
  `(setf (aref ,board (posn->row ,posn) (posn->col ,posn)) ,token))

;;  PLACE-TOKEN
;; -------------------------------------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           BORED, a 15-by-15 array
;;           PLR, either *BLACK* or *WHITE*
;;           ROW, COL, integers between 0 and 14
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Places TOKEN on BORED at specified ROW/COL.
;;    Also updates the 64-bit vector for the appropriate player
;;    (see the STATE struct)

(defmacro place-token
    (game bored plr row col)
  `(progn (setf (aref ,bored ,row ,col) ,plr) ))
;;	  (if-black ,plr (incf (othello-black-pieces ,game)
;;			       (ash 1 (row-col->posn ,row ,col)))
;;		    (incf (othello-white-pieces ,game)
;;			  (ash 1 (row-col->posn ,row ,col))))))

;;  OFF-BOARD?
;; ------------------------------------------
;;  INPUTS:  ROW, COL, two integers
;;  OUTPUT:  T if (ROW,COL) does not specify a legal position
;;              in a 15-by-15 array; NIL otherwise.

(defmacro off-board?
    (row col)
  `(or (< ,row 0)
       (< ,col 0)
       (> ,row 14)
       (> ,col 14)))


