;; ===========================================
;;  CMPU-365, Spring 2017
;;  Basic Definitions for GOMOKU
;;  Lilian Zhao and Hao Wu
;; ===========================================;; 

(defconstant *black* -1)
(defconstant *white* 1)
(defconstant *border* 100)


(defparameter *neg-inf* -10000000)
(defparameter *pos-inf*  10000000)
(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)


(defparameter *list-o-files* 
  (list "basic-defns"        
	"gomoku-macros"
	"gomoku"     
	"mctsrave"
    "mcts"
    "alpha-beta"
    "testing"
	"ravetest")) 

;;  MAKER
;; ------------------------------------------------
;;  To facilitate recompiling and reloading your files
;;  after you make changes.

(defun maker
    ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file)))



