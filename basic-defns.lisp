;; ===========================================
;;  CMPU-365, Spring 2017
;;  Basic Definitions for GOMOKU
;; ===========================================

(defconstant *black* -1)
(defconstant *white* 1)
(defconstant *border* 100)


(defparameter *neg-inf* -10000000)
(defparameter *pos-inf*  10000000)


(defparameter *list-o-files* 
    (list "basic-defns"        ;; this file
	  "gomoku-macros"     ;; macros, etc.   
	  "gomoku"    ;; struct and basics
	  "mcts"
    "alpha-beta"
	  )) 

;;  MAKER
;; ------------------------------------------------
;;  To facilitate recompiling and reloading your files
;;  after you make changes.

(defun maker
    ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file)))



