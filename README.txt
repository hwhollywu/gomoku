;; ====================================
;;  CMPU-365, Spring 2017
;;  Gomoku 
;;  Lilian Zhao and Hao Wu
;; ====================================

---------------------------------------------------------------------
;;  README
----------------------------------------------------------------------
FILES included in the project:
  basic-defns.lisp
  gomoku-macros.lisp
  gomoku.lisp
  alpha-beta.lisp
  mcts.lisp
  mctsrave.lisp 
  testing.lisp
  testing-result.txt

BEGIN by loading "basic-defns.lisp" in common-lisp. Type: (load “basic-defns.lisp”)
To compile-and-load implementation, type:  (maker)

To PLAY a game:

  (setf g (new-gomoku))

  (do-move! g nil 7 8)  ;; <--- Place a token in position (7 8)

  OR: (do-random-move! g) ;;  <--- Place a random token for the game

To PLAY a game versus AI:

  (uct-search g 1000 2) ;; <--- Ask monte carlo tree search to compute a move using
                         ;;      1000 simulations and c = 2

  (apply #'do-move! g nil (uct-search g 1000 2)) ;; <--- Do the move based on the mcts move


  (compute-move g 4) ;; <--- Ask alpha-beta pruning to compute a move using 4 cutoff-depth 

  (apply #'do-move! g nil (compute g 4)) ;; <--- Do the move based on the alpha-beta move


To TEST a game / to make alpha-beta and mcts competing with each other:

  (compete-mcts 1000 2 10000 2) ;; <--- Ask monte carlo tree search to compete with itself, 
;; using 1000 simulations and c=2 for the black player 
;; and 10000 simulations and c=2 for the white player

  (compete-ab 4 2) ;; <--- Ask alpha-beta pruning to compete with itself,
    ;; using cut-off depth 4 for the black player and 2 for the white player

  (compete-ab-mcts 4 1000 2) ;; <--- Ask alpha-beta pruning to compete 
;; with monte carlo tree search, 
;; with alpha-beta being the black player.
;; Using cut-off depth 4 for the black player,  
;; 1000 simulations and c=2 for the white player. 

  (compete-mcts-ab 1000 2 4) ;; <--- Ask alpha-beta pruning to compete 
;; with monte carlo tree search, 
;; with monte carlo tree search being black.
;; Using 1000 simulations and c=2 for the black player. 
;; cut-off depth 4 for the white player. 

More testing functions are defined in testing.lisp. Alpha-beta pruning takes one input, the cutoff-depth; monte carlo tree search takes two inputs, number of simulations and c; RAVE takes only number of simulations. 