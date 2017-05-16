;; ====================================
;;  CMPU-365, Spring 2017
;;  Gomoku Project Report
;;  Lilian Zhao and Hao Wu
;; ====================================

GitHub address: https://github.com/hwhollywu/gomoku

---------------------------------------------------------------------
1. Introduction
----------------------------------------------------------------------
Gomoku is a classic strategy board game (also known as Gobang and Five in a row) originated in Japan. It is played on a board with 15x15 intersections by two players, the black and white. The black player always goes first. To win the game, one player must have a row of five tokens horizontally, vertically or diagonally. Some simple strategies involved in gomoku include “blocking fours” and “created opened threes”.

There are different variations involved in playing gomoku. For example, the Standard gomoku requires a row of "exactly" five tokens for a win, but free-style gomoku accepts a row of five or more tokens (aka overlines) for a win. The implementation of gomoku in this project simulates the free-style gomoku. In gomoku, there is a slight advantage for the black player because it is the starting player. It is theoretically proven that black has a sure win. Therefore, there are different versions of rules to limit the advantage of the black player. In our game, we implemented the “three and three” ban, which means the black is not allowed to simultaneously form two open rows of three tokens.

---------------------------------------------------------------------
2. Approach
----------------------------------------------------------------------

This project includes the implementation of gomoku in Lisp, and the application of alpha-beta pruning and monte carlo tree search algorithm for Gomoku. 

To implement gomoku in Lisp, we define the the Gomoku struct by the following elements:

  BOARD :  a 15 by 15 array, initially as all *blank*
  WHOSE-TURN : *black* or *white* player’s turn, switch after each move
  BLACK-PIECES, WHITE-PIECES : 64-bit integers representing black and white pieces
  NUM-OPEN : number of empty slots in the board, ranged from 0 to 224
  LEGAL-MOVES : a vector of legal moves that new token can be placed in the current game
  MOVE-HISTORY : a list of the moves that get to the current game

The board helps to maintain the game structure by showing blank, black or white tokens. We use whose-turn, black-pieces and white-pieces to form a more concise the hash-key in monte carlo tree search for different game state. Num-open keeps track of the blank slots, and is used in game-over function to check the draw. Legal-moves is used in both algorithms as a set of potential next tokens. Move-history keeps track of the latest move on the board, and it is used in backtracking. 

We define the following basic functions for implementation:

  NEW-GOMOKU: 
  IS-LEGAL?
  GEN-LEGAL-MOVES
  DO-MOVE!
  UNDO-MOVE!
  GAME-OVER?  
  DEFAULT-POLICY 
  EVAL-FUNC

NEW-GOMOKU starts from the empty board with the first black token being placed in the middle of the board, according to the custom of gomoku. 
IS-LEGAL and GEN-LEGAL-MOVES define the legal moves as the 24 neighbor tokens (+- 2 tokens) around the latest move on the board, and make sure the legal-moves vector do not have duplicates.
DO-MOVE! and UNDO-MOVE! destructively modify the game struct by placing/removing a token from the board.
GAME-OVER? defines the game to be over when there is five-in-a-row and return 0 when there is a draw.
DEFAULT-POLICY is used in monte carlo tree search and will randomly play the game.
To evaluate the game, we assign different scores for open and blocked ones, twos, threes, fours and fives, and go through the board horizontally, vertically, diagonally to count the numbers of different n-in-a-row structures. EVAL-FUNC uses EVAL-HELPER, which returns a 3D array (Player, Block, Length) to represent the number of each n-in-a-row, and EVAL-FUNC returns an integer showing the value of (black - white).

We also optimizes the algorithms by finding a relative efficient static evaluation function to implement alpha-beta pruning, and implementing RAVE (Rapid Action Value Estimation) in monte carlo tree search.

---------------------------------------------------------------------
3. README
----------------------------------------------------------------------
FILES included in the project:
  basic-defns.lisp
  gomoku-macros.lisp
  gomoku.lisp
  alpha-beta.lisp
  mcts.lisp
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

  (mcts-compute 1000 2 10000 2) ;; <--- Ask monte carlo tree search to compete with itself, 
;; using 1000 simulations and c=2 for the black player 
;; and 10000 simulations and c=2 for the white player

  (ab-compute 4 2) ;; <--- Ask alpha-beta pruning to compete with itself,
		;; using cut-off depth 4 for the black player and 2 for the white player

  (ab-mcts-compete-black-ab 4 1000 2) ;; <--- Ask alpha-beta pruning to compete 
;; with monte carlo tree search, 
;; with alpha-beta being the black player.
;; Using cut-off depth 4 for the black player,  
;; 1000 simulations and c=2 for the white player. 

  (ab-mcts-compete-black-mcts 1000 2 4) ;; <--- Ask alpha-beta pruning to compete 
;; with monte carlo tree search, 
;; with monte carlo tree search being black.
;; Using 1000 simulations and c=2 for the black player. 
;; cut-off depth 4 for the white player. 


---------------------------------------------------------------------
4. Discussion
----------------------------------------------------------------------
