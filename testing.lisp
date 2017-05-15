;; ====================================
;;  CMPU-365, Spring 2017
;;  Testing functions for GOMOKU
;;  Lilian Zhao and Hao Wu
;; ====================================


(defun mcts-compete
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (uct-search g black-num-sims black-c))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g white-num-sims white-c))))))
    (format t "~A~%" (eval-func g))))



(defun ab-compete
    (depth-black depth-white)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (compute-move g depth-black))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (compute-move g depth-white))))))
    (format t "~A~%" (eval-func g))))


(defun ab-mcts-compete-black-ab
    (depth num-sims c)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (compute-move g depth))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g num-sims c))))))
    (format t "~A~%" (eval-func g))))



(defun ab-mcts-compete-black-mcts
    (num-sims c depth)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (uct-search g num-sims c))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (compute-move g depth))))))
    (format t "~A~%" (eval-func g))))

