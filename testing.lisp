;; ====================================
;;  CMPU-365, Spring 2017
;;  Testing functions for GOMOKU
;;  Lilian Zhao and Hao Wu
;; ====================================


(defun compete-mcts
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



(defun compete-ab
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


(defun compete-rave
    (black-num-sims white-num-sims )
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
  (format t "BLACK'S TURN!~%")
  (format t "~A~%" 
    (apply #'do-move! g nil (mc-rave g black-num-sims ))))
       (t
  (format t "WHITE'S TURN!~%")
  (format t "~A~%"
    (apply #'do-move! g nil (mc-rave g white-num-sims ))))))))


(defun compete-ab-mcts
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


(defun compete-mcts-ab
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


(defun compete-rave-mcts
    (black-num-sims white-num-sims c)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
  (format t "BLACK'S TURN!~%")
  (format t "~A~%" 
    (apply #'do-move! g nil (mc-rave g black-num-sims ))))
       (t
  (format t "WHITE'S TURN!~%")
  (format t "~A~%"
    (apply #'do-move! g nil (uct-search g white-num-sims c))))))
    (format t "~A~%" (eval-func g))))


(defun compete-mcts-rave
    (black-num-sims c white-num-sims )
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
  (format t "BLACK'S TURN!~%")
  (format t "~A~%" 
    (apply #'do-move! g nil (uct-search g black-num-sims c))))
       (t
  (format t "WHITE'S TURN!~%")
  (format t "~A~%"
    (apply #'do-move! g nil (mc-rave g white-num-sims ))))))
    (format t "~A~%" (eval-func g))))


(defun compete-ab-rave
    (depth num-sims )
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
    (apply #'do-move! g nil (mc-rave g num-sims ))))))
    (format t "~A~%" (eval-func g))))



(defun compete-rave-ab
    (num-sims depth)
  (let ((g (new-gomoku)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
  (format t "BLACK'S TURN!~%")
  (format t "~A~%" 
    (apply #'do-move! g nil (mc-rave g num-sims))))
       (t
  (format t "WHITE'S TURN!~%")
  (format t "~A~%"
    (apply #'do-move! g nil (compute-move g depth))))))
    (format t "~A~%" (eval-func g))))