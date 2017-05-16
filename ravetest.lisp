(defun compete
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