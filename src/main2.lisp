;; initialize bord
(defparameter *board*
  (make-instance 'board:board))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'player:human :player-type 'human :player-name "Ryu" :stone-type constants:+black+))
(defparameter *cpu*
  (make-instance 'player:cpu :player-type 'cpu :player-name "MOCCA" :stone-type constants:+white+))


(defun main ()
  ;; game-start
 
  (let ((turn-player *player*)
	(turn-cnt 0)
	(pass-cnt 0))
    
    ;; loop
    (loop while (and (< pass-cnt 2) (< turn-cnt 5)) ;; 置けない現象が2回ずついたら終わり
	  do (if (eql turn-player *player*)
		 
		 (progn
		   (format t "Your turn.~%") ;; message for putting stone
		   (board:put-stone 0 0 (player:stone-type turn-player) *board*)
		   ()				;; board update
		   (board:display-grid *board*) ;; show board
		   (setq turn-player *cpu*)
		   (setq turn-cnt (1+ turn-cnt))) ;; change-turn player
		 
		 (progn
		   (format t "CPU turn.~%") ;; message for putting stone
		   () ;; check the postion can put stone
		   (board:put-stone 5 5 (player:stone-type turn-player) *board*)		   
		   ()				;; board update
		   (board:display-grid *board*) ;; show board		   
		   (setq turn-player *player*)	;; chagen turn-player
		   (setq turn-cnt (1+ turn-cnt))
		   )))
    ;; result
    (format t "WHITE: ~a, BLACK: ~a~%" 1 1) ;; 白と黒の数を出力
    (format t "~a is win~%" 1) ;; 買ったほうを伝える
    )
  )
