
(defparameter *board*
  (make-instance 'board:board))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'player:human :player-type 'human :player-name "Ryu" :stone-type constants:+black+))
(defparameter *cpu*
  (make-instance 'player:cpu :player-type 'cpu :player-name "MOCCA" :stone-type constants:+white+))


(defun main ()
  ;; initialize bord
  (setq *board*
    (make-instance 'board:board))

  ;; Player & CPU sample
  (setq *player*
    (make-instance 'player:human :player-type 'human :player-name "Ryu" :stone-type constants:+black+))
  (setq *cpu*
    (make-instance 'player:cpu :player-type 'cpu :player-name "MOCCA" :stone-type constants:+white+))

  ;; game-start
  (board:display-grid *board*) ;; show board 
  (let ((turn-player *player*)
	(turn-cnt 0)
	(pass-cnt 0)
	(put-point-plist '(:row 2 :column 2 :stone-type #\○)))
    
    ;; loop
    (loop while (and (< pass-cnt 2) (< turn-cnt 5)) ;; 置けない現象が2回ずついたら終わり
	  do (if (eql turn-player *player*)
		 
		 (progn
		   (format t "Your turn.~% Put Your Stone(~a)~%" (player:stone-type *player*)) ;; message for putting stone
		   (loop while (eql (board:can-put-stone (getf put-point-plist :row)
							 (getf put-point-plist :column)
							 (getf put-point-plist :stone-type)
							 *board*)
				    nil)
			 do (setf put-point-plist (player:decide-put-point *player*))) ;; decide put next stone
		   
		   (board:put-stone (getf put-point-plist :row)
				    (getf put-point-plist :column)
				    (getf put-point-plist :stone-type)
				    *board*)
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
    (format t "WHITE: ~a, BLACK: ~a~%" 1 1) ;; 白と黒の数を出力
    (format t "~a is win~%" 1)) ;; 買ったほうを伝える
  )
