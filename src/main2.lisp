
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
	(put-point-plist '(:row 2 :column 2 :stone-type constants:+black+)))
    
    ;; loop
    (loop while (< pass-cnt 2) ;; 置けない現象が2回ずついたら終わり
	  do (if (eql turn-player *player*)
		 
		 (progn
		   (format t "Your turn.~% Put Your Stone(~a)~%" (player:stone-type turn-player)) ;; message for putting stone

		   ;; TODO: このifの処理はplayer/cpu共通なのでまとめたい
		   (if (board:can-put-stone-somewhere (player:stone-type turn-player) *board*) ;; 置ける場所があるかを確認
		       (progn
			 (setf pass-cnt 0)
			 (loop while (eql (board:can-put-stone (getf put-point-plist :row)
							       (getf put-point-plist :column)
							       (getf put-point-plist :stone-type)
							       *board*)
					  nil)
			       do (setf put-point-plist (player:decide-put-point turn-player))) ;; decide put next stone
			 
			 (board:put-stone (getf put-point-plist :row)
					  (getf put-point-plist :column)
					  (getf put-point-plist :stone-type)
					  *board*))
		       (progn
			 (incf pass-cnt 1)
			 (format t "You can't put stone anywhere.~%")))
		   
		   (board:display-grid *board*) ;; show board
		   (setq turn-player *cpu*)
		   (setq turn-cnt (1+ turn-cnt))) ;; change-turn player
		 
		 (progn
		   (format t "CPU turn.~% Put CPU Stone(~a)~%" (player:stone-type turn-player)) ;; message for putting stone
		   
		   (if (board:can-put-stone-somewhere (player:stone-type turn-player) *board*) ;; 置ける場所があるかを確認
		       (progn
			 (setf pass-cnt 0)
			 (loop while (eql (board:can-put-stone (getf put-point-plist :row)
							       (getf put-point-plist :column)
							       (getf put-point-plist :stone-type)
							       *board*)
					  nil)
			       do (setf put-point-plist (player:decide-put-point turn-player))) ;; decide put next stone
			 
			 (board:put-stone (getf put-point-plist :row)
					  (getf put-point-plist :column)
					  (getf put-point-plist :stone-type)
					  *board*))
		       (progn
			 (incf pass-cnt 1)
			 (format t "CPU can't put stone anywhere.~%")))

		   (board:display-grid *board*) ;; show board		   
		   (setq turn-player *player*)	;; chagen turn-player
		   (setq turn-cnt (1+ turn-cnt))
		   )))

    ;; TODO: 結果出力
    (format t "WHITE: ~a, BLACK: ~a~%" 1 1) ;; 白と黒の数を出力
    (format t "~a is win~%" 1)) ;; 買ったほうを伝える
  )
