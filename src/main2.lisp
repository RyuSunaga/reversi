
(load "constants.lisp")
(load "board.lisp")
(load "player.lisp")


;; board
(defparameter *board*
  (make-instance 'board:board))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'player:human :player-type 'human :player-name "Ryu" :stone-type +black+))

(defparameter *cpu*
  (make-instance 'player:cpu :player-type 'cpu :player-name "MOCCA" :stone-type +white+))


(defun main ()
  
  )
