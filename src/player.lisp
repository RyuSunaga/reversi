(defpackage :player
  (:use :cl :constants)
  (:export
   :human
   :cpu
   :stone-type))

(in-package :player)

;; Player Class
(defclass player ()
  ((player-type :initarg :player-type
		:initform (error "Must set player type.")
		:accessor player-type
		:documentation "Player type. Human or CPU")
   (player-name :initarg :player-name
		:initform (error "Must set player name.")
		:accessor player-name)
   (stone-type :initarg :stone-type
	       :initform (error "Must set stone type.")
	       :accessor stone-type
	       :documentation "Player stone type.")))

;; Human Class -> Playerqqqqqqq Class
(defclass human (player) ())

;; CPU Class -> Player Class
(defclass cpu (player) ())

(defgeneric decide-put-point (player)
  (:documentation "Decide put point."))

(defmethod decide-put-point ((p human))
  ;; If human put a stone. The point is read by repl.

  (let ((r nil) (c nil) (stone-type (stone-type p)))
    ;; READ INPUT    
    (format t "Please input row number...~% row:")
    (setf r (read))
    (format t "Please input row number...~% column:")
    (setf c (read))
    (format t "Row: ~a Column: ~a Stone: ~a~%" r c stone-type)
    `(:row ,r :column ,c :stone-type ,stone-type)))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'human :player-type 'human :player-name "Ryu" :stone-type +black+))

(defparameter *cpu*
  (make-instance 'human :player-type 'cpu :player-name "MOCCA" :stone-type +white+))
