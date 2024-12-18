;; 目的：以下の３つに慣れる
;;; 1. lisp
;;; 2. CLOS
;;; 3. マクロ

;; 定数
(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")
(defconstant +space+ #\． "Symbol for white stone.")

;; Board Class
;; Board情報/書き換え/出力/チェック
(defclass board ()
  ((grid :initarg :grid
   	 :initform (initialise-grid)
	 :accessor grid
	 :documentation "Grid Infomation")))

(defun initialize-grid ()
  ;; initialize grid status.
  (let ((grid (make-array '(8 8) :initial-element +space+)))
    (setf (aref grid 3 3) +black+)
    (setf (aref grid 4 4) +black+)    
    (setf (aref grid 3 4) +white+)
    (setf (aref grid 4 3) +white+)
    grid))

(defgeneric display-grid (board)
  (:documentation "Display board info."))

(defmethod display-grid ((board board))
  ;; TODO：ボードを表示する際に行番号と列番号を表示できるようにしたい
  ;; (progn
  ;;   (format t "  ")
  ;;   (dotimes (i 8) (format t "~A" i)
  ;;   (format t "~%"))
  (dotimes (i 8)
    (dotimes (j 8)
      (format t "~a" (aref (grid board) i j)))
    (format t "~%")))

;; Player Class
;; Player情報/Action
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

;; Human Class -> Player Class
(defclass human (player) ())

;; CPU Class -> Player Class
(defclass cpu (player) ())

;; Player & CPU sample
(defparameter *player*
  (make-instance 'human :player-type 'human :player-name "Ryu" :stone-type +black+))

(defparameter *cpu*
  (make-instance 'human :player-type 'cpu :player-name "MOCCA" :stone-type +white+))


;; User Interface

;; main
;;; game-start
;;;; print
;;;; put
;;;; update -> end if game close
;;;; player change






