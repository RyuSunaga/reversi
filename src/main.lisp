;; 目的：以下の３つに慣れる
;;; 1. lisp
;;; 2. CLOS
;;; 3. マクロ

;; board values
(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")
(defconstant +space+ #\． "Symbol for white stone.")

;; message values
(defconstant +sart+ "GAME START")
(defconstant +end+  "GAME FINISHED")

(defconstant +human-turn+ "YOUR TURN")
(defconstant +cpu-turn+ "CPU TURN")

(defconstant +human-win+ "YOUR WIN!!!")
(defconstant +human-lose+ "YOUR LOSE...")

;; put time
(defconstant +cpu-thinking+ "CPU is thinking...")
(defconstant +put-error+ "You can't put the point")


;; Board Class
;; Board情報/書き換え/出力/チェック
(defclass board ()
  ((grid :initarg :grid
   	 :initform (initialize-grid)
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
  (dotimes (i 8)
    (dotimes (j 8)
      (format t "~a" (aref (grid board) i j)))
    (format t "~%")))

(defgeneric put-stone (r c stone board)
  (:documentation "Updage grid stone."))

(defmethod put-stone (r c stone (board board))
  ;; Put stone grid (r c)
  (setf (aref (grid board) r c) stone))

;; TODO: put-stoneをしてすぐに結果を見るようにできないか？


;; Player Class
;; Player/PlayeAction
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

  (let ((r nil) (c nil) (stone (stone-type p)))
    ;; READ INPUT    
    (format t "Please input row number...~% row:")
    (setf r (read))
    (format t "Please input row number...~% column:")
    (setf c (read))
    (format t "Row: ~a Column: ~a Stone: ~a~%" r c stone)
    (list r c stone)))


;; board
(defparameter *board*
  (make-instance 'board))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'human :player-type 'human :player-name "Ryu" :stone-type +black+))

(defparameter *cpu*
  (make-instance 'human :player-type 'cpu :player-name "MOCCA" :stone-type +white+))

;; main
;;; game-start
;;;; print
;;;; put
;;;; update -> end if game close
;;;; player change

(defun main ()

  ;; game-start

  ;; initialize
  ;; board, player, cpu
  (setq *board* (make-instance 'board))
  (setq *player* (make-instance 'human :player-type 'human :player-name "Ryu" :stone-type +black+))
  (setq *cpu* (make-instance 'human :player-type 'cpu :player-name "MOCCA" :stone-type +white+))

  
  ;; display info: Player-StoneType, CPU-StoneType, Which starts the game.
  
  ;; loop
  ;; TODO: (loop (is-continue *board*))

  ;; game-end
  
  )

