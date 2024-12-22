;;;; My Lisp training program.
;;;; Author: Ryu
;;;; Description: Simple reversi program.


;;;; ---- Constant Values ----

(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")
(defconstant +space+ #\． "Symbol for white stone.")

(defconstant +row-min-inclusive+ 0)
(defconstant +row-max-inclusive+ 7) 

(defconstant +column-min-inclusive+ 0)
(defconstant +column-max-inclusive+ 7) 


;; ;; message values
;; (defconstant +start+ "GAME START")
;; (defconstant +end+  "GAME FINISHED")

;; (defconstant +human-turn+ "YOUR TURN")
;; (defconstant +cpu-turn+ "CPU TURN")

;; (defconstant +human-win+ "YOUR WIN!!!")
;; (defconstant +human-lose+cd "YOUR LOSE...")

;; ;; put time
;; (defconstant +cpu-thinking+ "CPU is thinking...")
;; (defconstant +put-error+ "You can't put the point")

(defvar *zenkaku-numbers* '("０" "１" "２" "３" "４" "５" "６" "７" "８"))



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

(defun get-other-stone (stone)
  (cond
    ((char= stone +white+) +black+)
    ((char= stone +black+) +white+)
    (t nil)))

(defgeneric display-grid (board)
  (:documentation "Display board info."))
(defgeneric put-stone (r c stone board)
  (:documentation "Updage grid stone."))
(defgeneric can-put-stone (r c stone board)
  (:documentation "Check the point can put sotne"))


(defmethod display-grid ((board board))
  
  (format t "~a" " ")
  (dotimes (i 8) (format t "~a" (elt *zenkaku-numbers* i)))
  (format t "~%")
	  
  (dotimes (i 8)
    (format t "~a" (elt *zenkaku-numbers* i))
    (dotimes (j 8)
      (format t "~a" (aref (grid board) i j)))
    (format t "~%")))

(defmethod put-stone (r c stone (board board))
  ;; Put stone grid (r c)
  (setf (aref (grid board) r c) stone))



(defmethod can-put-stone (r c stone (board board))
  ;; 石がおける条件は２つ
  ;; 1. 空いているスペースがある
  ;; 2. 相手の石を挟むことができる

  ;; 1. 対象の箇所が空いていなければその時点でnil
  (when (char/= (aref (grid board) r c) +space+)
      (return-from can-put-stone nil))

  ;; 2. 相手の石を挟むことができる
  (flet
      ((check-specific-direction (dr dc)
	 (let ((comb nil) (nstone stone) (sr r) (sc c)) ;; s: start, d: direction
	   (loop
	     while
	     (and ;; next stone position include in grid.
	      (<= +row-min-inclusive+ (+ sr dr))
	      (<= (+ sr dr) +row-max-inclusive+)
	      (<= +column-min-inclusive+ (+ sc dc))
	      (<= (+ sc dc) +column-max-inclusive+))
	     do
		(setf sr (+ sr dr));; update row
		(setf sc (+ sc dc));; update column
		(setf nstone (aref (grid board) sr sc));; TODO: get-stone
		(if (char= nstone stone);; 同じ石か
		    (if comb;;1度でも連なったか
			(return-from check-specific-direction t) 
			(return-from check-specific-direction nil))
		    (if (char= nstone +space+);; スペース or 違う石
			(return-from check-specific-direction nil)
			(setf comb t)))))
	 (return-from check-specific-direction nil)))
  
    (loop for i from -1 to 1
	  do
	     (loop for j from -1 to 1
		   do
		      (when (check-specific-direction i j)
			(return-from can-put-stone t))))
    (return-from can-put-stone nil)))
				  

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

  (let ((r nil) (c nil) (stone-type (stone-type p)))
    ;; READ INPUT    
    (format t "Please input row number...~% row:")
    (setf r (read))
    (format t "Please input row number...~% column:")
    (setf c (read))
    (format t "Row: ~a Column: ~a Stone: ~a~%" r c stone-type)
    `(:row ,r :column ,c :stone-type ,stone-type)))


;; board
(defparameter *board*
  (make-instance 'board))

;; Player & CPU sample
(defparameter *player*
  (make-instance 'human :player-type 'human :player-name "Ryu" :stone-type +black+))

(defparameter *cpu*
  (make-instance 'human :player-type 'cpu :player-name "MOCCA" :stone-type +white+))
p

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

;;;; TestCase
;;  ０１２３４５６７
;; ０．．．．．．．．
;; １．．．．．．．．
;; ２．．．．．．．．
;; ３．．．○●．．．
;; ４．．．●○．．．
;; ５．．．．．．．．
;; ６．．．．．．．．
;; ７．．．．．．．．
