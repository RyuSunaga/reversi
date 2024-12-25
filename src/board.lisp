(defpackage :board
  (:use :cl :constants)
  (:export
   :board))

(in-package :board)

;;;; ---- Constant ----

;; mini-reversiとして6x6にする
(defconstant +row-min-inclusive+ 0)
(defconstant +row-max-inclusive+ 5) 
(defconstant +column-min-inclusive+ 0)
(defconstant +column-max-inclusive+ 5)

(defvar *zenkaku-numbers* '("０" "１" "２" "３" "４" "５" "６" "７" "８"))

;;; utilitis
(defun initialize-grid ()
  ;; initialize grid status.
  (let ((grid (make-array '(8 8) :initial-element +space+)))
    (setf (aref grid 2 2) +black+)
    (setf (aref grid 3 3) +black+)    
    (setf (aref grid 2 3) +white+)
    (setf (aref grid 3 3) +white+)
    grid))


;;;; ---- Class Definition ----
(defclass board ()
  ((grid :initarg :grid
   	 :initform (initialize-grid)
	 :accessor grid
	 :documentation "Grid Infomation")))

;;;; ---- Functions ----


(defun get-other-stone (stone)
  (cond
    ((char= stone +white+) +black+)
    ((char= stone +black+) +white+)
    (t nil)))

;;;; ---- Generics ----
(defgeneric display-grid (board)
  (:documentation "Display board info."))

(defgeneric put-stone (r c stone board)
  (:documentation "Updage grid stone."))

(defgeneric can-put-stone (r c stone board)
  (:documentation "Check the point can put sotne."))

(defgeneric reverse-stones (r c stone board)
  (:documentation "Reverse stones."))

;;;; ---- Methods ----

(defmethod display-grid ((board board))
  (format t "~a" " ")
  (dotimes (i (1+ +row-max-inclusive+)) (format t "~a" (elt *zenkaku-numbers* i)))
  (format t "~%")
	  
  (dotimes (i (1+ +row-max-inclusive+))
    (format t "~a" (elt *zenkaku-numbers* i))
    (dotimes (j (1+ +column-max-inclusive+))
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
	     ;;NOTE: この条件式は色々なところで使うから関数可したほうがいいかも？（例えば石の反転処理などで使うかも）
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

;; TODO: 重要関数
(defmethod reverse-stones (r c stone (board board))
  ;; 可能なら全方向で石を入れ替える
  ;;; step1: 各方向で入れ替えが可能かを確認
  ;;; step2: 不可能な場合：次の方向へ
  ;;; step3: 可能な場合：自分と同じ石が出るまで反転し続ける

  ;; NOTE: check-specific-directionを使いまわしたい
)

;;;; ---- TEST ----
;; board
(defparameter *board*
  (make-instance 'board))



