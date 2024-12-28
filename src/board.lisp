(defpackage :board
  (:use :cl :constants)
  (:export
   :board
   :put-stone
   :display-grid
   :can-put-stone))

(in-package :board)

;;;; ---- Constant ----

;; mini-reversiとして6x6にする
(defconstant +row-min-inclusive+ 0)
(defconstant +row-max-inclusive+ 5) 
(defconstant +column-min-inclusive+ 0)
(defconstant +column-max-inclusive+ 5)

(defvar *zenkaku-numbers* '("０" "１" "２" "３" "４" "５" "６" "７" "８"))
(defvar *hankaku-numbers* '(0 1 2 3 4 5))

;;; utilitis
(defun initialize-grid ()
  ;; initialize grid status.
  (let ((grid (make-array '(6 6) :initial-element +space+)))
    (setf (aref grid 2 2) +black+)
    (setf (aref grid 3 3) +black+)    
    (setf (aref grid 2 3) +white+)
    (setf (aref grid 3 2) +white+)
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

(defgeneric get-stone (r c board)
  (:documentation "Get grid info."))

(defgeneric flip-stone (r c stone board)
  (:documentation "flip grid stone."))

(defgeneric %check-specific-direction (sr sc dr dc stone-type board)
  (:documentation "Check stone can put specific direction"))

(defgeneric %check-all-direction (sr sc stone-type board)
  (:documentation "Check stone can put all direction"))

(defgeneric can-put-stone (r c stone board)
  (:documentation "Check the point can put sotne."))

(defgeneric reverse-stones (sr sc stone-type board)
  (:documentation "Reverse stones."))

(defgeneric %reverse-stones-specific-direction (sr sc dr dc stone-type board)
  (:documentation "Reverse stones for specific direction."))

;;;; ---- Methods ----

(defmethod display-grid ((board board))
  (format t "~a" " ")
  (dotimes (i (1+ +row-max-inclusive+)) (format t "~a" (elt *zenkaku-numbers* i)))
  (format t "~%")
	  
  (dotimes (i (1+ +row-max-inclusive+))
    ;; (format t "~a" (elt *zenkaku-numbers* i))
    (format t "~a" (elt *zenkaku-numbers* i))    
    (dotimes (j (1+ +column-max-inclusive+))
      (format t "~a" (aref (grid board) i j)))
    (format t "~%")))

(defmethod put-stone (r c stone-type (board board))
  ;; Put stone grid (r c)
  (setf (aref (grid board) r c) stone-type)
  (reverse-stones r c stone-type board))

(defmethod get-stone (r c (board board))
  ;; Put stone grid (r c)
  (aref (grid board) r c))

(defmethod flip-stone (r c stone (board board))
  ;; Put stone grid (r c)
  (setf (aref (grid board) r c) (get-other-stone (get-stone r c board))))

(defun position-in-grid-p (r c)
  (and ;; next stone position include in grid.
   (<= +row-min-inclusive+ r)
   (<= r +row-max-inclusive+)
   (<= +column-min-inclusive+ c)
   (<= c +column-max-inclusive+)))

(defmethod %check-specific-direction-can-put-stone (sr sc dr dc put-stone-type (board board))
  (let ((comb nil) (stone put-stone-type) (sr sr) (sc sc)) ;; s: start, d: direction
    (loop
      while
      (position-in-grid-p (+ sr dr) (+ sc dc))
      do
	 ;;(setf sr (+ sr dr)) ;; update row
	 ;;(setf sc (+ sc dc)) ;; update column
	 (incf sr dr)
	 (incf sc dc)
	 (setf stone (aref (grid board) sr sc))	

	 (if (char= stone put-stone-type) ;; 同じ石か
	     (if comb			  ;;1度でも連なったか
		 (return-from %check-specific-direction-can-put-stone t) 
		 (return-from %check-specific-direction-can-put-stone nil))
	     (if (char= stone +space+) ;; スペース or 違う石
		 (return-from %check-specific-direction-can-put-stone nil)
		 (setf comb t)))))
  (return-from %check-specific-direction-can-put-stone nil))

;; 8方向全てを確認してtrueの場合にある処理をする。というのはリバーシにおいて何回も出てくる処理である。これをマクロ化する
(defmacro check-each-direction (sr sc stone-type board forms)
  ;; sr: row, sc: column, form: form when the check is true
  `(loop for dr from -1 to 1
	 do (loop for dc from -1 to 1
		  do (when (%check-specific-direction ,sr ,sc dr dc ,stone-type ,board)
		       ,@forms))))

;;(check-each-direction 0 0 #\○ *board* (return-from %check-all-direction-can-put-stone t))

(defmethod %check-all-direction-can-put-stone (sr sc stone-type (board board))
  (check-each-direction sr sc stone-type board
			((return-from %check-all-direction-can-put-stone t)))
  nil)

(defmethod can-put-stone (r c put-stone-type (board board))
  ;; 1. 対象の箇所が空いていなければその時点でnil
  (when (char/= (aref (grid board) r c) +space+)
    (format t "THIS PLACE IS ALREADY EXISTS.~%")
    (return-from can-put-stone nil))

  ;; 2. 相手の石を挟むことができる
  (when (eql (%check-all-direction-can-put-stone r c put-stone-type board) nil)
    (format t "THIS PLACE CAN'T REVERSE OTHER STONES.~%")
    (return-from can-put-stone nil))

  (return-from can-put-stone t))


(defmethod %reverse-stones-specific-direction (sr sc dr dc stone-type (board board))
  ;; dr, dc方向に相手の石があり続ける限りflipする
  ;;  TODO: 特定方向へ進める処理をマクロまたは関数にできないか
  (let ((nr (+ sr dr)) (nc (+ sc dc)))
    (loop while (and
		 (position-in-grid-p nr nc);; 次置く石がおける位置にある
		 (char= (get-stone nr nc board) (get-other-stone stone-type));; 次置く石が相手の石の色である
		 )
	  do
	     (flip-stone nr nc stone-type board)
	     (incf nr dr)
	     (incf nc dc))))

(defmethod reverse-stones (sr sc stone-type (board board))
  ;; 可能なら全方向で石を入れ替える
;;; step1: 各方向で入れ替えが可能かを確認
;;; step2: 不可能な場合：次の方向へ
;;; step3: 可能な場合：自分と同じ石が出るまで反転し続ける

  ;; NOTE: 8方向チェックの場合に困るのはformsでdr,dcを引数とするformの使い方が分からないこと。→letを使って対象スコープでの変数を書き換えて仕舞えばよい
  (check-each-direction sr sc stone-type board
    ((%reverse-stones-specific-direction sr sc dr dc stone-type board)))
  )

;;;; ---- TEST ----
;; board
(defparameter *board*
  (make-instance 'board))
