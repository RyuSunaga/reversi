;; 目的：以下の３つに慣れる
;;; 1. lisp
;;; 2. CLOS
;;; 3. マクロ


;; Board Class
;; Board情報/書き換え/出力/チェック
(defclass board ()
  ((grid :initarg :grid
   	 :initform (initialise-grid)
	 :accessor grid
	 :documentation "Grid Infomation")))

(defun initialise-grid ()
  ;; initialize grid status.
  (let ((grid (make-array '(8 8) :initial-element #\.)))
    (setf (aref grid 3 3) #\o)
    (setf (aref grid 4 4) #\o)    
    (setf (aref grid 3 4) #\x)
    (setf (aref grid 4 3) #\x)
    grid))

(defgeneric display-grid (board)
  (:documentation "Display board info."))

(defmethod display-grid ((board board))
  (dotimes (i 8)
    (dotimes (j 8)
      (format t "~a" (aref (grid board) i j)))
    (format t "~%")))

;; Player Class
;; Player情報/Action


;; Human Class -> Player Class


;; CPU Class -> Player Class


;; User Interface

;; main
;;; game-start
;;;; print
;;;; put
;;;; update -> end if game close
;;;; player change








