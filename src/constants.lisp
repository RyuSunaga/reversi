(defpackage :constants
  (:use :cl)
  (:export :+black+ :+white+ :+space+))

(in-package :constants)

;;;; ---- Constant Values for some files ----
(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")

;;; BUG: #\.が半角のために表示の際にずれが発生する。しかし全角に直すとエラーが発生する
(defconstant +space+ #\． "Symbol for white stone.")
