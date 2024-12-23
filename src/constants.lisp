(defpackage :constants
  (:use :cl)
  (:export :+black+ :+white+ :+space+))

(in-package :constants)

;;;; ---- Constant Values for some files ----
(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")
(defconstant +space+ #\． "Symbol for white stone.")
