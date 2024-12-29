(defpackage :constants
  (:use :cl)
  (:export
   :+black+
   :+white+
   :+space+
   :+row-min-inclusive+
   :+row-max-inclusive+
   :+column-min-inclusive+
   :+column-max-inclusive+
   :*zenkaku-numbers*
   :*hankaku-numbers*))

(in-package :constants)

;;;; ---- Constant Values for some files ----
(defconstant +black+ #\○ "Symbol for black stone.")
(defconstant +white+ #\● "Symbol for white stone.")
(defconstant +space+ #\. "Symbol for white stone.")


;;;; ---- Constant for boards ----
(defconstant +row-min-inclusive+ 0)
(defconstant +row-max-inclusive+ 5) 
(defconstant +column-min-inclusive+ 0)
(defconstant +column-max-inclusive+ 5)

(defvar *zenkaku-numbers* '("０" "１" "２" "３" "４" "５" "６" "７" "８"))
(defvar *hankaku-numbers* '(0 1 2 3 4 5))

