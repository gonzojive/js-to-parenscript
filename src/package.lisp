;;;; package.lisp
;;;
;;; Define the packages used by the js-on-cl system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :js-to-parenscript
  (:use :js-parser :common-lisp)
  (:nicknames :js-to-ps)
  (:export
   #:parse
   #:process
   #:syntax-error))
