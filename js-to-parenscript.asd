;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :js-to-parenscript-system
  (:use :cl :asdf)
  (:export
   #:*version*))

(in-package :js-to-parenscript-system)

;;;; ======= Build parameters ======================================================================

(defparameter *version* "alpha"
  "The current version")

;;;; ======= Compilation configuration =============================================================
(defparameter *muffle-conflicts* nil
  "When T, yacc warnings about Shift/Reduce and Reduce/Reduce conflicts will be muffled.
   When NIL, all such conflicts will be reported.
   When non-NIL, non-T, a single summary warning will be reported when conflicts exist.

   This value should be set to NIL or non-T during grammar
   development/debugging (so that we find out about the conflicts), but T
   at all other times (so that SBCL won't drop into the debugger when
   we're trying to load parse-javascript.lisp).")

;;;; ======= System definition =====================================================================
(asdf:defsystem js-to-parenscript
  :version *version*
  :author "Red Daly"
  :licence "MIT License <http://www.opensource.org/licenses/mit-license.php>"
  :serial t
  :components ((:module
		"src"
		:components
		((:file "package")
		 (:file "js-to-parenscript" :depends-on ("package")))))
;		 #+(or sbcl lispworks) (:file "main"))
  :depends-on (js-parser))