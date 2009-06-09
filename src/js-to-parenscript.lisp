(in-package :js-to-parenscript)

(defun js-intern (js-literal-string)
  "interns a camel-cased js string to an appropriate lispy symbol"
  (intern
   (string-upcase
    (as-lisp-style-symbol-string
     js-literal-string))))

(defun as-lisp-style-symbol-string (js-style-symbol-string)
  "converts camelCasedStrings to not-so camel-cased-strings"
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "[A-Z]")
   js-style-symbol-string ;"AnAniMal"
   #'(lambda (match &rest ignored)
       (declare (ignore ignored))
       (format nil "-~A" (string-downcase match)))
   :simple-calls t))

(defgeneric as-paren (js-elem)
  (:documentation "converts a javascript element to a parenscript element.
Input is an abstract javascript form and output is a parenscript form."))

(defmethod as-paren ((expr expression))
  (print "an expression!")
  expr)

(defmethod as-paren ((lengthy-decl var-decl-statement))
  "Converts a var declaration statement like var x = 3, y = 34; to a
series of (defvar x 3) (defvar y 34) forms."
  (let ((statements (var-decl-statement-var-decls  lengthy-decl)))
    (if (eql 1 (length statements))
	(as-paren (first statements))
	`(progn
	  ,@(mapcar #'as-paren statements)))))

(defmethod as-paren ((decl var-decl))
  (let ((name (js-intern
	       (var-decl-name decl)))
	(value (var-decl-initializer decl)))
    (if (null value)
	`(defvar ,name)
	`(defvar ,name ,(as-paren value) ))))

(defmethod as-paren ((js-form numeric-literal))
  (let ((number (numeric-literal-value js-form)))
    number))

(defmethod as-paren ((js-form binary-operator))
  (let ((left-arg (binary-operator-left-arg js-form))
	(right-arg (binary-operator-right-arg js-form))
	(op (token-to-paren (binary-operator-op-symbol js-form))))
    `(,op
      ,(as-paren left-arg)
      ,(as-paren right-arg))))

(defun token-to-paren (tok)
  (js-intern
   (or
    (gethash tok *symbols-to-tokens*)
    (string-downcase (string tok)))))

(defmethod as-paren ((js-form unary-operator))
  (let ((arg (unary-operator-arg js-form))
	(op (token-to-paren (unary-operator-op-symbol js-form))))
    `(,op
      ,(as-paren arg) )))

(defmethod as-paren ((js-form fn-call))
  (let ((fn-identifier (fn-call-fn js-form))
	(fn-args (fn-call-args js-form)))
    `(,(as-paren fn-identifier)
      ,@(mapcar #'as-paren fn-args))))

(defmethod as-paren ((js-form identifier))
  (let ((name (identifier-name js-form)))
    (js-intern name)))

(defmethod as-paren ((js-form special-value))
  (let ((keyword-sym (special-value-symbol js-form)))
    (token-to-paren keyword-sym)))

(defmethod as-paren ((js-form string-literal))
  (let ((value (string-literal-value js-form)))
    value))

(defmethod as-paren ((js-form array-literal))
  (let ((elements (array-literal-elements js-form)))
    `(array ,@(mapcar #'as-paren elements))))

(defmethod as-paren ((js-form object-literal))
  (let ((properties (object-literal-properties js-form)))
    `(create ,@(mapcan
		#'(lambda (entry)
		    `(,(as-paren (car entry))
		      ,(as-paren (cdr entry))))
		properties))))

(defmethod as-paren ((js-form new-expr))
  (let ((constructor (new-expr-constructor js-form))
	(args (new-expr-args js-form)))
    (if (null args)
	`(new ,(as-paren constructor))
	`(new (,(as-paren constructor)
	       ,@(mapcar #'as-paren args))))))


(defmethod as-paren ((js-form property-access))
  (let ((target (property-access-target js-form))
	(field (property-access-field js-form)))
    `(slot-value ,(as-paren target) ,(as-paren field) )))

(defmethod as-paren ((js-form conditional))
  (let ((condition (conditional-condition js-form))
	(true-arg (conditional-true-arg js-form))
	(false-arg (conditional-false-arg js-form)))
    `(if ,(as-paren condition)
      ,(as-paren true-arg)
      ,(as-paren false-arg) )))

(defmethod as-paren ((js-form if-statement))
  (let ((condition (if-statement-condition js-form))
	(then-arg (if-statement-then-statement js-form))
	(else-arg (if-statement-else-statement js-form)))
    (if (not else-arg)
	`(if ,(as-paren condition) ,(as-paren then-arg))
	`(if ,(as-paren condition) ,(as-paren then-arg) ,(as-paren else-arg)))))

(defmethod as-paren ((js-form do-statement))
  (let ((condition (do-statement-condition js-form))
	(body (do-statement-body js-form)))
    `(while t
      ,(as-paren body)
      (if ,(as-paren condition)
	  (continue) (break)))))

(defmethod as-paren ((js-form while))
  (let ((condition (while-condition js-form))
	(body (while-body js-form)))
    `(while ,(as-paren condition) ,(as-paren body))))

(defmethod as-paren ((js-form for-in))
  (unless (var-decl-statement-p (for-in-binding js-form))
    (error "PS -> JS translator does not accept for (x in obj) forms, only for (var x in obj)." js-form))

  `(ps:for-in (,(js-intern
		 (var-decl-name (first (var-decl-statement-var-decls (for-in-binding js-form)))))
		,(as-paren (for-in-collection js-form)))
	      ,(as-paren (for-in-body js-form))))

(defmethod as-paren ((js-form for))
  (let ((initializer (for-initializer js-form))
	(condition (for-condition js-form))
	(step (for-step js-form))
	(body (for-body js-form)) )
    `(while t
      ,(as-paren initializer)
      (if (not ,(as-paren condition))
	  (break))
      ,(as-paren step) ,(as-paren body))))


(defmethod as-paren ((js-form comma-expr))
  (let ((expressions (comma-expr-exprs js-form)))
    `(progn ,@(mapcar #'as-paren expressions))))

(defmethod as-paren ((js-form statement-block))
  (let ((statements (statement-block-statements js-form)))
    `(progn ,@(mapcar #'as-paren statements))))

(defmethod as-paren ((js-form function-expression))
  (let ((name (function-expression-name js-form))
	(params (function-expression-parameters js-form))
	(body (function-expression-body js-form)))
    `(,(if (null name) 'lambda 'defun)
      ,(mapcar #'js-intern params)
      ,@(mapcar #'as-paren body))))

(defmethod as-paren ((js-form function-decl))
  (with-accessors ((name function-decl-name)
                   (params function-decl-parameters)
                   (body function-decl-body))
                  js-form
                  `(defun ,(js-intern name) ,(mapcar #'js-intern params)
                     ,@(mapcar #'as-paren body))))

(defmethod as-paren ((js-form return-statement))
  `(return ,(as-paren (return-statement-arg js-form))))

(defun js-to-paren (js-text)
  `(progn
     ,@(mapcar #'as-paren (parse js-text))))

(defparameter *example-script* "/** comments are left out **/ function getElementPosition(elemID) {
    /** comments are left out **/
    var offsetTrail = document.getElementById(elemID);
    var offsetLeft = 0;
    var offsetTop = 0;
    while (offsetTrail) {
        offsetLeft += offsetTrail.offsetLeft;
        offsetTop += offsetTrail.offsetTop;
        offsetTrail = offsetTrail.offsetParent;
    }
    if (navigator.userAgent.indexOf(\"Mac\") != -1 &&
        typeof document.body.leftMargin != \"undefined\") {
        offsetLeft += document.body.leftMargin;
        offsetTop += document.body.topMargin;
    }
    return {left:offsetLeft, top:offsetTop};
}")

(defun slurp-stream4 (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun js-file-to-paren (filespec)
  (with-open-file (stream filespec :direction :input)
    (js-to-paren (slurp-stream4 stream))))

(defun example-usage ()
  (js-to-paren *example-script*))