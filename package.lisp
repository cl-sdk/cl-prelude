(defpackage #:cl-prelude
  (:use #:cl #:cl-algebraic-data-type)
  (:export
   #:do-notation
   #:maybe
   #:just
   #:nothing
   #:maybe-case
   #:lazy-maybe-case
   #:maybe/bind
   #:maybe/fmap
   #:either
   #:right
   #:left
   #:either-case
   #:either/bind
   #:either/fmap
   #:ordering
   #:ord-lt
   #:ord-eq
   #:ord-gt
   #:result
   #:ok
   #:fail
   #:bind-result
   #:result-case
   #:>>=
   #:->
   #:result/bimap
   #:result/bind
   #:maybe/cata
   #:either/cata
   #:result/cata
   #:pipeline
   #:validation
   #:success
   #:failure
   #:validation/bind
   #:validation/cata
   #:validation/bimap
   #:bimap
   #:maybe-t
   #:either-t
   #:result-t
   #:validation-t
   #:fail?
   #:ok?
   #:lazy-from-maybe
   #:from-maybe
   #:result/fmap
   #:validation/fmap
   #:=<<
   #:=<))

(in-package :cl-prelude)

(defgeneric >>= (f obj)
  (:documentation "Monadic bind."))

(defgeneric -> (f obj)
  (:documentation "Functor map."))

(defgeneric =< (f obj)
  (:documentation "If a ADT has a concept of \"negative\",
 this method can gives an opportunity to convert into a  \"positive\" case."))

(defgeneric bimap (f g obj)
  (:documentation "Bimap"))

(defmacro do-notation (expressions &body body)
  "Given a BIND-FUNCTION and all the bind expressions, evaluate BODY."
  `(progn
     ,@(reduce (lambda (acc do-binding)
		 (let ((var-name (car do-binding)))
		   (append `((>>= (lambda (,var-name)
				    ,(when (string-equal "_" (symbol-name var-name))
				       `(declare (ignorable ,var-name)))
				    ,@acc)
				  ,@(cdr do-binding))))))
	       (reverse expressions)
	       :initial-value body)))
