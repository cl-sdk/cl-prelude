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
   #:ok?))

(in-package :cl-prelude)

;; ordering

(defdata ordering
  ord-lt
  ord-eq
  ord-gt)

;; maybe

(defdata maybe
  (just t)
  nothing)

(deftype maybe-t (a)
  (declare (ignore a))
  'result)

(defun maybe/cata (f g obj)
  (match maybe obj
    ((just a) (funcall f a))
    (nothing (funcall g))))

(defmacro lazy-maybe-case (b f obj)
  `(progn
     (match maybe ,obj
       ((just a) (funcall ,f a))
       (nothing (funcall (lambda () ,b))))))

(defun maybe/bind (f obj)
  (match maybe obj
    ((just a) (funcall f a))
    (nothing nothing)))

(defun maybe/fmap (f obj)
  (match maybe obj
    ((just a) (just (funcall f a)))
    (nothing nothing)))

;; either

(deftype either-t (&optional a b)
  (declare (ignore a b))
  'either)

(defdata either
  (right t)
  (left t))

(defun either/cata (f g obj)
  (match either obj
    ((right b) (funcall f b))
    ((left a) (funcall g a))))

(defun either/bind (f obj)
  (match either obj
    ((right v) (funcall f v))
    ((left a) (left a))))

(defun either/fmap (f obj)
  (match either obj
    ((right b) (right (funcall f b)))
    ((left a) (left a))))

(defun either/bimap (f g obj)
  (match either obj
    ((right v) (funcall f v))
    ((left a) (funcall g a))))

;; result

(defdata result
  (ok t)
  (fail t))

(deftype result-t (&optional a b)
  (declare (ignore a b))
  'result)

(defun ok? (obj)
  (subtypep (type-of obj) 'ok))

(defun fail? (obj)
  (subtypep (type-of obj) 'fail))

(defun result/cata (f g obj)
  (match result obj
    ((ok b) (funcall f b))
    ((fail a) (funcall g a))))

(defun result/fmap (f obj)
  (match result obj
    ((ok b) (ok (funcall f b)))
    ((fail a) (fail a))))

(defun result/bind (f obj)
  (match result obj
    ((ok v) (funcall f v))
    ((fail a) (fail a))))

(defun result/bimap (f g obj)
  (match result obj
    ((ok v) (funcall f v))
    ((fail a) (funcall g a))))

;; validation

(defdata validation
  (success t)
  (failure t))

(deftype validation-t (&optional a b)
  (declare (ignore a b))
  'validation)

(defun validation/cata (f g obj)
  (match validation obj
    ((success b) (funcall f b))
    ((failure a) (funcall g a))))

(defun validation/fmap (f obj)
  (match validation obj
    ((success b) (success (funcall f b)))
    ((failure a) (failure a))))

(defun validation/bind (f obj)
  (match validation obj
    ((success v) (funcall f v))
    ((failure a) (failure a))))

(defun validation/bimap (f g obj)
  (match validation obj
    ((success v) (funcall f v))
    ((failure a) (funcall g a))))

(defgeneric >>= (f obj))
(defgeneric -> (f obj))
(defgeneric bimap (f g obj))

(defmethod >>= (f (obj maybe))
  (maybe/bind f obj))

(defmethod >>= (f (obj either))
  (either/bind f obj))

(defmethod >>= (f (obj result))
  (result/bind f obj))

(defmethod >>= (f (obj validation))
  (validation/bind f obj))

(defmethod -> (f (obj maybe))
  (maybe/fmap f obj))

(defmethod -> (f (obj either))
  (either/fmap f obj))

(defmethod -> (f (obj result))
  (result/fmap f obj))

(defmethod -> (f (obj validation))
  (validation/fmap f obj))

(defmethod bimap (f g (obj either))
  (either/bimap f g obj))

(defmethod bimap (f g (obj result))
  (result/bimap f g obj))

(defmethod bimap (f g (obj validation))
  (validation/bimap f g obj))

(defmacro do-notation (expressions &body body)
  "Given a BIND-FUNCTION and all the bind expressions, evaluate BODY."
  (let ((bind-function '>>=))
    `(progn
       ,@(reduce (lambda (acc do-binding)
		   (let ((var-name (car do-binding)))
		     (append `((,bind-function (lambda (,var-name)
						 ,(when (string-equal "_" (symbol-name var-name))
						    `(declare (ignorable ,var-name)))
						 ,@acc)
					       ,@(cdr do-binding))))))
		 (reverse expressions)
		 :initial-value body))))
