(in-package :cl-prelude)

(defdata validation
  (success t)
  (failure t))

(deftype validation-t (&optional a b)
  (declare (ignore a b))
  'validation)

(declaim (inline validation/cata))
(defun validation/cata (f g obj)
  (match validation obj
    ((success b) (funcall f b))
    ((failure a) (funcall g a))))

(declaim (inline validation/fmap))
(defun validation/fmap (f obj)
  (match validation obj
    ((success b) (success (funcall f b)))
    ((failure a) (failure a))))

(declaim (inline validation/bind))
(defun validation/bind (f obj)
  (match validation obj
    ((success v) (funcall f v))
    ((failure a) (failure a))))

(declaim (inline validation/bimap))
(defun validation/bimap (f g obj)
  (match validation obj
    ((success v) (funcall f v))
    ((failure a) (funcall g a))))

(defmethod >>= (f (obj validation))
  (validation/bind f obj))

(defmethod -> (f (obj validation))
  (validation/fmap f obj))

(defmethod bimap (f g (obj validation))
  (validation/bimap f g obj))
