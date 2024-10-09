(in-package :cl-prelude)

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

(declaim (inline result/cata))
(defun result/cata (f g obj)
  (match result obj
    ((ok b) (funcall f b))
    ((fail a) (funcall g a))))

(declaim (inline result/fmap))
(defun result/fmap (f obj)
  (match result obj
    ((ok b) (ok (funcall f b)))
    ((fail a) (fail a))))

(declaim (inline result/bind))
(defun result/bind (f obj)
  (match result obj
    ((ok v) (funcall f v))
    ((fail a) (fail a))))

(declaim (inline result/bimap))
(defun result/bimap (f g obj)
  (match result obj
    ((ok v) (funcall f v))
    ((fail a) (funcall g a))))

(defmethod >>= (f (obj result))
  (result/bind f obj))

(defmethod -> (f (obj result))
  (result/fmap f obj))

(defmethod bimap (f g (obj result))
  (result/bimap f g obj))

(defmethod =< (f (obj result))
  (if (ok? obj)
      obj
      (funcall f obj)))
