(in-package :cl-prelude)

(deftype either-t (&optional a b)
  (declare (ignore a b))
  'either)

(defdata either
  (right t)
  (left t))

(declaim (inline either/cata))
(defun either/cata (f g obj)
  (match either obj
    ((right b) (funcall f b))
    ((left a) (funcall g a))))

(declaim (inline either/bind))
(defun either/bind (f obj)
  (match either obj
    ((right v) (funcall f v))
    ((left a) (left a))))

(declaim (inline either/fmap))
(defun either/fmap (f obj)
  (match either obj
    ((right b) (right (funcall f b)))
    ((left a) (left a))))

(declaim (inline either/bimap))
(defun either/bimap (f g obj)
  (match either obj
    ((right v) (funcall f v))
    ((left a) (funcall g a))))

(defmethod >>= (f (obj either))
  (either/bind f obj))

(defmethod -> (f (obj either))
  (either/fmap f obj))

(defmethod bimap (f g (obj either))
  (either/bimap f g obj))
