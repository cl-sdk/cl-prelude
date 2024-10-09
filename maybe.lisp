(in-package :cl-prelude)

(defdata maybe
  (just t)
  nothing)

(deftype maybe-t (a)
  (declare (ignore a))
  'result)

(declaim (inline maybe/cata))
(defun maybe/cata (f g obj)
  (match maybe obj
    ((just a) (funcall f a))
    (nothing (funcall g))))

(defmacro lazy-maybe-case (b f obj)
  `(progn
     (match maybe ,obj
       ((just a) (funcall ,f a))
       (nothing (funcall (lambda () ,b))))))

(declaim (inline maybe/bind))
(defun maybe/bind (f obj)
  (match maybe obj
    ((just a) (funcall f a))
    (nothing nothing)))

(declaim (inline maybe/fmap))
(defun maybe/fmap (f obj)
  (match maybe obj
    ((just a) (just (funcall f a)))
    (nothing nothing)))

(defun from-maybe (n v)
  (match maybe v
    ((just a) a)
    (nothing n)))

(defun lazy-from-maybe (n v)
  (match maybe v
    ((just a) a)
    (nothing (funcall n))))

(defmethod -> (f (obj maybe))
  (maybe/fmap f obj))

(defmethod >>= (f (obj maybe))
  (maybe/bind f obj))
