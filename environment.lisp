(in-package #:lox.environment)

(defclass environment ()
  ((vals :initform (make-hash-table :test #'equal) :accessor vals)
   (enclosing :initarg :enclosing :accessor enclosing))
  (:default-initargs
   :enclosing nil))

(defun make-environment (&optional enclosing)
  (make-instance 'environment :enclosing enclosing))

(defun get (environment token)
  (multiple-value-bind (val found-p) (gethash (lexeme token) (vals environment))
    (cond (found-p val)
          ((enclosing environment) (get (enclosing environment) token))
          (t (runtime-error token (format nil "Undefined variable '~a'"
                                          (lexeme token)))))))

(defun assign (environment token value)
  (multiple-value-bind (val found-p) (gethash (lexeme token) (vals environment))
    (declare (ignorable val))
    (cond (found-p (setf (gethash (lexeme token) (vals environment)) value))
          ((enclosing environment) (assign (enclosing environment) token value))
          (t (runtime-error token (format nil "Undefined variable '~a'"
                                          (lexeme token)))))))

(defun define (environment name value)
  (setf (gethash name (vals environment)) value))

(defun ancestor (environment distance)
  (dotimes (i distance environment)
    (setf environment (enclosing environment))))

(defun get-at (environment distance name)
  (gethash name (vals (ancestor environment distance))))

(defun assign-at (environment distance token value)
  (setf (gethash (lexeme token) (vals (ancestor environment distance))) value))
