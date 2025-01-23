(in-package #:lox.error)

(define-condition analysis-error (error)
  ((line :initarg :line :reader line)
   (message :initarg :message :reader message))
  (:default-initargs
   :line (a:required-argument :line)
   :message (a:required-argument :message))
  (:report report-analysis-error))

(defun report-analysis-error (condition stream)
  (let ((line (line condition))
        (message (message condition)))
    (format stream "~&[line: ~a] Error: ~a" line message)))

(defgeneric report-error (location message))

(defmethod report-error ((location token) (message string))
  (report (line location) message))

(defmethod report-error ((location integer) (message string))
  (report location message))

(defun report (line message)
  (format *error-output* "~&[line: ~a] Error: ~a" line message)
  (signal 'analysis-error :line line :message message))

(define-condition runtime-error (error)
  ((token :initarg :token :reader token)
   (message :initarg :message :reader message))
  (:default-initargs
   :token (a:required-argument :token)
   :message (a:required-argument :message))
  (:report report-runtime-error))

(defun report-runtime-error (condition stream)
  (let ((token (token condition))
        (message (message condition)))
    (format stream "~&[line: ~a] Error: ~a" (line token) message)))

(defun runtime-error (token message)
  (error 'runtime-error :token token :message message))
