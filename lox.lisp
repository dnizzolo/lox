(in-package #:lox)

(defvar *interpreter* (make-interpreter))

(defun run (source)
  (let (had-error-p)
    (handler-bind ((analysis-error (lambda (condition)
                                     (declare (ignorable condition))
                                     (setf had-error-p t))))
      (let* ((scanner (make-scanner source))
             (tokens (scan-tokens scanner))
             (parser (make-parser tokens))
             (statements (parse parser)))
        (unless had-error-p
          (let ((interpreter (resolve *interpreter* statements)))
            (unless had-error-p
              (interpret interpreter statements))))))))

(defun run-prompt ()
  (flet ((prompt-for-input ()
           (format t "~&> ")
           (finish-output)
           (read-line t nil)))
    (loop
      (let ((input (prompt-for-input)))
        (cond ((null input) (return))
              ((zerop (length input)))
              (t (run input)))))))

(defun run-file (pathname)
  (run (uiop:read-file-string pathname)))
