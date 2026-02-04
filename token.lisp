(in-package #:lox.token)

(deftype token-type ()
  '(member
    ;; Single character tokens.
    :left-paren :right-paren :left-brace :right-brace
    :comma :dot :minus :plus :semicolon :slash :star

    ;; One or two character tokens.
    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal
    :question-mark :colon

    ;; Literals.
    :identifier :string :number

    ;; Keywords.
    :and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while

    :eof))

(defclass token ()
  ((token-type :initarg :token-type :reader token-type)
   (lexeme :initarg :lexeme :reader lexeme)
   (line :initarg :line :reader line)
   (literal :initarg :literal :reader literal))
  (:default-initargs
   :token-type (a:required-argument :token-type)
   :lexeme (a:required-argument :lexeme)
   :line (a:required-argument :line)
   :literal nil))

(defun make-token (token-type lexeme line &optional literal)
  (check-type token-type token-type)
  (make-instance 'token
                 :token-type token-type
                 :lexeme lexeme
                 :line line
                 :literal literal))

(defun token-p (object)
  (typep object 'token))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a ~a" (token-type object) (lexeme object) (literal object))))
