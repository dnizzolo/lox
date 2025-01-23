(in-package #:lox.resolver)

(defclass resolver ()
  ((interpreter :initarg :interpreter :reader interpreter)
   (scopes :initform nil :accessor scopes)
   (current-function :initform :none :accessor current-function)
   (current-class :initform :none :accessor current-class)))

(defun make-resolver (interpreter)
  (make-instance 'resolver :interpreter interpreter))

(defun resolve (interpreter statements)
  (let ((resolver (make-resolver interpreter)))
    (resolve-statements resolver statements)
    (interpreter resolver)))

(defgeneric resolve-statement (resolver statement))
(defgeneric resolve-expression (resolver expression))

(defun resolve-statements (resolver statements)
  (map nil (lambda (statement) (resolve-statement resolver statement)) statements))

(defun begin-scope (resolver)
  (push (make-hash-table :test #'equal) (scopes resolver)))

(defun end-scope (resolver)
  (pop (scopes resolver)))

(defun %declare (resolver name)
  (when (scopes resolver)
    (setf (gethash (lexeme name) (first (scopes resolver))) nil)))

(defun %define (resolver name)
  (when (scopes resolver)
    (setf (gethash (lexeme name) (first (scopes resolver))) t)))

(defun resolve-local (resolver expression name)
  (loop with name = (lexeme name)
        for scope in (scopes resolver)
        for distance from 0
        do (when (gethash name scope)
             (lox.interpreter:resolve-depth (interpreter resolver) expression distance)
             (return))))

(defmethod resolve-statement ((resolver resolver) (statement ast:block))
  (begin-scope resolver)
  (resolve-statements resolver (ast:statements statement))
  (end-scope resolver))

(defmethod resolve-statement ((resolver resolver) (statement ast:var))
  (%declare resolver (ast:name statement))
  (when (ast:initializer statement)
    (resolve-expression resolver (ast:initializer statement)))
  (%define resolver (ast:name statement)))

(defmethod resolve-statement ((resolver resolver) (statement ast:expression))
  (resolve-expression resolver (ast:expression statement)))

(defmethod resolve-statement ((resolver resolver) (statement ast:if))
  (resolve-expression resolver (ast:condition statement))
  (resolve-statement resolver (ast:then-branch statement))
  (when (ast:else-branch statement)
    (resolve-statement resolver (ast:else-branch statement))))

(defmethod resolve-statement ((resolver resolver) (statement ast:print))
  (resolve-expression resolver (ast:expression statement)))

(defmethod resolve-statement ((resolver resolver) (statement ast:return))
  (when (eql (current-function resolver) :none)
    (report-error (ast:token statement) "Can't return from top-level code"))
  (when (ast:value statement)
    (when (eql (current-function resolver) :initializer)
      (report-error (ast:token statement) "Can't return a value from an initializer"))
    (resolve-expression resolver (ast:value statement))))

(defmethod resolve-statement ((resolver resolver) (statement ast:while))
  (resolve-expression resolver (ast:condition statement))
  (resolve-statement resolver (ast:body statement)))

(defmethod resolve-statement ((resolver resolver) (statement ast:function))
  (%declare resolver (ast:name statement))
  (%define resolver (ast:name statement))
  (resolve-function resolver statement :function))

(defun resolve-function (resolver function type)
  (let ((previous-function (current-function resolver)))
    (setf (current-function resolver) type)
    (begin-scope resolver)
    (loop for param across (ast:params function) do
      (%declare resolver param)
      (%define resolver param))
    (resolve-statements resolver (ast:body function))
    (end-scope resolver)
    (setf (current-function resolver) previous-function)))

(defmethod resolve-statement ((resolver resolver) (statement ast:class))
  (let ((enclosing-class (current-class resolver)))
    (setf (current-class resolver) :class)
    (%declare resolver (ast:name statement))
    (%define resolver (ast:name statement))
    (when (and (ast:superclass statement)
               (string= (lexeme (ast:name (ast:superclass statement)))
                        (lexeme (ast:name statement))))
      (report-error (ast:name (ast:superclass statement))
                    "A class can't inherit from itself"))
    (when (ast:superclass statement)
      (setf (current-class resolver) :subclass)
      (resolve-expression resolver (ast:superclass statement))
      (begin-scope resolver)
      (setf (gethash "super" (first (scopes resolver))) t))
    (begin-scope resolver)
    (setf (gethash "this" (first (scopes resolver))) t)
    (map nil
         (lambda (method)
           (resolve-function
            resolver
            method
            (if (string= (lexeme (ast:name method)) "init")
                :initializer
                :method)))
         (ast:methods statement))
    (end-scope resolver)
    (when (ast:superclass statement) (end-scope resolver))
    (setf (current-class resolver) enclosing-class)))

(defmethod resolve-expression ((resolver resolver) (expression ast:assign))
  (resolve-expression resolver (ast:value expression))
  (resolve-local resolver expression (ast:name expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:binary))
  (resolve-expression resolver (ast:left expression))
  (resolve-expression resolver (ast:right expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:ternary))
  (resolve-expression resolver (ast:condition expression))
  (resolve-expression resolver (ast:consequent expression))
  (resolve-expression resolver (ast:alternative expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:call))
  (resolve-expression resolver (ast:callee expression))
  (map nil (lambda (arg) (resolve-expression resolver arg)) (ast:arguments expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:grouping))
  (resolve-expression resolver (ast:expression expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:literal)))

(defmethod resolve-expression ((resolver resolver) (expression ast:logical))
  (resolve-expression resolver (ast:left expression))
  (resolve-expression resolver (ast:right expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:unary))
  (resolve-expression resolver (ast:right expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:variable))
  (when (and (scopes resolver)
             (multiple-value-bind (value found-p)
                 (gethash (lexeme (ast:name expression)) (first (scopes resolver)))
               (and (null value) found-p)))
    (report-error (ast:name expression)
                  "Can't read local variable in its own initializer"))
  (resolve-local resolver expression (ast:name expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:super))
  (cond ((eql (current-class resolver) :none)
         (report-error (ast:token expression)
                       "Can't use 'super' outside of a class"))
        ((not (eql (current-class resolver) :subclass))
         (report-error (ast:token expression)
                       "Can't use 'super' in a class with no superclass")))
  (resolve-local resolver expression (ast:token expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:this))
  (if (eql (current-class resolver) :none)
      (report-error (ast:token expression)
                    "Can't use 'this' outside of a class")
      (resolve-local resolver expression (ast:token expression))))

(defmethod resolve-expression ((resolver resolver) (expression ast:get))
  (resolve-expression resolver (ast:object expression)))

(defmethod resolve-expression ((resolver resolver) (expression ast:set))
  (resolve-expression resolver (ast:object expression))
  (resolve-expression resolver (ast:value expression)))
