(in-package #:lox.interpreter)

(defvar *lox-native-functions* nil)

(defclass interpreter ()
  ((locals :initform (make-hash-table) :accessor locals)
   (globals :initform (env:make-environment) :accessor globals)
   (environment :accessor environment)))

(defmethod initialize-instance :after ((object interpreter) &key)
  (with-slots (globals environment) object
    (setf environment globals)
    (dolist (native-function *lox-native-functions*)
      (env:define environment (name native-function) native-function))))

(defun make-interpreter ()
  (make-instance 'interpreter))

(defun resolve-depth (interpreter expr depth)
  (setf (gethash expr (locals interpreter)) depth))

(defun interpret (interpreter statements)
  (map nil (lambda (statement) (execute interpreter statement)) statements))

(defgeneric execute (interpreter statement))

(defun execute-block (interpreter statements environment)
  (let ((previous (environment interpreter)))
    (unwind-protect
         (loop initially (setf (environment interpreter) environment)
               for statement across statements
               do (execute interpreter statement))
      (setf (environment interpreter) previous))))

(defmethod execute ((interpreter interpreter) (statement ast:block))
  (execute-block interpreter
                 (ast:statements statement)
                 (env:make-environment (environment interpreter))))

(defmethod execute ((interpreter interpreter) (statement ast:class))
  (with-accessors ((environment environment)) interpreter
    (let ((superclass
            (when (ast:superclass statement)
              (let ((superclass (evaluate interpreter (ast:superclass statement))))
                (unless (lox-class-p superclass)
                  (runtime-error (ast:name statement) "Superclass must be a class"))
                superclass))))
      (env:define environment (lexeme (ast:name statement)) nil)
      (when superclass
        (setf environment (env:make-environment environment))
        (env:define environment "super" superclass))
      (loop with methods = (make-hash-table :test #'equal)
            for method across (ast:methods statement)
            for method-name = (lexeme (ast:name method))
            for func = (make-lox-function method
                                          environment
                                          (string= method-name "this"))
            do (setf (gethash (lexeme (ast:name method)) methods) func)
            finally (let ((class (make-lox-class
                                  (lexeme (ast:name statement))
                                  superclass
                                  methods)))
                      (when superclass
                        (setf environment (env:enclosing environment)))
                      (env:assign environment (ast:name statement) class))))))

(defmethod execute ((interpreter interpreter) (statement ast:expression))
  (evaluate interpreter (ast:expression statement)))

(defmethod execute ((interpreter interpreter) (statement ast:function))
  (let* ((environment (environment interpreter))
         (function (make-lox-function statement environment)))
    (env:define environment (lexeme (ast:name statement)) function)))

(defmethod execute ((interpreter interpreter) (statement ast:if))
  (let ((condition (ast:condition statement))
        (then (ast:then-branch statement))
        (else (ast:else-branch statement)))
    (cond ((evaluate interpreter condition)
           (execute interpreter then))
          (else
           (execute interpreter else)))))

(defmethod execute ((interpreter interpreter) (statement ast:print))
  (let ((value (evaluate interpreter (ast:expression statement))))
    (write-line (stringify value))))

(defun stringify (object)
  (etypecase object
    (string object)
    (number (write-to-string object))
    (boolean (if object "true" "false"))
    (lox-object (lox-object->string object))))

(defmethod execute ((interpreter interpreter) (statement ast:return))
  (let* ((value (ast:value statement))
         (value (and value (evaluate interpreter value))))
    (signal 'return-value :value value)))

(defmethod execute ((interpreter interpreter) (statement ast:var))
  (let* ((initializer (ast:initializer statement))
         (value (and initializer (evaluate interpreter initializer))))
    (env:define (environment interpreter) (lexeme (ast:name statement)) value)))

(defmethod execute ((interpreter interpreter) (statement ast:while))
  (do ()
      ((not (evaluate interpreter (ast:condition statement))))
    (execute interpreter (ast:body statement))))

(defgeneric evaluate (interpreter expression))

(defmethod evaluate ((interpreter interpreter) (expression ast:assign))
  (let ((name (ast:name expression))
        (value (evaluate interpreter (ast:value expression))))
    (multiple-value-bind (distance found-p) (gethash expression (locals interpreter))
      (if found-p
          (env:assign-at (environment interpreter) distance name value)
          (env:assign (globals interpreter) name value)))))

(defmethod evaluate ((interpreter interpreter) (expression ast:get))
  (let ((object (evaluate interpreter (ast:object expression))))
    (if (lox-instance-p object)
        (get-instance-property object (ast:name expression))
        (runtime-error (ast:name expression) "Only instances have properties"))))

(defmethod evaluate ((interpreter interpreter) (expression ast:set))
  (let ((object (evaluate interpreter (ast:object expression))))
    (unless (lox-instance-p object)
      (runtime-error (ast:name expression) "Only instances have fields"))
    (let ((value (evaluate interpreter (ast:value expression))))
      (set-instance-property object (ast:name expression) value))))

(defmethod evaluate ((interpreter interpreter) (expression ast:super))
  (with-accessors ((environment environment) (locals locals)) interpreter
    (let* ((distance (gethash expression locals))
           (superclass (env:get-at environment distance "super"))
           (object (env:get-at environment (1- distance) "this"))
           (method (find-lox-method superclass (lexeme (ast:method expression)))))
      (if method
          (bind method object)
          (runtime-error
           (ast:method expression)
           (format nil "Undefined property ~a" (lexeme (ast:method expression))))))))

(defmethod evaluate ((interpreter interpreter) (expression ast:this))
  (lookup-variable interpreter (ast:token expression) expression))

(defmethod evaluate ((interpreter interpreter) (expression ast:variable))
  (lookup-variable interpreter (ast:name expression) expression))

(defun lookup-variable (interpreter name expr)
  (multiple-value-bind (distance found-p) (gethash expr (locals interpreter))
    (if found-p
        (env:get-at (environment interpreter) distance (lexeme name))
        (env:get (globals interpreter) name))))

(defmethod evaluate ((interpreter interpreter) (expression ast:call))
  (let ((arguments (ast:arguments expression))
        (callee (evaluate interpreter (ast:callee expression))))
    (unless (lox-callable-p callee)
      (runtime-error (ast:token expression)
                     (format nil "Can only call functions and classes")))
    (unless (= (length arguments) (arity callee))
      (runtime-error (ast:token expression)
                     (format nil "Expected ~a arguments but got ~a"
                             (arity callee) (length arguments))))
    (let ((arguments (map 'vector
                          (lambda (argument) (evaluate interpreter argument))
                          arguments)))
      (call callee interpreter arguments))))

(defmethod evaluate ((interpreter interpreter) (expression ast:grouping))
  (evaluate interpreter (ast:expression expression)))

(defmethod evaluate ((interpreter interpreter) (expression ast:logical))
  (ecase (token-type (ast:operator expression))
    (:or (or (evaluate interpreter (ast:left expression))
             (evaluate interpreter (ast:right expression))))
    (:and (and (evaluate interpreter (ast:left expression))
               (evaluate interpreter (ast:right expression))))))

(defmethod evaluate ((interpreter interpreter) (expression ast:ternary))
  (if (evaluate interpreter (ast:condition expression))
      (evaluate interpreter (ast:consequent expression))
      (evaluate interpreter (ast:alternative expression))))

(defmethod evaluate ((interpreter interpreter) (expression ast:literal))
  (ast:value expression))

(defmethod evaluate ((interpreter interpreter) (expression ast:unary))
  (let ((expr (evaluate interpreter (ast:right expression)))
        (operator (ast:operator expression)))
    (ecase (token-type operator)
      (:bang (not expr))
      (:minus
       (check-number-operand operator expr)
       (- expr)))))

(defmethod evaluate ((interpreter interpreter) (expression ast:binary))
  (let ((left (evaluate interpreter (ast:left expression)))
        (right (evaluate interpreter (ast:right expression)))
        (operator (ast:operator expression)))
    (ecase (token-type operator)
      (:bang-equal (not (equal left right)))
      (:equal-equal (equal left right))
      (:greater
       (check-number-operands operator left right)
       (> left right))
      (:greater-equal
       (check-number-operands operator left right)
       (>= left right))
      (:less
       (check-number-operands operator left right)
       (< left right))
      (:less-equal
       (check-number-operands operator left right)
       (<= left right))
      (:minus
       (check-number-operands operator left right)
       (- left right))
      (:plus
       (cond ((and (numberp left) (numberp right))
              (+ left right))
             ((and (stringp left) (stringp right))
              (concatenate 'string left right))
             (t (runtime-error operator "Operands must be two numbers or two strings"))))
      (:slash
       (check-number-operands operator left right)
       (/ left right))
      (:star
       (check-number-operands operator left right)
       (* left right)))))

(defun check-number-operand (operator expr)
  (unless (numberp expr)
    (runtime-error operator "Operand must be a number")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (runtime-error operator "Operands must be numbers")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Return condition.

(define-condition return-value (condition)
  ((value :initarg :value :reader value))
  (:default-initargs
   :value (a:required-argument :value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Representation of objects.

(defclass lox-object () ())

(defun lox-object-p (object)
  (typep object 'lox-object))

(defgeneric lox-object->string (object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Callable interface.

(defclass lox-callable (lox-object) ())

(defun lox-callable-p (object)
  (typep object 'lox-callable))

(defgeneric arity (callable))

(defgeneric call (callable interpreter arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lox function representation.

(defclass lox-function (lox-callable)
  ((%declaration :initarg :declaration :accessor %declaration)
   (closure :initarg :closure :accessor closure)
   (initializer-p :initarg :initializer-p :accessor initializer-p))
  (:default-initargs
   :declaration (a:required-argument :declaration)
   :closure (a:required-argument :closure)
   :initializer-p nil))

(defun make-lox-function (declaration closure &optional initializer-p)
  (make-instance 'lox-function
                 :declaration declaration
                 :closure closure
                 :initializer-p initializer-p))

(defmethod lox-object->string ((object lox-function))
  (format nil "<fn ~a>" (lexeme (ast:name (%declaration object)))))

(defmethod arity ((callable lox-function))
  (length (ast:params (%declaration callable))))

(defmethod call ((callable lox-function) (interpreter interpreter) arguments)
  (with-slots (closure %declaration initializer-p) callable
    (let ((params (ast:params %declaration))
          (environment (env:make-environment closure)))
      (dotimes (i (length params))
        (env:define environment (lexeme (aref params i)) (aref arguments i)))
      (handler-case
          (progn
            (execute-block interpreter (ast:body %declaration) environment)
            (when initializer-p (env:get-at closure 0 "this")))
        (return-value (condition)
          (if initializer-p
              (env:get-at closure 0 "this")
              (value condition)))))))

(defun bind (lox-function lox-instance)
  (let ((environment (env:make-environment (closure lox-function))))
    (env:define environment "this" lox-instance)
    (make-lox-function (%declaration lox-function)
                       environment
                       (initializer-p lox-function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Native functions.

(defclass lox-native-function (lox-callable)
  ((name :initarg :name :accessor name))
  (:default-initargs
   :name (a:required-argument :name)))

(defun make-lox-native-function (name)
  (make-instance 'lox-native-function :name name))

(defmethod lox-object->string ((object lox-native-function))
  "<native fn>")

(defvar *lox-clock* (make-lox-native-function "clock"))
(push *lox-clock* *lox-native-functions*)

(defmethod arity ((callable (eql *lox-clock*)))
  0)

(defmethod call ((callable (eql *lox-clock*)) (interpreter interpreter) arguments)
  (/ (get-internal-real-time) internal-time-units-per-second))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lox classes.

(defclass lox-class (lox-callable)
  ((name :initarg :name :accessor name)
   (superclass :initarg :superclass :accessor superclass)
   (methods :initarg :methods :accessor methods))
  (:default-initargs
   :name (a:required-argument :name)
   :superclass (a:required-argument :superclass)
   :methods (a:required-argument :methods)))

(defun make-lox-class (name superclass methods)
  (make-instance 'lox-class :name name
                            :superclass superclass
                            :methods methods))

(defun lox-class-p (object)
  (typep object 'lox-class))

(defun find-lox-method (lox-class method-name)
  (multiple-value-bind (value found-p) (gethash method-name (methods lox-class))
    (cond (found-p
           value)
          ((superclass lox-class)
           (find-lox-method (superclass lox-class) method-name)))))

(defmethod lox-object->string ((object lox-class))
  (name object))

(defmethod arity ((callable lox-class))
  (let ((initializer (find-lox-method callable "this")))
    (if initializer (arity initializer) 0)))

(defmethod call ((callable lox-class) (interpreter interpreter) arguments)
  (let ((instance (make-lox-instance callable))
        (initializer (find-lox-method callable "init")))
    (when initializer
      (call (bind initializer instance) interpreter arguments))
    instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lox instances.

(defclass lox-instance (lox-object)
  ((class :initarg :class :accessor instance-class)
   (fields :initform (make-hash-table :test #'equal) :accessor fields))
  (:default-initargs
   :class (a:required-argument :class)))

(defun make-lox-instance (class)
  (make-instance 'lox-instance :class class))

(defun lox-instance-p (object)
  (typep object 'lox-instance))

(defmethod lox-object->string ((object lox-instance))
  (format nil "~a instance" (name (instance-class object))))

(defun get-instance-property (lox-instance token)
  (let ((lexeme (lexeme token)))
    (multiple-value-bind (property found-p) (gethash lexeme (fields lox-instance))
      (if found-p
          property
          (let ((method (find-lox-method (instance-class lox-instance) lexeme)))
            (if method
                (bind method lox-instance)
                (runtime-error token (format nil "Undefined property ~a" lexeme))))))))

(defun set-instance-property (lox-instance token value)
  (setf (gethash (lexeme token) (fields lox-instance)) value))
