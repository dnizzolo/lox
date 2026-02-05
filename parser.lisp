(in-package #:lox.parser)

(defclass parser ()
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current))
  (:default-initargs
   :tokens (a:required-argument :tokens)))

(defun make-parser (tokens)
  (make-instance 'parser :tokens tokens))

(defun parser-error (token message)
  (report-error token message)
  (error 'parse-error))

(defun parse (parser)
  (do ((statements (make-array 256 :adjustable t :fill-pointer 0)))
      ((at-end-p parser) statements)
    (vector-push-extend (%declaration parser) statements)))

(defun synchronize (parser)
  (advance parser)
  (do ()
      ((at-end-p parser))
    (when (or (eql (token-type (previous parser)) :semicolon)
              (member (token-type (peek parser))
                      '(:class :fun :var :for :if :while :print :return)))
      (return))
    (advance parser)))

(defun %declaration (parser)
  (handler-case
      (cond ((match parser :class) (%class-declaration parser))
            ((match parser :fun) (%function parser "function"))
            ((match parser :var) (%var-declaration parser))
            (t (%statement parser)))
    (parse-error () (synchronize parser))))

(defun %class-declaration (parser)
  (let ((name (consume parser :identifier "Expect class name"))
        (superclass (when (match parser :less)
                      (ast:make-variable
                       (consume parser :identifier "Expect superclass name"))))
        (methods (make-array 16 :adjustable t :fill-pointer 0)))
    (consume parser :left-brace "Expect '{' before class body")
    (do ()
        ((or (check parser :right-brace) (at-end-p parser)))
      (vector-push-extend (%function parser "method") methods))
    (consume parser :right-brace "Expect '}' after class body")
    (ast:make-class name superclass methods)))

(defun %var-declaration (parser)
  (let ((name (consume parser :identifier "Expect variable name"))
        (initializer (when (match parser :equal) (%expression parser))))
    (consume parser :semicolon "Expect ';' after variable declaration")
    (ast:make-var name initializer)))

(defun %statement (parser)
  (cond ((match parser :for) (%for-statement parser))
        ((match parser :if) (%if-statement parser))
        ((match parser :print) (%print-statement parser))
        ((match parser :return) (%return-statement parser))
        ((match parser :while) (%while-statement parser))
        ((match parser :left-brace) (ast:make-block (%block parser)))
        (t (%expression-statement parser))))

(defun %for-statement (parser)
  (consume parser :left-paren "Expect '(' after for")
  (let ((initializer
          (cond ((match parser :semicolon) nil)
                ((match parser :var) (%var-declaration parser))
                (t (%expression-statement parser))))
        (condition
          (prog1 (if (match parser :semicolon)
                     (ast:make-literal t)
                     (%expression parser))
            (consume parser :semicolon "Expect ';' after loop condition")))
        (increment
          (prog1 (unless (match parser :right-paren) (%expression parser))
            (consume parser :right-paren "Expect ')' after loop condition")))
        (body (%statement parser)))
    (when increment
      (setf body (ast:make-block
                  (vector body (ast:make-expression increment)))))
    (setf body (ast:make-while condition body))
    (when initializer
      (setf body (ast:make-block (vector initializer body))))
    body))

(defun %if-statement (parser)
  (let ((condition
          (prog2 (consume parser :left-paren "Expect '(' after if")
              (%expression parser)
            (consume parser :right-paren "Expect ')' after if condition")))
        (then-branch (%statement parser))
        (else-branch (when (match parser :else) (%statement parser))))
    (ast:make-if condition then-branch else-branch)))

(defun %print-statement (parser)
  (ast:make-print
   (prog1 (%expression parser)
     (consume parser :semicolon "Expect ';' after print value"))))

(defun %return-statement (parser)
  (let ((token (previous parser))
        (value (unless (check parser :semicolon) (%expression parser))))
    (consume parser :semicolon "Expect ';'after return value")
    (ast:make-return token value)))

(defun %while-statement (parser)
  (let ((condition
          (prog2 (consume parser :left-paren "Expect '(' after while")
              (%expression parser)
            (consume parser :right-paren "Expect ')' after while condition")))
        (body (%statement parser)))
    (ast:make-while condition body)))

(defun %expression-statement (parser)
  (ast:make-expression
   (prog1
       (%expression parser)
     (consume parser :semicolon "Expect ';' after expression"))))

(defconstant +maximum-parameters+ 255)

(defun %function (parser kind)
  (let ((name
          (prog1
              (consume parser :identifier
                       (concatenate 'string "Expect " kind " name"))
            (consume parser :left-paren
                     (concatenate 'string "Expect '(' after " kind " name"))))
        (parameters (make-array 16 :adjustable t :fill-pointer 0)))
    (unless (check parser :right-paren)
      (loop
        (when (>= (length parameters) +maximum-parameters+)
          (parser-error (peek parser)
                        (format nil "Can't have more than ~a parameters"
                                +maximum-parameters+)))
        (vector-push-extend
         (consume parser :identifier "Expect parameter name")
         parameters)
        (unless (match parser :comma) (return))))
    (consume parser :right-paren "Expect ')' after parameters")
    (consume parser :left-brace (concatenate 'string "Expect '{' before " kind " body"))
    (ast:make-function name parameters (%block parser))))

(defun %block (parser)
  (let ((statements (make-array 16 :adjustable t :fill-pointer 0)))
    (do ()
        ((or (check parser :right-brace) (at-end-p parser)))
      (vector-push-extend (%declaration parser) statements))
    (consume parser :right-brace "Expect '}' after block")
    statements))

(defun %expression (parser)
  (%assignment parser))

(defun %assignment (parser)
  (let ((expr (%ternary parser)))
    (if (match parser :equal)
        (let ((equals (previous parser))
              (value (%assignment parser)))
          (cond ((ast:variable-p expr)
                 (ast:make-assign (ast:name expr) value))
                ((ast:get-p expr)
                 (ast:make-set (ast:object expr) (ast:name expr) value))
                (t (parser-error equals "Invalid assignment target"))))
        expr)))

(defun %ternary (parser)
  (let ((expr (%or parser)))
    (if (match parser :question-mark)
        (let ((consequent (%expression parser)))
          (consume parser :colon
                   "Expect ':' after consequent expression in ternary operator")
          (let ((alternative (%expression parser)))
            (ast:make-ternary expr consequent alternative)))
        expr)))

(defun %or (parser)
  (do ((expr (%and parser)))
      ((not (match parser :or)) expr)
    (let ((token (previous parser))
          (right (%and parser)))
      (setf expr (ast:make-logical expr token right)))))

(defun %and (parser)
  (do ((expr (%equality parser)))
      ((not (match parser :and)) expr)
    (let ((token (previous parser))
          (right (%equality parser)))
      (setf expr (ast:make-logical expr token right)))))

(defun %equality (parser)
  (do ((expr (%comparison parser)))
      ((not (match parser :equal-equal :bang-equal)) expr)
    (let ((token (previous parser))
          (right (%comparison parser)))
      (setf expr (ast:make-binary expr token right)))))

(defun %comparison (parser)
  (do ((expr (%term parser)))
      ((not (match parser :greater :greater-equal :less :less-equal)) expr)
    (let ((token (previous parser))
          (right (%term parser)))
      (setf expr (ast:make-binary expr token right)))))

(defun %term (parser)
  (do ((expr (%factor parser)))
      ((not (match parser :minus :plus)) expr)
    (let ((token (previous parser))
          (right (%factor parser)))
      (setf expr (ast:make-binary expr token right)))))

(defun %factor (parser)
  (do ((expr (%unary parser)))
      ((not (match parser :slash :star)) expr)
    (let ((token (previous parser))
          (right (%unary parser)))
      (setf expr (ast:make-binary expr token right)))))

(defun %unary (parser)
  (if (match parser :bang :minus)
      (let ((token (previous parser))
            (right (%unary parser)))
        (ast:make-unary token right))
      (%call parser)))

(defun %finish-call (parser callee)
  (let ((arguments (make-array 16 :adjustable t :fill-pointer 0)))
    (unless (check parser :right-paren)
      (loop
        (when (>= (length arguments) +maximum-parameters+)
          (parser-error (peek parser)
                        (format nil "Can't have more than ~a arguments"
                                +maximum-parameters+)))
        (vector-push-extend (%expression parser) arguments)
        (unless (match parser :comma) (return))))
    (ast:make-call callee (consume parser :right-paren "Expect ')' after arguments")
                   arguments)))

(defun %call (parser)
  (let ((expr (%primary parser)))
    (loop
      (cond ((match parser :left-paren)
             (setf expr (%finish-call parser expr)))
            ((match parser :dot)
             (setf expr (ast:make-get
                         expr (consume parser :identifier
                                       "Expect property name after '.'"))))
            (t (return expr))))))

(defun %primary (parser)
  (cond ((match parser :false) (ast:make-literal nil))
        ((match parser :true) (ast:make-literal t))
        ((match parser :nil) (ast:make-literal nil))
        ((match parser :number :string)
         (ast:make-literal (literal (previous parser))))
        ((match parser :this)
         (ast:make-this (previous parser)))
        ((match parser :super)
         (let ((keyword (previous parser)))
           (consume parser :dot "Expect '.' after 'super'")
           (let ((method (consume parser :identifier "Expect superclass method name")))
             (ast:make-super keyword method))))
        ((match parser :identifier)
         (ast:make-variable (previous parser)))
        ((match parser :left-paren)
         (let ((expr (%expression parser)))
           (consume parser :right-paren "Expect ')' after expression")
           (ast:make-grouping expr)))
        (t (parser-error (peek parser) "Expect expression"))))

(defun consume (parser token-type message)
  (if (check parser token-type)
      (advance parser)
      (parser-error (peek parser) message)))

(defun at-end-p (parser)
  (or (zerop (length (tokens parser)))
      (eql (token-type (peek parser)) :eof)))

(defun peek (parser)
  (aref (tokens parser) (current parser)))

(defun previous (parser)
  (aref (tokens parser) (1- (current parser))))

(defun advance (parser)
  (prog1 (peek parser)
    (unless (at-end-p parser)
      (incf (current parser)))))

(defun check (parser token-type)
  (unless (at-end-p parser)
    (eql (token-type (peek parser)) token-type)))

(defun match (parser &rest expected)
  (dolist (item expected)
    (when (check parser item)
      (return (advance parser)))))
