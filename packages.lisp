(defpackage #:lox.token
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl)
  (:export
   #:make-token
   #:token
   #:token-p
   #:lexeme
   #:line
   #:literal
   #:token-type))

(defpackage #:lox.error
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl #:lox.token)
  (:export
   #:report-error
   #:analysis-error
   #:runtime-error))

(defpackage #:lox.scanner
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl #:lox.token #:lox.error)
  (:export
   #:make-scanner
   #:scan-tokens))

(defpackage #:lox.ast
  (:local-nicknames (#:a #:alexandria))
  (:nicknames #:ast)
  (:use #:cl)
  (:shadow
   #:get
   #:set
   #:method
   #:condition
   #:variable
   #:block
   #:class
   #:function
   #:if
   #:print
   #:return))

(defpackage #:lox.parser
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl #:lox.token #:lox.error)
  (:export
   #:make-parser
   #:parse))

(defpackage #:lox.resolver
  (:use #:cl #:lox.token #:lox.error)
  (:export
   #:resolve))

(defpackage #:lox.environment
  (:nicknames #:env)
  (:use #:cl #:lox.token #:lox.error)
  (:export
   #:make-environment
   #:enclosing
   #:get
   #:assign
   #:get-at
   #:assign-at
   #:define)
  (:shadow
   #:get))

(defpackage #:lox.interpreter
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl #:lox.token #:lox.error)
  (:export
   #:make-interpreter
   #:interpret
   #:resolve-depth))

(defpackage #:lox
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl #:lox.error #:lox.scanner #:lox.parser
        #:lox.resolver #:lox.interpreter))
