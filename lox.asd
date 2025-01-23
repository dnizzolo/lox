(defsystem "lox"
  :description "Tree-walk implementation of Lox."
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :maintainer "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license  "MIT"
  :homepage "https://github.com/dnizzolo/lox"
  :source-control (:git "https://github.com/dnizzolo/lox.git")
  :bug-tracker "https://github.com/dnizzolo/lox/issues"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "packages")
               (:file "token")
               (:file "scanner")
               (:file "error")
               (:file "ast")
               (:file "parser")
               (:file "resolver")
               (:file "environment")
               (:file "interpreter")
               (:file "lox")))
