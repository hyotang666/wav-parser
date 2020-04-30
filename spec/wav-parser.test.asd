; vim: ft=lisp et
(in-package :asdf)
(defsystem "wav-parser.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "wav-parser")
  :components
  ((:file "wav-parser"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :wav-parser args)))