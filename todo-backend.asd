;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :todo-backend 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:yason
               #:fset
               #:ningle
               #:alexandria
               #:clack)
  :components ((:file "package")
               (:file "lack-cors-middleware" :depends-on ("package"))
               (:file "lack-json-middleware" :depends-on ("package"))
               (:file "model" :depends-on ("package"))
               (:file "routing" :depends-on ("package"
                                             "model"))
               (:file "main" :depends-on ("package"
                                          "routing"
                                          "lack-cors-middleware"
                                          "lack-json-middleware"))))
