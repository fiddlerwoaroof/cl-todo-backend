(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build ()
    (ql:quickload :todo-backend)
    (handler-case
        (progn (setq sb-alien::*shared-objects* nil)
               (save-lisp-and-die (car (last (uiop:command-line-arguments)))
                                  :executable t
                                  :toplevel (find-symbol "MAIN" :fwoar.todo)
                                  :save-runtime-options t
                                  :compression 5))
      (serious-condition (c)
        (format t "~&fail: ~a~%" c)
        (sb-ext:exit :code 1)))))

(build)
