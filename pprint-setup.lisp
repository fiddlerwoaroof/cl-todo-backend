(in-package :cl-user)
(defvar *old-pprint-dispatch* *print-pprint-dispatch*)

(setf *print-pprint-dispatch*
      (copy-pprint-dispatch *old-pprint-dispatch*))

(defun pprint-hashtable (s hash-table)
  (pprint-logical-block (s nil)
    (princ "#<hash-table"  s)
    (let ((v (fset:convert 'list (fset:convert 'fset:map hash-table))))
      (when v
        (pprint-logical-block (s v)
          (pprint-indent :block 0 s)
          (loop do
            (destructuring-bind (key . value) (pprint-pop)
              (format s " ~s: ~s" key value)
              (pprint-exit-if-list-exhausted)
              (princ ", " s)
              (pprint-newline :mandatory s))))))
    (princ #\> s)))

(set-pprint-dispatch 'hash-table 'pprint-hashtable)
