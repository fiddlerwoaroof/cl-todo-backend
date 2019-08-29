(in-package :fwoar.lack.json.middleware)

;; json middleware
(defparameter *result-lens*
  (data-lens.lenses:make-list-lens 2))

(defun json-middleware (app)
  (lambda (env)
    (let ((res (funcall app env)))
      (data-lens.lenses:over *result-lens* 'encode-result
                             res))))

(defun encode-json-to-string (v)
  (yason:with-output-to-string* (:indent t)
    (yason:encode v
                  yason::*json-output*)))

(defclass json-result ()
  ((%v :initarg :v :reader json-value)))

(defun wrap-result (v)
  (make-instance 'json-result :v v))

(defgeneric encode-result (v)
  (:method (v)
    v)
  (:method ((v json-result))
    (list (encode-json-to-string (json-value v)))))

(defmethod yason:encode ((o fset:seq) &optional s)
  (yason:encode (coerce (fset:convert 'list o) 'vector)
                s))

(defmethod yason:encode ((o fset:map) &optional s)
  (yason:encode (fset:convert 'hash-table o)
                s))
