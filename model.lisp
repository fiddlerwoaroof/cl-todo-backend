(in-package :fwoar.todo)

;; todo "database" api
(defvar *todos* (fset:empty-map))

(defun todos ()
  (gmap:gmap :seq
             (lambda (_ b)
               (declare (ignore _))
               b)
             (:map *todos*)))

(defun todo (id)
  (let ((todo (fset:@ *todos* id)))
    todo))

(defun (setf todo) (new-value id)
  (setf *todos*
        (fset:with *todos* id new-value))
  new-value)

(defun delete-todo (id)
  (setf *todos*
        (fset:less *todos* id)))

(defparameter *cur-id* 0)
(defun next-id ()
  (incf *cur-id*))

(defun new-todo (value)
  (let ((id (next-id)))
    (setf (todo id)
          (alexandria:alist-hash-table
           (rutilsx.threading:->>
            value
            (acons "completed" 'yason:false)
            (acons "url" (format nil "http://localhost:5000/todo/~d" id)))
           :test 'equal))))

(defun clear-todos ()
  (setf *todos*
        (fset:empty-map)))

(defun update-todo (id v)
  (setf (todo id)
        (serapeum:merge-tables (or (todo id)
                                   (make-hash-table :test 'equal))
                               (alexandria:alist-hash-table
                                v
                                :test 'equal))))
