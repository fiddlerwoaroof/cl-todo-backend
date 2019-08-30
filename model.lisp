;; [[file:~/git_repos/lisp-sandbox/todo/README.org::*model.lisp%20source%20code][model.lisp source code:1]]
;; [[file:~/git_repos/lisp-sandbox/todo/README.org::package-include][package-include]]
(in-package :fwoar.todo)

;; package-include ends here
;; [[file:~/git_repos/lisp-sandbox/todo/README.org::model-utils][model-utils]]
(defparameter *cur-id* 0)
(defun next-id ()
  (incf *cur-id*))

(defparameter *completed-lens*
  (data-lens.lenses:make-hash-table-lens "completed"))

(defun bool-to-yason (bool)
  (if bool
      'yason:true
      'yason:false))
;; model-utils ends here

(defvar *todos* (fset:empty-map))

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::todolist-manipulation][todolist-manipulation]]
(defun todos ()
  (gmap:gmap :seq
             (lambda (_ b)
               (declare (ignore _))
               b)
             (:map *todos*)))

(defun clear-todos ()
  (setf *todos*
        (fset:empty-map)))
;; todolist-manipulation ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::todo-accessor][todo-accessor]]
(defun todo (id)
  (let ((todo (fset:@ *todos* id)))
    todo))

(defun (setf todo) (new-value id)
  (setf (fset:@ *todos* id)
        new-value))

(defun delete-todo (id)
  (setf *todos*
        (fset:less *todos* id)))
;; todo-accessor ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::new-todo][new-todo]]
(defvar *external-host*
  "localhost")
(defvar *external-port*
  5000)

(defun new-todo (value)
  (let ((id (next-id)))
    (setf (todo id)
          (alexandria:alist-hash-table
           (rutilsx.threading:->>
            value
            (acons "completed" 'yason:false)
            (acons "url" (format nil "http://~a:~d/todo/~d" *external-host* *external-port* id)))
           :test 'equal))))
;; new-todo ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::update-todo][update-todo]]
(defun update-todo (id v)
  (setf (todo id)
        (serapeum:merge-tables (or (todo id)
                                   (make-hash-table :test 'equal))
                               (data-lens.lenses:over *completed-lens*
                                                      'bool-to-yason
                                                      (alexandria:alist-hash-table
                                                       v
                                                       :test 'equal)))))
;; update-todo ends here

(defmacro with-fresh-todos (() &body body)
  `(let ((*todos* (fset:empty-map)))
     ,@body))
;; model.lisp source code:1 ends here
