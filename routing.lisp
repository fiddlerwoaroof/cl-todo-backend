;; [[file:~/git_repos/lisp-sandbox/todo/README.org::*routing.lisp%20source][routing.lisp source:1]]
;; [[file:~/git_repos/lisp-sandbox/todo/README.org::package-include][package-include]]
(in-package :fwoar.todo)

;; package-include ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::defroutes][defroutes]]
(defmacro defroutes (app &body routes)
  (alexandria:once-only (app)
    `(setf
      ,@(loop for (target . descriptors) in routes
              append (loop for (method callback) in descriptors
                           append `((ningle:route ,app ,target
                                                  :method method)
                                    ,callback))))))
;; defroutes ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::routing-helpers][routing-helpers]]
(defun success (value)
  (list 200 '(:conent-type "application/json") value))

(defmacro handler ((&optional (sym (gensym "PARAMS"))) &body body)
  `(lambda (,sym)
     (declare (ignorable ,sym))
     (success
      (fwoar.lack.json.middleware:wrap-result
       (progn ,@body)))))
;; routing-helpers ends here

;; [[file:~/git_repos/lisp-sandbox/todo/README.org::todo-routes][todo-routes]]
;; routing
(defun get-id (params)
  (parse-integer (serapeum:assocdr :id params)))

(defun setup-routes (app)
  (defroutes app
    ("/" (:GET (handler () (todos)))
         (:POST (handler (v) (new-todo v)))
         (:DELETE (handler () (clear-todos))))
    ("/todo/:id" (:GET    (handler (v) (todo (get-id v))))
                 (:DELETE (handler (v)
                            (delete-todo (get-id v))
                            nil))
                 (:PATCH  (handler (v)
                            (update-todo (get-id v) 
                                         (remove :id v :key #'car)))))))
;; todo-routes ends here
;; routing.lisp source:1 ends here
