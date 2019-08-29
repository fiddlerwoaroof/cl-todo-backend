(in-package :fwoar.todo)

(defmacro defroutes (app &body routes)
  "Define a set of routes for given paths. the ROUTES parameter expects this format:
   ((\"/path/to/{route}\" :method :POST) route-callback) the AS-ROUTE macro helps one
   avoid binding function values to the route for flexibility."
  (alexandria:once-only (app)
    `(progn
       ,@(loop for ((target &key method) callback) in routes
               collect `(setf (ningle:route ,app ,target :method ,(or method :GET)) ,callback)))))


;; routing
(defun success (value)
  (list 200 nil value))

(defmacro handler ((&optional (sym (gensym "PARAMS"))) &body body)
  `(lambda (,sym)
     (declare (ignorable ,sym))
     (success
      (fwoar.lack.json.middleware:wrap-result
       (progn ,@body)))))

(defun get-id (params)
  (parse-integer (serapeum:assocdr :id params)))

(defun setup-routes (app)
  (defroutes app
    (("/" :method :GET)            (handler () (todos)))
    (("/" :method :POST)           (handler (v) (new-todo v)))
    (("/" :method :DELETE)         (handler () (clear-todos)))
    (("/todo/:id" :method :GET)    (handler (v) (todo (get-id v))))
    (("/todo/:id" :method :DELETE) (handler (v)
                                     (delete-todo (get-id v))
                                     nil))
    (("/todo/:id" :method :PATCH)  (handler (v)
                                     (update-todo (get-id v) 
                                                  (remove :id v :key #'car))))))
