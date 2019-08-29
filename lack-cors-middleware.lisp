(in-package :fwoar.lack.cors.middleware)
;; cors middleware
(defparameter *acao-lens*
  (data-lens:<>1 (data-lens.lenses:make-list-lens 1)
                 (data-lens.lenses:make-plist-lens :Access-Control-Allow-Origin)))
(defparameter *acah-lens*
  (data-lens:<>1 (data-lens.lenses:make-list-lens 1)
                 (data-lens.lenses:make-plist-lens :Access-Control-Allow-Headers)))
(defparameter *acam-lens*
  (data-lens:<>1 (data-lens.lenses:make-list-lens 1)
                 (data-lens.lenses:make-plist-lens :Access-Control-Allow-Methods)))

(defun cors-middleware (app)
  (lambda (env)
    (rutilsx.threading:->>
     (if (eq :options
             (getf env :request-method))
         '(200 nil nil)
         (let ((res (funcall app env)))
           res))
     (data-lens.lenses:set *acao-lens* "*")
     (data-lens.lenses:set *acah-lens* "Content-Type")
     (data-lens.lenses:set *acam-lens* "GET,POST,DELETE,PATCH"))))
