(in-package :fwoar.todo)

;;; entrypoint
(defun setup ()
  (let ((app (make-instance 'ningle:<app>)))
    (prog1 app (setup-routes app))))

(defvar *handler*)

(defun is-running ()
  (and (boundp '*handler*)
       *handler*))

(defun ensure-started (&rest r &key port)
  (declare (ignore port))
  (let ((app (setup)))
    (values app
            (setf *handler*
                  (if (not (is-running))
                      (apply 'clack:clackup
                             (lack.builder:builder
                              :accesslog
                              'fwoar.lack.cors.middleware:cors-middleware
                              'fwoar.lack.json.middleware:json-middleware
                              app)
                             r)
                      *handler*)))))

(defun stop ()
  (if (is-running)
      (progn
        (clack:stop *handler*)
        (makunbound '*handler*)
        nil)
      nil))
