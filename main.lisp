(in-package :fwoar.todo)

;;; entrypoint
(defun setup ()
  (let ((app (make-instance 'ningle:<app>)))
    (prog1 app (setup-routes app))))

(defvar *handler*)

(defun is-running ()
  (and (boundp '*handler*)
       *handler*))

(defun ensure-started (&rest r &key (address "127.0.0.1") (port 5000))
  (declare (ignore address port))
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

(defun main (&rest _)
  (declare (ignore _))
  (ensure-started :address "0.0.0.0" :port 5000)
  (loop (sleep 5)))
