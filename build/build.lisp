#.(progn (ql:quickload :legit)
         nil)

(defun clone-github-repo (user repo)
  (let ((result-dir (merge-pathnames (make-pathname :directory (list :relative
                                                                     "quicklisp" "local-projects"
                                                                     repo)
                                                    :defaults #p"/")
                                     (user-homedir-pathname))))
    (legit:git-clone (format nil "https://github.com/~a/~a.git" user repo)
                     :directory result-dir)))

(clone-github-repo "fukamachi" "lack")
(clone-github-repo "fiddlerwoaroof" "data-lens")
(clone-github-repo "fiddlerwoaroof" "cl-todo-backend")

(ql:quickload :todo-backend)
