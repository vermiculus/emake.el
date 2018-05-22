;;; Emacs-Make -- simple, transparent functionality for automated elisp testing
;;; Based on http://edkolev.github.io/posts/2017-09-10-travis-for-emacs-packages.html

;; PACKAGE_FILE     := the root file of your package
;; PACKAGE_TESTS    := the root file to load your tests
;; PACKAGE_LISP     := list of Lisp files in this package separated by spaces
;; PACKAGE_ARCHIVES := list of named ELPA archives separated by spaces; see also
;;                     `emake-package-archive-master-alist'

;;; You can debug emacs-make by setting the environment variable EMACS_MAKE_DEBUG_MODE.

(setq debug-on-error (getenv "EMACS_MAKE_DEBUG_MODE"))

(require 'package)
(require 'subr-x)
(require 'cl-lib)

(defun emake-message (format &rest args)
  "Print a message to standard out"
  (apply #'message (concat "emake: " format) args))

(defconst emake-package-file
  (getenv "PACKAGE_FILE")
  "The Elisp file with package headers")

(defconst emake-package-tests-file
  (getenv "PACKAGE_TESTS")
  "The Elisp file with test definitions")

(defvar emake-package-desc
  (with-temp-buffer
    (insert-file-contents-literally emake-package-file)
    (package-buffer-info)))

(defconst emake-project-root
  (locate-dominating-file (or (file-name-directory emake-package-file)
                              default-directory)
                          emake-package-file)
  "The folder `emake-package-file' is in")

(defconst emake-package-archives
  (when-let ((deps (getenv "PACKAGE_ARCHIVES")))
    (thread-last (split-string deps nil 'omit-nulls)
      (mapcar #'downcase)))
  "List of names of archives in use by the package
Keys in `emake-package-archive-master-alist'.")

(defconst emake-package-archive-master-alist
  '(("melpa" . "http://melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/"))
  "Alist of archive-names to their locations")

(defmacro emake-task (description &rest body)
  "Run BODY wrapped by DESCRIPTION messages"
  (declare (indent 1))
  (let ((Sdescription (cl-gensym)))
    `(let ((,Sdescription (concat ,description "...")))
       (prog2
           (emake-message ,Sdescription)
           (progn ,@body)
         (emake-message (concat ,Sdescription "done"))))))

(defmacro emake-with-elpa (&rest body)
  "Run BODY after setting up ELPA context"
  (declare (debug t))
  `(let ((package-user-dir (expand-file-name (format ".elpa/%s/elpa" emacs-version)
                                             emake-project-root))
         (package-archives nil))
     (dolist (pair emake-package-archive-master-alist)
       (when (member (car pair) emake-package-archives)
         (push pair package-archives)))
     (emake-task "initializing package.el"
       (package-initialize))
     ,@body))

(defun emake (target)
  "Run `emake-TARGET' if bound."
  (let ((fun (intern (format "emake-my-%s" target))))
    (unless (fboundp fun)
      (setq fun (intern (format "emake-%s" target))))
    (unless (fboundp fun)
      (error "%S target not found" target))
    (emake-message "Running target %S as %S" target fun)
    (apply fun (prog1 command-line-args-left
                 (setq command-line-args-left nil)))))

(defun emake-test ()
  "Run all tests in \"PACKAGE-NAME-test.el\"."
  (let ((default-directory emake-project-root))
    (emake-with-elpa
     ;; add the package being tested to `load-path' so it can be required
     (add-to-list 'load-path emake-project-root)
     (add-to-list 'load-path (file-name-directory emake-package-tests-file))

     ;; load the file with tests
     (load emake-package-tests-file)

     ;; run the tests and exit with an appropriate status
     (ert-run-tests-batch-and-exit))))

(defun emake-install ()
  "Install dependencies.
Required packages include those that `emake-package-file' lists as
dependencies."
  (emake-with-elpa
   (emake-task (format "installing in %s" package-user-dir)
     (package-refresh-contents)

     ;; install dependencies
     (dolist (package (thread-last (package-desc-reqs emake-package-desc)
                        (mapcar #'car)
                        (delq 'emacs)))
       (unless (package-installed-p package)
         (ignore-errors
           (package-install package)))))))

(defun emake-compile ()
  "Compile all files in PACKAGE_LISP"
  (require 'bytecomp)
  (emake-with-elpa
   (add-to-list 'load-path emake-project-root)
   (dolist (f (split-string (getenv "PACKAGE_LISP") nil 'omit-nulls))
     (emake-task (format "compiling %s" f)
       (byte-compile-file f)))))
