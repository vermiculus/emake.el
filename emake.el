;;; Emacs-Make -- simple, transparent functionality for automated elisp testing
;;; Based on http://edkolev.github.io/posts/2017-09-10-travis-for-emacs-packages.html

;; PACKAGE_FILE     := the root file of your package
;; PACKAGE_LISP     := list of Lisp files separated by spaces
;; PACKAGE_ARCHIVES := list of named ELPA archives separated by spaces; see also
;;                     `emake-package-archive-master-alist'

;;; You can debug emacs-make by setting the environment variable EMACS_MAKE_DEBUG_MODE.

(setq debug-on-error (getenv "EMACS_MAKE_DEBUG_MODE"))

(require 'package)
(require 'subr-x)

(defun emake-message (format &rest args)
  "Print a message to standard out"
  (apply #'message (concat "emake: " format) args))

(defconst emake-package-file
  (getenv "PACKAGE_FILE")
  "The Elisp file with package headers")

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
      (mapcar #'downcase)
      (mapcar #'intern)))
  "List of names of archives in use by the package
Keys in `emake-package-archive-master-alist'.")

(defconst emake-package-archive-master-alist
  '((melpa . "http://melpa.org/packages/")
    (gnu . "http://elpa.gnu.org/packages/"))
  "Alist of archive-names to their locations")

(defmacro emake-with-elpa (&rest body)
  "Run BODY after setting up ELPA context"
  (declare (debug t))
  `(let ((package-user-dir (expand-file-name (format ".elpa/%s/elpa" emacs-version)
                                             emake-project-root))
         (package-archives nil))
     (dolist (pair emake-package-archive-master-alist)
       (when (memq (car pair) emake-package-archives)
         (push pair package-archives)))
     (emake-message "initializing package.el...")
     (package-initialize)
     (emake-message "initializing package.el...done")
     ,@body))

(defun emake (target)
  "Run `emake-TARGET' if bound."
  (let ((fun (intern (format "emake-%s" target))))
    (unless (fboundp fun)
      (error "%S target not found" target))
    (emake-message "Running target %S with %S\n" target fun)
    (emake-message "Project file detected as: %S" emake-package-file)
    (apply fun (prog1 command-line-args-left
                 (setq command-line-args-left nil)))))

(defun emake-test ()
  "Run all tests in \"PACKAGE-NAME-test.el\"."
  (let* ((default-directory emake-project-root)
         (project-tests-path (expand-file-name "test/" emake-project-root))
         (project-tests-file (expand-file-name (format "%S-test.el" (package-desc-name emake-package-desc))
                                               project-tests-path)))

    (emake-with-elpa
     ;; add the package being tested to `load-path' so it can be required
     (add-to-list 'load-path emake-project-root)
     (add-to-list 'load-path project-tests-path)

     ;; load the file with tests
     (load project-tests-file)

     ;; run the tests and exit with an appropriate status
     (ert-run-tests-batch-and-exit))))

(defun emake-install ()
  "Install dependencies.
Required packages include those that `emake-package-file' lists as
dependencies."
  (emake-with-elpa
   (let ((msg (format "installing in %s..." package-user-dir)))
     (emake-message msg)
     (package-refresh-contents)

     ;; install dependencies
     (dolist (package (thread-last (package-desc-reqs emake-package-desc)
                        (mapcar #'car)
                        (delq 'emacs)))
       (unless (package-installed-p package)
         (ignore-errors
           (package-install package))))

     (emake-message (concat msg "done")))))

(defun emake-build ()
  "Compile all files in PACKAGE_LISP"
  (require 'bytecomp)
  (emake-with-elpa
   (add-to-list 'load-path emake-project-root)
   (dolist (f (split-string (getenv "PACKAGE_LISP") nil 'omit-nulls))
     (emake-message "Compiling %s" f)
     (byte-compile-file f))))
