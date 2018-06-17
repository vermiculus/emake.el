;;; emake.el --- simple automated Elisp testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: tools, lisp
;; Homepage: https://github.com/vermiculus/emake.el
;; Package-Requires: ((emacs "25"))
;; Package-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Original versions based on:
;;
;;     URL `http://evgeni.io/posts/travis-for-emacs-packages/'
;;
;; Emacs-Make is driven by environment variables to know what to do.
;; Before invoking `emake', ensure the following variables are set
;; appropriately:
;;
;;     PACKAGE_FILE     := the root file of your package
;;     PACKAGE_TESTS    := space-delimited list of Lisp files to load
;;                         to define your tests
;;     PACKAGE_LISP     := space-delimited list of Lisp files in this
;;                         package
;;     PACKAGE_ARCHIVES := space-delimited list of named ELPA archives;
;;                         see also `emake-package-archive-master-alist'
;;
;; There are certainly scenarios where a testing suite would want to
;; use special packages in its own right, but not require them for its
;; users.  For this case, a few more variables exist:
;;
;;     PACKAGE_TEST_DEPS     := space-delimited list of packages needed
;;                              by the test suite
;;     PACKAGE_TEST_ARCHIVES := like PACKAGE_ARCHIVES, but used for
;;                              installing PACKAGE_TEST_DEPS.
;;
;; To debug Emacs-Make, set the environment variable EMAKE_DEBUG_FLAGS.
;;
;; See the file README.org in this directory for more information and
;; minimal examples.

;;; Code:

(require 'package)
(require 'subr-x)
(require 'cl-lib)

(defun emake--declaration-environment-variables (fn _fn-args &rest environment-variables)
  "Note that FN (i.e., a target) depends on ENVIRONMENT-VARIABLES.
See also `emake-help'."
  (function-put fn 'emake-environment-variables environment-variables)
  nil)

(defun emake--declaration-default-target (fn _fn-args target)
  "Note that FN is TARGET.
See also `emake--resolve-target'."
  (function-put fn 'emake-default-target target)
  nil)

(defun emake--declaration-target (fn _fn-args target)
  "Note that FN is a custom/override TARGET.
See also `emake--resolve-target'."
  (function-put fn 'emake-target target)
  nil)

;; activate (declare ...) properties
(nconc defun-declarations-alist
       '((emake-environment-variables emake--declaration-environment-variables)
         (emake-default-target emake--declaration-default-target)
         (emake-target emake--declaration-target)))

(defvar emake-environment-variables
  '(("EMACS_VERSION" . "MAJOR.MINOR version of Emacs we intended to run under")
    ("EMAKE_DEBUG_FLAGS" . "have EMake print debugging information; see source for details")
    ("PACKAGE_FILE" . "root file of your package")
    ("PACKAGE_TESTS" . "space-delimited list of Lisp files to load to define your tests")
    ("PACKAGE_LISP" . "space-delimited list of Lisp files in this package")
    ("PACKAGE_ARCHIVES" . "space-delimited list of named ELPA archives; see also `emake-package-archive-master-alist'")
    ("PACKAGE_TEST_DEPS" . "space-delimited list of packages needed by the test suite")
    ("PACKAGE_IGNORE_DEPS" . "space-delimited list of dependencies to ignore when installing")
    ("PACKAGE_TEST_ARCHIVES" . "space-delimited list of named ELPA archives needed by the test suite; see also `emake-package-archive-master-alist'"))
  "List of environment variables used by EMake targets.")

;;; Dealing with environment variables

(defvar emake--env-cache nil
  "Alist mapping environment variables to their values.")

(defun emake--getenv (variable)
  "Get the value of VARIABLE in the current environment."
  (cl-assert (assoc-string variable emake-environment-variables) 'show-args)
  (if-let ((entry (assoc-string variable emake--env-cache)))
      (cdr entry)
    (let ((val (getenv variable)))
      (push (cons variable val) emake--env-cache)
      val)))

(defun emake--clean-list (env)
  "Return a list made from the environment variable ENV.
ENV is expected to be a space-separated string."
  (when-let ((vals (emake--getenv env)))
    (split-string vals nil 'omit-nulls)))

(defvar emake--debug-flags (emake--clean-list "EMAKE_DEBUG_FLAGS")
  "Flags to control non-functional behavior of EMake.

show-environment:
  show relevant environment variables when running targets.

debug:
  set `debug-on-error' when running targets.")

(defun emake-verify-version ()
  "Prints \"ok\" if the correct version of Emacs is being used.
Compares the MAJOR.MINOR versions of variable `emacs-version' to
the EMACS_VERSION environment variable.

Note that if this is called in a batch run, output will go to
stderr.

Used in companion file `emake.mk'."
  (declare (emake-environment-variables "EMACS_VERSION"))
  (let ((major-minor (and (string-match (rx (+ digit) ?. (+ digit))
                                        emacs-version)
                          (match-string 0 emacs-version))))
    (when (version= major-minor (emake--getenv "EMACS_VERSION"))
      (message "ok"))))

(defun emake--message (format &rest args)
  "Print a message to standard out.
Argument FORMAT is a format string.  Optional argument ARGS is a
list of arguments for that format string."
  (princ
   (apply #'format
          (concat "\033[0;37memake:\033[0m " format "\n")
          args)))

(defmacro emake-task (description &rest body)
  "Wrapped by DESCRIPTION messages, run BODY."
  (declare (indent 1) (debug t))
  (let ((Sdescription (cl-gensym)))
    `(let ((,Sdescription (concat ,description "..."))
           (kill-emacs-hook kill-emacs-hook)) ; close this variable
       (push (lambda () (emake--message (concat ,Sdescription "done (emacs killed)")))
             kill-emacs-hook)
       (emake--message ,Sdescription)
       (unwind-protect
           (progn ,@body)
         (emake--message (concat ,Sdescription "done"))))))

;;; Package metadata

(defvar emake-package-desc
  (when-let ((package-file (emake--getenv "PACKAGE_FILE")))
    (when (file-readable-p package-file)
      (with-temp-buffer
        (insert-file-contents-literally package-file)
        (package-buffer-info))))
  "Package description corresponding to the code in PACKAGE_FILE.")

(defvar emake-package-reqs
  (when emake-package-desc
    (delq nil (mapcar (lambda (x)
                        (unless (eq (car x) 'emacs)
                          x))
                      (package-desc-reqs emake-package-desc))))
  "Non-emacs dependencies of PACKAGE_FILE.")

(defvar emake-project-root
  (when-let ((package-file (emake--getenv "PACKAGE_FILE")))
    (when (file-readable-p package-file)
      (locate-dominating-file (or (file-name-directory package-file)
                                  default-directory)
                              package-file)))
  "The folder `PACKAGE_FILE' is in.")

;;; Installing dependencies

(defvar emake-package-archive-master-alist
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("org"          . "http://orgmode.org/elpa/"))
  "Archive definition alist.
Key is the string name of the archive.
Value is the URL at which the archive is hosted.")

(defun emake--genform-with-elpa (dir archives-env body)
  "Generate a 'with-elpa' macro form.
DIR is the directory to use for `package-user-dir'.  ARCHIVES-ENV
is the environment variable used to determine the available
archives.  See also `emake-package-archive-master-alist'.  BODY
is the executable body of the macro."
  (let ((Sarchives (cl-gensym)))
    `(let* ((,Sarchives (emake--clean-list ,archives-env))
            (package-user-dir (expand-file-name ,dir emake-project-root))
            (package-archives (seq-filter (lambda (pair)
                                            (member (car pair) ,Sarchives))
                                          emake-package-archive-master-alist)))
       (emake-task "initializing package.el"
         (package-initialize))
       ,@body)))

(defmacro emake-with-elpa (&rest body)
  "Run BODY after setting up ELPA context."
  (declare (debug t))
  (emake--genform-with-elpa ".elpa" "PACKAGE_ARCHIVES" body))

(defmacro emake-with-elpa-test (&rest body)
  "Run BODY after setting up ELPA context."
  (declare (debug t))
  `(progn
     (unless (emake--getenv "PACKAGE_TEST_ARCHIVES")
       (setcdr (assoc-string "PACKAGE_TEST_ARCHIVES" emake--env-cache)
               (emake--getenv "PACKAGE_ARCHIVES")))
     ,(emake--genform-with-elpa ".elpa.test" "PACKAGE_TEST_ARCHIVES" body)))

(defun emake--package-download-archives (archives)
  "Download ARCHIVES if they do not exist locally.
ARCHIVES is a list of archives like `package-archives'."
  (let (empty-archives)
    (dolist (archive archives)
      ;; Determine all the archives that don't yet exist.
      ;; package.el uses a predictable format for this;
      ;; see `package-read-archive-contents'.
      (let ((archive-contents (format "%s/archives/%s/archive-contents"
                                      package-user-dir
                                      (car archive))))
        (if (file-exists-p archive-contents)
            (emake--message "Already downloaded `%s' to %s"
                            (car archive)
                            (file-relative-name archive-contents))
          (push archive empty-archives))))
    (when empty-archives
      (emake-task (format "[%s/] downloading archives: %S"
                          (file-relative-name package-user-dir)
                          empty-archives)
        (let ((package-archives empty-archives))
          (package-refresh-contents))))))

(defun emake--install (packages)
  "Ensure each package in PACKAGES is installed."
  (emake--package-download-archives package-archives)
  (dolist (package packages)
    (unless (package-installed-p package)
      (emake-task (format "installing %S" package)
        (package-install package)))))

;;; Running targets

(defun emake--resolve-target (target)
  "Resolve TARGET to a function."
  (let (candidates finder)
    ;; make sure `candidates' gets closed onto the right thing
    (setq finder (lambda (sym)
                   (lambda (f)
                     (when (string= target (function-get f sym))
                       (push f candidates)))))
    (mapatoms (funcall finder 'emake-target))
    (unless candidates
      (mapatoms (funcall finder 'emake-default-target)))
    (if (= 1 (length candidates))
        (car candidates)
      (error "Target unresolved: %s => %S" target candidates))))

(defun emake (target)
  "Search for a function matching TARGET and execute it.

The executed function is emake-my-TARGET if bound, else emake-TARGET."
  (let ((fun (emake--resolve-target target))
        (debug-on-error (member "debug" emake--debug-flags)))
    (emake-task (format (if command-line-args-left
                            "Running target %S with function `%S' with arguments %S"
                          "Running target %S with function `%S'")
                        target fun command-line-args-left)
      (when (member "show-environment" emake--debug-flags)
        (emake-task "showing relevant environment information"
          (mapc #'emake--var-message (function-get fun 'emake-environment-variables))))
      (apply fun (prog1 command-line-args-left
                   (setq command-line-args-left nil))))))

(defun emake--var-message (entry)
  "Show information for environment variable ENTRY."
  (let (var specific-description)
    (pcase entry
      (`(,v . ,d) (setq var v specific-description d))
      (`,v (setq var v)))
    (when (emake--getenv var)
      (emake--message "    %s=%S" var (emake--getenv var))
      (emake--message "      %s" (cdr (assoc-string var emake-environment-variables)))
      (when (stringp specific-description)
        (emake--message "      [target-specific] %s" specific-description)))))

(defmacro emake-with-options (args options &rest body)
  "With ARGS, determine and bind OPTIONS while executing BODY.
OPTIONS is a list of (CLI-OPT BINDING [TRUE-VALUE]) lists.  If
CLI-OPT is present in ARGS, then BINDING will be bound to
TRUE-VALUE during execution of BODY."
  (declare (indent 2))
  (let (bindings (Sargs (cl-gensym)))
    (dolist (option options)
      (unless (member (length option) '(2 3))
        (error "Wrong number of arguments in spec %S" option))
      (let ((opt (car option))
            (var (cadr option))
            (val (or (= 2 (length option)) ; if there is no default value, use t
                     (nth 2 option))))
        (unless (stringp opt)
          (error "Option must be a string literal: %S" opt))
        (unless (symbolp var)
          (error "Binding must be a symbol literal: %S" var))
        (push (list var `(if (member ,(concat "~" opt) ,Sargs) ,val ,var))
              bindings)))
    `(let ((,Sargs ,args))
       (let ,(nreverse bindings)
         ,@body))))

;;; Targets

(defun emake-install ()
  "Install dependencies.
Required packages include those that `PACKAGE_FILE' lists as
dependencies."
  (declare (emake-environment-variables
            "PACKAGE_IGNORE_DEPS"
            ("PACKAGE_ARCHIVES" . "used to install dependencies")
            ("PACKAGE_FILE" . "parsed to determine dependencies"))
           (emake-default-target "install"))
  (if emake-package-reqs
      (emake-with-elpa
       (emake-task (format "installing in %s" (file-relative-name package-user-dir))
         ;; install dependencies
         (emake--install
          (thread-last emake-package-reqs
            (mapcar #'car)
            (mapcar (lambda (p)
                      (unless (thread-last "PACKAGE_IGNORE_DEPS"
                                (emake--clean-list)
                                (mapcar #'intern )
                                (memq p))
                        p)))
            (delq nil)))))
    (emake--message "No dependencies detected")))

(defun emake-compile (&rest options)
  "Compile all files in PACKAGE_LISP.
Several OPTIONS are available:

`~error-on-warn': set `byte-compile-error-on-warn'"
  (declare (emake-environment-variables ("PACKAGE_LISP" . "these files are compiled"))
           (emake-default-target "compile"))
  (require 'bytecomp)
  (emake-with-options options
      (("error-on-warn" byte-compile-error-on-warn))
    (let (compile-buffer)
      (emake--message "error-on-warn => %S" byte-compile-error-on-warn)
      (emake-with-elpa
       (add-to-list 'load-path emake-project-root)
       (dolist (f (emake--clean-list "PACKAGE_LISP"))
         (emake-task (format "compiling %s" f)
           (byte-compile-file f)
           (when (and byte-compile-error-on-warn
                      (setq compile-buffer (get-buffer byte-compile-log-buffer)))
             ;; double-check; e.g. (let (hi)) won't error otherwise
             (with-current-buffer compile-buffer
               (goto-char (point-min))
               (when (re-search-forward "^.*:\\(Error\\|Warning\\): .*$" nil t)
                 (error "There were compile-time errors"))))))))))

(defun emake-help (target)
  "Get help for TARGET.
Uses the documentation string to get help for an EMake target.
If the target's behavior is driven by environment variables,
those will be reported as well."
  (declare (emake-default-target "help"))
  (let ((fn (emake--resolve-target target)))
    (emake-task (format "Documentation of %s (function %S)" target fn)
      (princ (documentation fn))
      (princ "\n\n----\n\nThis target uses the following environment variables:\n\n")
      (dolist (entry (function-get fn 'emake-environment-variables))
        (let (var desc)
          (if (consp entry)
              (setq var (car entry)
                    desc (cdr entry))
            (setq var entry))
          (princ (format "    %s\n      %s\n" var (cdr (assoc-string var emake-environment-variables))))
          (when desc
            (princ (format "      [target-specific] %s\n" desc)))))
      (princ "\n"))))

;;; Running tests

(defvar emake-test-runner-master-alist
  '(("buttercup"    . (progn (require 'buttercup)
                             'buttercup-run-discover))

    ("checkdoc"     . 'emake--test-helper-checkdoc)

    ("ert"          . (progn (require 'ert)
                             'ert-run-tests-batch-and-exit))

    ("package-lint" . 'emake--test-helper-package-lint))
  "Test-runner definition alist.
Key is the string name of the test-runner.  Value is a form that,
when evaluated, produces a defined function that will run all
defined tests and exit Emacs with code 0 if and only if all tests
pass.")

(defun emake-test (test-runner &rest args)
  "Prepare for and call TEST-RUNNER.
Argument TEST-RUNNER is a test-runner name in
`emake-test-runner-master-alist' or the name of a function that
runs the tests.

ARGS is bound to `command-line-args-left' while running
TEST-RUNNER.

Controlled by environment variables:

PACKAGE_TESTS is a list of files to `load' before running
TEST-RUNNER.

PACKAGE_TEST_DEPS is a list of packages to use as dependencies of
the test suite (not necessarily dependencies of the package being
tested).

PACKAGE_ARCHIVES is a list of archives to use; see
`emake-package-archive-master-alist'."
  (declare (emake-environment-variables
            "PACKAGE_TEST_DEPS"
            "PACKAGE_TEST_ARCHIVES"
            ("PACKAGE_TESTS" . "these files are loaded before the test-runner is called")
            "PACKAGE_ARCHIVES")
           (emake-default-target "test"))
  (when-let ((test-dependencies (emake--clean-list "PACKAGE_TEST_DEPS")))
    (emake-with-elpa-test
     (emake-task (format "installing test suite dependencies into %s"
                         (file-relative-name package-user-dir))
       (emake--install (mapcar #'intern test-dependencies)))))
  (let ((entry (assoc-string test-runner emake-test-runner-master-alist)))
    (cond
     (entry
      (setq test-runner (eval (cdr entry))))
     ((fboundp (intern test-runner))
      (setq test-runner (intern test-runner)))
     (t
      (error "%S test-runner not defined" test-runner))))
  (emake--message "Detected test-runner as `%S'" test-runner)

  (unless (functionp test-runner)
    (error "Test-runner not defined!"))

  (emake-with-elpa
   ;; Load any test definitions
   (when-let ((test-files (emake--clean-list "PACKAGE_TESTS")))
     (add-to-list 'load-path emake-project-root)
     (dolist (test-file test-files)
       (emake-task (format "loading test definitions in %s" test-file)
         (unless (file-readable-p test-file)
           (error "Cannot read file: %S" test-file))
         ;; load the file with tests
         (load test-file))))

   ;; run the tests and exit with an appropriate status
   (emake-task (format "running test `%S'" test-runner)
     (let ((command-line-args-left args))
       (funcall test-runner)))))

(defun emake--test-helper-checkdoc ()
  "Helper function for `checkdoc' test backend.
`checkdoc' apparently does not have a means to determine if a
given file has errors and return those errors as data (or even
just `error' out).  This function hopes to hack around this
limitation by throwing an error if the `*Warnings*' buffer
created by `checkdoc-file' is non-empty."
  (require 'checkdoc)
  (let ((guess-checkdoc-error-buffer-name "*Warnings*"))
    ;; This buffer name is hard-coded in checkdoc and it may change
    (ignore-errors
      (kill-buffer guess-checkdoc-error-buffer-name))
    (mapc (lambda (f)
            (emake-task (format "checking %s" f)
              (checkdoc-file f)))
          (emake--clean-list "PACKAGE_LISP"))
    (when-let ((buf (get-buffer guess-checkdoc-error-buffer-name)))
      (with-current-buffer buf
        (unless (= 0 (buffer-size))
          (error "Checkdoc issues detected"))))))

(declare-function package-lint-batch-and-exit "ext:package-lint.el")
(defun emake--test-helper-package-lint ()
  "Helper function for `package-lint' test backend."
  (require 'package-lint)
  (let ((command-line-args-left (emake--clean-list "PACKAGE_LISP")))
    (package-lint-batch-and-exit)))

(provide 'emake)
;;; emake.el ends here
