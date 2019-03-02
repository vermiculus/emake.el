;;; emake.el --- simple automated Elisp testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Sean Allred

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

(defun emake--declaration-help (fn _fn-args help-fn)
  "Note that the help-function for FN is HELP-FN."
  (function-put fn 'emake-help help-fn)
  nil)

(defun emake--declaration-default-test (fn _fn-args test-name)
  "Note that FN is the default handler for TEST-NAME."
  (function-put fn 'emake-default-test test-name)
  nil)

(defun emake--declaration-test (fn _fn-args test-name)
  "Note that FN is a handler for TEST-NAME."
  (function-put fn 'emake-test test-name)
  nil)

;; activate (declare ...) properties
(nconc defun-declarations-alist
       '((emake-environment-variables emake--declaration-environment-variables)
         (emake-default-target emake--declaration-default-target)
         (emake-target emake--declaration-target)
         (emake-help emake--declaration-help)
         (emake-default-test emake--declaration-default-test)
         (emake-test emake--declaration-test)))

(defvar emake-environment-variables
  '(("EMACS_VERSION" . "MAJOR.MINOR version of Emacs we intended to run under")
    ("EMAKE_DEBUG_FLAGS" . "have EMake print debugging information; see source for details")
    ("EMAKE_WORKDIR" . "directory of all files downloaded by Emake")
    ("EMAKE_LOGLEVEL" . "one of DEBUG, INFO, or NONE; controls verbosity of logging")
    ("EMAKE_USE_LOCAL" . "controls use of PACKAGE_ARCHIVES: \
ALWAYS prohibits installation of remote dependencies; \
NEVER forces install from the archives (i.e., never use local copies); \
any other value installs from archives when local copies are unavailable")
    ("PACKAGE_FILE" . "file containing the package definition")
    ("PACKAGE_TESTS" . "space-delimited list of Lisp files to load to define your tests")
    ("PACKAGE_LISP" . "space-delimited list of Lisp files in this package")
    ("PACKAGE_ARCHIVES" . "space-delimited list of named ELPA archives; see also `emake-package-archive-master-alist'")
    ("PACKAGE_TEST_DEPS" . "space-delimited list of packages needed by the test suite")
    ("PACKAGE_TEST_ARCHIVES" . "space-delimited list of named ELPA archives needed by the test suite; see also `emake-package-archive-master-alist'"))
  "List of environment variables used by EMake targets.")

;;; Utilities

(defun emake--flatten (lst)
  "Flatten the list LST such that it will contain no sublists."
  (cond ((null lst) nil)
        ((listp lst) (append (emake--flatten (car lst))
                             (emake--flatten (cdr lst))))
        (t (list lst))))

(cl-defun emake--dir-parent (dir &optional (how-many 1))
  "Find the parent of DIR.
With optional parameter HOW-MANY, find the grand-parent (=2) or
great-grand-parent (=3) or... of DIR."
  (cl-assert (>= how-many 0))
  (while (< 0 how-many)
    (cl-decf how-many)
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

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
  "Determines if the correct version of Emacs is being used.
Compares the MAJOR.MINOR versions of variable `emacs-version' to
the EMACS_VERSION environment variable."
  (declare (emake-environment-variables "EMACS_VERSION"))
  (let ((envver (emake--getenv "EMACS_VERSION")))
    (if (string= envver "snapshot")
        (progn (emake-message-info "Apparently running from snapshot (EMACS_VERSION=%S); cannot verify" envver)
               (emake-message-info "Reported version: %S" emacs-version)
               t)
      (emake-task (info "Verifying Emacs version")
        (let* ((major-minor (and (string-match (rx (+ digit) ?. (+ digit))
                                               emacs-version)
                                 (match-string 0 emacs-version)))
               (match (version= major-minor (string-trim envver))))
          (if match
              (emake-message-info "Emacs version %S verified" envver)
            (emake-message-info "Emacs version %S expected; but this `emacs-version' is %S"
                                envver
                                emacs-version))
          match)))))

(defun emake--message-internal (format &rest args)
  "Print a message to standard out.
Argument FORMAT is a format string.  Optional argument ARGS is a
list of arguments for that format string."
  (let ((s (apply #'format format args)))
    (princ (concat (if noninteractive "\033[0;37memake:\033[0m" "emake:") s "\n"))
    s))
(defun emake-message (format &rest args)
  "Print a message to standard out.
Argument FORMAT is a format string.  Optional argument ARGS is a
list of arguments for that format string."
  (apply #'emake--message-internal (concat " " format) args))

(defmacro emake-message-loglevel (fn tag &rest values)
  "Generate a messaging function.
FN is the name of the new function.  TAG is a string given to
note the level of the function.  VALUES is a list of values of
EMAKE_LOGLEVEL under which this message will print."
  (let ((Sloglevel (cl-gensym)))
    `(defun ,fn (format &rest args)
       ,(format "Print a message to standard out under certain circumstances.
Argument FORMAT is a format string.  Optional argument ARGS is a
list of arguments for that format string.

This message is only printed when environment variable
EMAKE_LOGLEVEL is one of the following values:

- %s" (string-join values "\n- "))
       (when-let ((,Sloglevel (emake--getenv "EMAKE_LOGLEVEL")))
         (when (member (upcase ,Sloglevel) ',values)
           (apply #'emake--message-internal
                  (format (if noninteractive "\033[32m%s:\033[0m %s" "%s: %s")
                          ,tag format)
                  args))))))
(emake-message-loglevel emake-message-debug "DEBUG" "DEBUG")
(emake-message-loglevel emake-message-info  "INFO"  "DEBUG" "INFO")

(defmacro emake-task (description &rest body)
  "Wrapped by DESCRIPTION messages, run BODY."
  (declare (indent 1) (debug t))
  (let ((Sdescription (cl-gensym))
        (printer (cond ((stringp description)
                        #'emake-message)
                       ((listp description)
                        (pcase (car description)
                          (`info  (setq description (cadr description))
                                  #'emake-message-info)
                          (`debug (setq description (cadr description))
                                  #'emake-message-debug)
                          (_ #'emake-message)))
                       (t (error "Unrecognized description format")))))
    `(let ((,Sdescription (concat ,description "..."))
           (kill-emacs-hook kill-emacs-hook)) ; close this variable
       (push (lambda () (,printer (concat ,Sdescription "done (emacs killed)")))
             kill-emacs-hook)
       (,printer ,Sdescription)
       (unwind-protect
           (progn ,@body)
         (,printer (concat ,Sdescription "done"))))))

;;; Developer locations

(defvar emake-package-dev-locations-functions
  '(emake-package-dev-locations-default)
  "Functions to find a package's PACKAGE_FILE and location on disk.
The package is given as a symbol as the first argument to each
function in succession and is expected to return a cons of the
form (PACKAGE_FILE . DIR) or nil.  If a function returns nil, the
next functions is attempted.  If all functions return nil, the
package and its dependencies are installed normally.

If any function does not return nil, DIR is added to the
`load-path' and PACKAGE_FILE is parsed for dependencies.  Note
that each dependency will then in turned be passed back into this
function to find its requirements.")

(defvar emake-package-file-subdirectories
  '((magit . "lisp")
    (transient . "lisp"))
  "An alist to determine the locatio of PKG-FILE.
Where will you find PKG-FILE for a given package?  This is an
alist from package-symbols to the directory (relative to the root
directory of the package) where the PKG-pkg.el or PKG.el file may
be located.  In essence, it answers the question 'Where will you
find PKG-FILE for a given package?'

This structure is used by `emake-package-dev-locations-default'.")

(defun emake-package-dev-locations-default (pkg)
  "Find PKG as a sibling of this package.
See `emake-project-root' and
`emake-package-dev-locations-functions'."
  (let ((dir-parent (expand-file-name (symbol-name pkg)
                                      (emake--dir-parent emake-project-root))))
    (when (file-directory-p dir-parent)
      (let ((pkg-file-dir (if-let ((entry (assoc pkg emake-package-file-subdirectories)))
                              (expand-file-name (cdr entry) dir-parent)
                            dir-parent)))
        (cl-some (lambda (pkg-pattern)
                   (let ((pkg-file (expand-file-name (format pkg-pattern pkg) pkg-file-dir)))
                     (when (file-readable-p pkg-file)
                       (cons pkg-file (file-name-as-directory dir-parent)))))
                 (list "%S-pkg.el" "%S.el"))))))

(defun emake--package-dev-location (package)
  "Determine the location of PACKAGE on this machine.
If the return value is nil, the package must be installed through
an ELPA."
  (declare (emake-environment-variables "EMAKE_USE_LOCAL"))
  (let* ((local (upcase (or (emake--getenv "EMAKE_USE_LOCAL") "AS_AVAILABLE")))
         (location (unless (string= local "NEVER")
                     (cl-some (lambda (pkg-finder)
                                (when-let ((result (funcall pkg-finder package)))
                                  (emake-message-debug "Found %S with %S" package pkg-finder)
                                  result))
                              emake-package-dev-locations-functions))))
    (unless location
      (if (string= local "ALWAYS")
          (error "Package not found locally: %S" package)
        (emake-message-debug "Package not found locally and will use archives: %S" package)))
    location))

;;; Dependencies

(defun emake-package-reqs ()
  "Non-emacs dependencies of PACKAGE_FILE."
  (when-let ((reqs (emake-package-reqs--recursive (emake--getenv "PACKAGE_FILE"))))
    (let (with-load-path)
      (dolist (pkg reqs)
        (when-let ((pair (emake--package-dev-location pkg)))
          (cl-pushnew (cons pkg (cdr pair)) with-load-path)))
      (cons reqs with-load-path))))

(defun emake-package-reqs--recursive (root-package-file)
  "For ROOT-PACKAGE-FILE, find all dependencies.
This includes dependencies of dependencies for dependencies whose
dependencies are known.

Yes, that *was* fun to write :-)

In layman's terms: if we are developing a dependency elsewhere on
this machine and it's detectable by one of the functions used by
`emake--package-dev-location', then the dependencies of that
dependency are added so that all dependencies are still met."
  (let ((deps (emake-package-reqs--single root-package-file)))
    (cl-delete-duplicates
     (emake--flatten
      (mapcar (lambda (d)
                (cons d (when-let ((d-file (car-safe (emake--package-dev-location d))))
                          (emake-package-reqs--recursive d-file))))
              (delq 'emacs deps))))))

(defun emake-package-reqs--single (package-file)
  "Get the direct dependencies of PACKAGE-FILE."
  (mapcar #'car
          (if-let ((desc (emake-package-desc package-file)))
              (package-desc-reqs desc)
            (require 'lisp-mnt)
            (with-temp-buffer
              (insert-file-contents-literally package-file)
              (when-let ((header (lm-header "package-requires")))
                (package--prepare-dependencies
                 (package-read-from-string header)))))))

(defun emake-package-desc (package-file)
  "Get a `package-desc' object from PACKAGE-FILE if possible."
  (or (emake-package-desc--define-package package-file)
      (ignore-errors
        ;; this will fail if the file does not have a Version header
        (emake-package-desc--headers package-file))))

(defun emake-package-desc--define-package (package-file)
  "Get a `package-desc' from PACKAGE-FILE using `define-package'."
  (package-process-define-package
   (with-temp-buffer
     (insert-file-contents package-file)
     (goto-char (point-min))
     (read (current-buffer)))))

(defun emake-package-desc--headers (package-file)
  "Get a `package-desc' from PACKAGE-FILE using headers."
  (with-temp-buffer
    (insert-file-contents-literally package-file)
    (package-buffer-info)))

(defvar emake-project-root
  (when-let ((package-file (emake--getenv "PACKAGE_FILE")))
    (let ((root (locate-dominating-file (or (file-name-directory package-file)
                                            default-directory)
                                        package-file)))
      (push (cons "PACKAGE_FILE" (expand-file-name package-file root))
            emake--env-cache)
      root))
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
            (package-user-dir (expand-file-name
                               (concat (file-name-as-directory (emake--getenv "EMAKE_WORKDIR")) ,dir)
                               emake-project-root))
            (package-archives (seq-filter (lambda (pair)
                                            (member (car pair) ,Sarchives))
                                          emake-package-archive-master-alist)))
       (emake-task (debug "Initializing package.el")
         (package-initialize))
       (dolist (pair (cdr (emake-package-reqs)))
         (emake-message-info "Using dependency `%S' at %s" (car pair) (cdr pair))
         (add-to-list 'load-path (cdr pair)))
       ,@body)))

(defmacro emake-with-elpa (&rest body)
  "Run BODY after setting up ELPA context."
  (declare (debug t))
  (emake--genform-with-elpa "elpa" "PACKAGE_ARCHIVES" body))

(defmacro emake-with-elpa-test (&rest body)
  "Run BODY after setting up ELPA context."
  (declare (debug t))
  `(progn
     (unless (emake--getenv "PACKAGE_TEST_ARCHIVES")
       (setcdr (assoc-string "PACKAGE_TEST_ARCHIVES" emake--env-cache)
               (emake--getenv "PACKAGE_ARCHIVES")))
     ,(emake--genform-with-elpa "elpa-test" "PACKAGE_TEST_ARCHIVES" body)))

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
            (emake-message-debug "Already downloaded `%s' to %s"
                                 (car archive)
                                 (file-relative-name archive-contents))
          (push archive empty-archives))))
    (when empty-archives
      (emake-task (info (format "Downloading archives to `%s/': %S"
                                (file-relative-name package-user-dir)
                                empty-archives))
        (let ((package-archives empty-archives))
          (package-refresh-contents))))))

(defun emake--install (dependencies-spec)
  "Ensure each package in DEPENDENCIES-SPEC is installed.
DEPENDENCIES-SPEC is a list of the form

  \(ALL-DEPS DEV-DEP-SPEC DEV-DEP-SPEC...)

where each DEV-DEP-SPEC is a cons of the package-symbol and the
directory to be added to `load-path'."
  (emake--package-download-archives package-archives)
  (dolist (dep (car dependencies-spec))
    (if-let ((entry (assq dep (cdr dependencies-spec))))
        (emake-message-debug "Skipping dev package: %S" entry)
      (if (package-installed-p dep)
          (emake-message-debug "Already installed: %S" dep)
        (emake-task (info (format "Installing %S" dep))
          (package-install dep))))))

;;; Running targets

(defun emake--targets ()
  "Return a hash table of all defined targets."
  (let ((dict (make-hash-table :test 'equal))
        target-fns)
    ;; Narrow our search space
    (mapatoms
     (lambda (sym)
       (when (or (function-get sym 'emake-default-target)
                 (function-get sym 'emake-target))
         (push sym target-fns))))
    ;; Search for custom targets first
    (mapc
     (lambda (sym)
       (when-let ((target (function-get sym 'emake-target)))
         (cl-assert (not (gethash target dict)) nil
                    (format "Shadowed target: %s (%S vs. %S) [%S]"
                            target sym (gethash target dict) target-fns))
         (puthash target sym dict)))
     target-fns)
    ;; Then fill in the default targets where there are holes
    (mapc
     (lambda (sym)
       (when-let ((default-target (function-get sym 'emake-default-target)))
         (unless (gethash default-target dict)
           (puthash default-target sym dict))))
     target-fns)
    dict))

(defun emake--resolve-target (target)
  "Resolve TARGET to a function."
  (or (gethash target (emake--targets))
      (error "Target unresolved: %s" target)))

(defun emake (target)
  "Search for a function matching TARGET and execute it.

The executed function is emake-my-TARGET if bound, else emake-TARGET."
  (let ((fun (emake--resolve-target target))
        (debug-on-error
         ;; this variable must eq t
         (and (member "debug" emake--debug-flags) t)))
    (cl-assert (emake-verify-version))
    (emake-task (info (format (if command-line-args-left
                                  "Running target %S with function `%S' with arguments %S"
                                "Running target %S with function `%S'")
                              target fun command-line-args-left))
      (when-let ((behavior (cond
                            ((member "show-environment" emake--debug-flags) t)
                            ((member "show-environment:non-nil" emake--debug-flags) 'only))))
        (emake-task (info "Showing relevant environment information")
          (emake--env-help fun behavior)))
      (apply fun (prog1 command-line-args-left
                   (setq command-line-args-left nil))))))

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
            ("PACKAGE_ARCHIVES" . "used to install dependencies")
            ("PACKAGE_FILE" . "parsed to determine dependencies"))
           (emake-default-target "install"))
  (if-let ((reqs (emake-package-reqs)))
      (emake-with-elpa
       (emake-task (info (format "Installing in %s" (file-relative-name package-user-dir)))
         (emake-message-debug "Dependencies: %S" reqs)
         (emake-message-debug "Development package locations functions: %S" emake-package-dev-locations-functions)
         (emake--install reqs)))
    (emake-message-debug "No dependencies detected")))

(defun emake-compile (&rest options)
  "Compile all files in PACKAGE_LISP.
Several OPTIONS are available:

`~error-on-warn': set `byte-compile-error-on-warn'"
  (declare (emake-environment-variables ("PACKAGE_LISP" . "these are the files that are compiled"))
           (emake-default-target "compile"))
  (require 'bytecomp)
  (emake-with-options options
      (("error-on-warn" byte-compile-error-on-warn))
    (let (compile-buffer)
      (emake-message-debug "  error-on-warn => %S" byte-compile-error-on-warn)
      (emake-with-elpa
       (add-to-list 'load-path emake-project-root)
       (dolist (f (emake--clean-list "PACKAGE_LISP"))
         (emake-task (info (format "Compiling %s" f))
           (byte-compile-file f)
           (when (and byte-compile-error-on-warn
                      (setq compile-buffer (get-buffer byte-compile-log-buffer)))
             ;; double-check; e.g. (let (hi)) won't error otherwise
             (with-current-buffer compile-buffer
               (goto-char (point-min))
               (when (re-search-forward "^.*:\\(Error\\|Warning\\): .*$" nil t)
                 (error "There were compile-time errors"))))))))))

(defun emake-setup-load-path ()
  "Load the testing environment.
This is intended to be run without the `--batch' invocation flag."
  (declare (emake-default-target "setup-load-path"))
  (emake-with-elpa
   ;; `emake-with-elpa' ensures that `load-path' is setup correctly.
   (ignore)))

(defun emake-autoloads ()
  "Generate autoloads in `emake-project-root'.
Uses `package-generate-autoloads' internally after determining
appropriate arguments to that function.."
  (declare (emake-default-target "autoloads")
           (emake-environment-variables "PACKAGE_FILE"
                                        "PACKAGE_LISP"))
  (let* ((pkg-file (emake--getenv "PACKAGE_FILE"))
         (desc (emake-package-desc pkg-file))
         (pkg-dir (expand-file-name
                   (or (ignore-errors (file-name-directory
                                       (car (emake--clean-list "PACKAGE_LISP"))))
                       emake-project-root)))
         (name (if desc
                   (symbol-name (package-desc-name desc))
                 (string-remove-suffix "-pkg" (file-name-base pkg-file))))
         (relative-path (string-remove-prefix
                         (expand-file-name emake-project-root)
                         (expand-file-name pkg-dir))))
    (unless desc
      (emake-message-info "Guessed package name: %S" name))
    (emake-task (info (if (string= relative-path "")
                          (format "Generating autoloads for %S" name)
                        (format "Generating autoloads for %S in %S" name relative-path)))
      (package-generate-autoloads name pkg-dir))))

(defun emake-help (&optional target)
  "Get help for TARGET or summarize all defined targets.
Uses the documentation string to get help for an EMake target.
If the target's behavior is driven by environment variables,
those will be reported as well.

If TARGET is nil, then all defined targets are presented along
with the first line of their documentation string."
  (declare (emake-default-target "help"))
  (if target
      (let ((fn (emake--resolve-target target)))
        (emake-task (format "Help for target `%s' (function %S)" target fn)
          (princ (documentation fn))
          (princ "\n"))
        (emake-task "Environment variables used"
          (emake--env-help fn t))
        (when-let ((fn-help (function-get fn 'emake-help)))
          (emake-task "Target-specific help"
            (funcall fn-help))))
    (emake-task "Summarizing EMake targets"
      (maphash
       (lambda (target fn)
         (princ (format "    %s: %s\n" target (car (split-string (documentation fn) "\n")))))
       (emake--targets)))))

(defun emake--env-help (fn &optional with-value)
  "Show help for function FN.
If WITH-VALUE is non-nil, values for the environment variables
will also be reported.  If it is the special value `only', then
help for ENTRY will only display if the environment variable has
a non-nil value according to `emake--getenv'."
  (dolist (entry (function-get fn 'emake-environment-variables))
    (let (var val doc this-doc)
      (cond
       ((stringp entry)
        (setq var entry))
       ((consp entry)
        (setq var      (car entry)
              this-doc (cdr entry))))
      (setq doc (cdr (assoc-string var emake-environment-variables)))
      (when with-value
        (setq val (emake--getenv var)))
      (unless (and (eq with-value 'only) (null val))
        (if with-value
            (princ (format "    %s=%S\n" var val))
          (princ (format "    %s\n" var)))
        (princ (format "      %s\n" doc))
        (when this-doc
          (princ (format "      * %s\n" this-doc)))))))

;;; Running tests

(defun emake-test (test-runner &rest args)
  "Prepare for and call TEST-RUNNER.
Argument TEST-RUNNER is a test-runner name in
`emake-test-runner-master-alist' or the name of a function that
runs the tests.

ARGS is bound to `command-line-args-left' while running
TEST-RUNNER."
  (declare (emake-environment-variables
            ("PACKAGE_TEST_DEPS" . "dependencies of test suite")
            ("PACKAGE_TEST_ARCHIVES" . "archives to use to install test suite dependencies")
            ("PACKAGE_TESTS" . "these files are loaded before the test-runner is called")
            ("PACKAGE_ARCHIVES" . "archives to use to install package dependencies"))
           (emake-help emake--test-help)
           (emake-default-target "test"))
  (interactive (list (completing-read "Run test in this session: "
                                      (mapcar
                                       (lambda (s)
                                         (or (function-get s 'emake-test)
                                             (function-get s 'emake-default-test)))
                                       (emake--get-syms-with-props 'emake-default-test 'emake-test)))))
  (when-let ((test-dependencies (emake--clean-list "PACKAGE_TEST_DEPS")))
    (emake-with-elpa-test
     (emake-task (info (format "Installing test suite dependencies into %s"
                               (file-relative-name package-user-dir)))
       (emake--install (list (mapcar #'intern test-dependencies))))))
  (let* ((tests (seq-filter (lambda (s)
                              (or (string= test-runner (function-get s 'emake-test))
                                  (string= test-runner (function-get s 'emake-default-test))))
                            (emake--get-syms-with-props 'emake-default-test 'emake-test)))
         (entry (if (<= 2 (length tests))
                    (error "Ambgiuous test-runner %S: %S" test-runner tests)
                  (car tests))))
    (cond
     (entry
      (setq test-runner entry))
     ((fboundp (intern test-runner))
      (setq test-runner (intern test-runner)))
     (t
      (error "%S test-runner not defined" test-runner))))
  (emake-message-debug "Detected test-runner as `%S'" test-runner)

  (unless (functionp test-runner)
    (error "Test-runner not defined!"))

  (emake-with-elpa
   ;; Load any test definitions
   (when-let ((test-files (emake--clean-list "PACKAGE_TESTS")))
     (add-to-list 'load-path emake-project-root)
     (dolist (test-file test-files)
       (emake-task (debug (format "Loading test definitions in %s" test-file))
         (unless (file-readable-p test-file)
           (error "Cannot read file: %S" test-file))
         ;; load the file with tests
         (load test-file))))

   ;; run the tests and exit with an appropriate status
   (emake-task (info (format "Running test `%S'" test-runner))
     (let ((command-line-args-left args))
       (funcall test-runner)))))

(defun emake--test-help ()
  "Print help for all test-runners."
  (let ((funcs (emake--get-syms-with-props 'emake-default-test 'emake-test))
        all)
    (dolist (fn funcs)
      (push (cons fn (or (function-get fn 'emake-default-test)
                         (function-get fn 'emake-test)))
            all))
    (pcase-dolist (`(,fn . ,test) (sort all (lambda (a b) (string< (cdr a) (cdr b)))))
      (emake-task (format "Help for test `%s' (function %S)" test fn)
        (princ (documentation fn))
        (princ "\n")
        (emake-task "Environment variables used"
          (emake--env-help fn t))))))

(defun emake--get-syms-with-props (&rest props)
  "Get all symbols with a value for at least one of PROPS."
  (let (syms)
    (mapatoms (lambda (s)
                (when (cl-find-if (lambda (p) (function-get s p)) props)
                  (push s syms))))
    syms))

(defun emake--with-args-from-env (list-env fn)
  "Use LIST-ENV to populate `command-line-args-left' for FN.
The environment's value for LIST-ENV is used to simulate
arguments for a function, FN, that expects them from the command
line."
  (let ((command-line-args-left (emake--clean-list list-env)))
    (emake-message-debug "Simulating command-line arguments: %S" command-line-args-left)
    (funcall fn)))

(defun emake--test-helper-ert ()
  "Run `ert-run-tests-batch-and-exit'."
  (declare (emake-default-test "ert"))
  (ert-run-tests-batch-and-exit))

(declare-function buttercup-run-discover "ext:buttercup.el" ())
(defun emake--test-helper-buttercup ()
  "Run Buttercup test suite with `buttercup-run-discover'."
  (declare (emake-default-test "buttercup"))
  (require 'buttercup)
  (buttercup-run-discover))

(defun emake--test-helper-checkdoc ()
  "Check documentation for style issues."
  (declare (emake-default-test "checkdoc")
           (emake-environment-variables ("PACKAGE_LISP" . "these files are checked")))
  (require 'checkdoc)
  ;; `checkdoc' apparently does not have a means to determine if a
  ;; given file has errors and return those errors as data (or even
  ;; just `error' out).  This function hopes to hack around this
  ;; limitation by throwing an error if the `*Warnings*' buffer
  ;; created by `checkdoc-file' is non-empty.
  (let ((guess-checkdoc-error-buffer-name "*Warnings*"))
    ;; This buffer name is hard-coded in checkdoc and it may change
    (ignore-errors
      (kill-buffer guess-checkdoc-error-buffer-name))
    (mapc (lambda (f)
            (emake-task (info (format "Checking %s" f))
              (checkdoc-file f)))
          (emake--clean-list "PACKAGE_LISP"))
    (when-let ((buf (get-buffer guess-checkdoc-error-buffer-name)))
      (with-current-buffer buf
        (unless (= 0 (buffer-size))
          (error "Checkdoc issues detected"))))))

(declare-function package-lint-batch-and-exit "ext:package-lint.el")
(defun emake--test-helper-package-lint ()
  "Helper function for `package-lint' test backend."
  (declare (emake-default-test "package-lint")
           (emake-environment-variables ("PACKAGE_FILE" . "this is the only file that will be linted; see purcell/package-lint#111")))
  (require 'package-lint)
  (emake--with-args-from-env "PACKAGE_FILE" #'package-lint-batch-and-exit))

(provide 'emake)
;;; emake.el ends here
