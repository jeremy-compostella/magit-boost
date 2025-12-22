;;; magit-boost.el --- Boost Magit performance on slow network. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: November 2025
;; Keywords: extensions magit
;; Homepage: https://github.com/jeremy-compostella/magit-bash
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (magit "4.4.0"))

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
;;
;; Magit Boost enhances the speed of Git command execution by leveraging a
;; persistent bash process.  This package initializes and manages dedicated
;; Boost buffers, intercepts Git commands from Magit, and executes them using
;; the running bash process.  In my experience, this results in a 2-4x
;; performance boost for commands like `magit-status' and `magit-log'.

(require 'cl-seq)
(require 'magit)
(require 'tramp)

(defcustom magit-boost-buffer-name "*magit-boost*"
  "Name of the magit bash buffer."
  :type 'string)

(defcustom magit-boost-debug nil
  "If non-nil, enable debug messages for Magit Boost."
  :type 'boolean)

(defvar-local magit-boost-git-dir nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

(defvar-local magit-boost-git-dir-truename nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

(defvar-local magit-boost-git-tree-files '())

(defun magit-boost-filter (process string)
  "Process filter for Magit Boost buffers.

Append STRING, the output from PROCESS, to the end of the buffer
associated with PROCESS. This function is used to collect and display
output from the Bash process in the Magit Boost buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)))

(defun magit-boost-buffers ()
  "Return a list of all Magit Boost buffers."
  (seq-filter (lambda (b)
		(string-prefix-p magit-boost-buffer-name (buffer-name b)))
	      (buffer-list)))

(defvar-local magit-boost-connection-type nil
  "Buffer-local variable specifying the connection type for the Magit Bash
process.

This variable determines how the Bash process is started in the current
buffer.  Its value should be either 'pty (for pseudo-terminal) or
nil (for pipe), depending on the requirements of the Git command being
executed and the contents of the buffer.

- If set to 'pty, the Bash process uses a pseudo-terminal, which is generally
required for interactive commands and for proper handling of Magit output.

- If set to nil, the Bash process uses a pipe, which may be necessary for buffers
containing carriage return characters (\\r), as pty processes can have issues
with such input.

This variable is automatically set when creating a new Magit Boost buffer via
`magit-boost-new-buffer', and is used to ensure the correct process connection
type is chosen for each Git command execution.")

(defun magit-boost-git-dir (dir)
  (with-temp-buffer
    (setq default-directory dir)
    (if (= (process-file "git" nil t nil "rev-parse" "--git-dir") 0)
	(string-trim (buffer-string))
      (error "Failed to read the git tree"))))

(defun magit-boost-new-buffer (dir connection-type)
  "Create and return a new Magit Boost buffer for DIR with
CONNECTION-TYPE."
  (setf dir (with-temp-buffer
	      (setq default-directory dir)
	      (with-parsed-tramp-file-name (expand-file-name dir) nil
		(if (= (process-file "git" nil t nil "rev-parse" "--show-toplevel") 0)
		    (tramp-make-tramp-file-name v (string-trim (buffer-string)))))))
  (if dir
    (let* ((buf-name (format magit-boost-buffer-name))
	   (buffer (generate-new-buffer buf-name)))
      (uniquify-rationalize-file-buffer-names buf-name default-directory buffer)
      (with-current-buffer buffer
	(let ((git-dir (magit-boost-git-dir default-directory)))
	  (setq default-directory (concat dir "/")
		magit-boost-git-dir git-dir
		magit-boost-git-tree-files (list (concat default-directory git-dir))
		magit-boost-git-dir-truename (file-truename git-dir)
		magit-boost-connection-type connection-type)))
      buffer)
    (error "No git repository found")))

(defun magit-boost-in-git-dir (filename)
  (when magit-boost-git-dir
    (let ((git-dir (concat default-directory magit-boost-git-dir)))
      (or (string-prefix-p git-dir filename)
	  (string-prefix-p magit-boost-git-dir-truename filename)))))

(defun magit-boost-buffer (filename connection-type)
  (cl-flet ((is-buffer-for (connection-type filename buffer)
	      (with-current-buffer buffer
		(and (eq magit-boost-connection-type connection-type)
		     (or (string-prefix-p default-directory filename)
			 (magit-boost-in-git-dir filename))))))
    (cl-find filename (magit-boost-buffers)
	     :test (apply-partially #'is-buffer-for connection-type))))

(defmacro with-magit-boost-buffer (directory connection-type &rest body)
  (declare (indent 2))
  (let ((buffer (gensym "buffer")))
    `(progn
       (setf ,buffer (magit-boost-buffer ,directory ,connection-type))
       (when ,buffer
	 (with-current-buffer ,buffer
	   (progn ,@body))))))

(defun magit-boost-buffer-create (dir connection-type)
  "Return a Magit Boost buffer associated with DIR, creating one if necessary."
  (let ((buffer (or (magit-boost-buffer dir connection-type)
		 (magit-boost-new-buffer dir connection-type))))
    (unless (get-buffer-process buffer)
      (let* ((process-connection-type connection-type)
	     (process (start-file-process "magit-boost-process" buffer "bash")))
	(process-send-string process "export PS1=''\n")
	(accept-process-output process 1 nil t)
	(set-process-filter process 'magit-boost-filter)))
    buffer))

(defmacro with-magit-boost-buffer-create (directory connection-type &rest body)
  (declare (indent 2))
  `(with-current-buffer (magit-boost-buffer-create ,directory ,connection-type)
     (progn ,@body)))

(defun magit-boost-process-cmd (cmd input destination)
  "Execute a shell command CMD."
  (let ((connection-type (unless (and input (string-search "\r" input))
			   'pty))
	(dir (with-parsed-tramp-file-name default-directory c
	       c-localname))
	(done-magic "MAGIT_BOOST_DONE")
	(stderr-magic "MAGIT_BOOST_STDERR")
	(stderr-local "/tmp/magit-boost-stderr")
	stdout ret stderr-dest)
    (with-magit-boost-buffer-create default-directory connection-type
      (cl-macrolet ((cappend (&rest args)
		      `(setf cmd (apply #'concat cmd (list ,@args))))
		    (cprepend (&rest args)
		      `(setf cmd (apply #'concat ,@args (list cmd)))))
	(when input
	  (cprepend "echo '" (replace-regexp-in-string "'" "'\"'\"'" input)
		    "' | "))
	(when (and (listp destination) (stringp (cadr destination)))
	  (setf stderr-dest (cadr destination))
	  (cappend " 2>'" stderr-local "'"))
	(cappend "; export RET=$?")
	(when stderr-dest
	  (cappend "; echo -n " stderr-magic
		   "; if [ -e '" stderr-local "' ]"
		   "; then cat '" stderr-local "'; fi"))
	(cprepend "cd " dir ";")
	(cappend "; echo " done-magic " $RET; cd - > /dev/null\n"))
      (when magit-boost-debug
	(save-excursion
	  (goto-char (point-max))
	  (insert cmd)))
      (let ((process (get-buffer-process (current-buffer)))
	    (start (point-max))
	    (regexp (concat done-magic " \\([0-9]+\\)")))
	(process-send-string process cmd)
	(while (not ret)
	  (accept-process-output process 1 nil t)
	  (save-excursion
	    (goto-char (point-max))
	    (when (re-search-backward regexp start t)
	      (setf stdout (buffer-substring start (match-beginning 0))
		    ret (string-to-number (match-string 1)))))))
      (when stderr-dest
	(let ((split (split-string stdout stderr-magic)))
	  (setf stdout (car split))
	  (let ((err (cadr split)))
	    (unless (string-empty-p err)
	      (write-region err nil stderr-dest)))))
      (unless magit-boost-debug
	(erase-buffer)))
    (when (and (= ret 0) destination)
      (insert stdout))
    ret))

(defun magit-boost--process-git (destination args &optional input)
  "Execute a Git command via the Magit Bash process."
  (let ((git-args (flatten-list args))
	fun)
    (if (and (stringp (car git-args))
	     (setf fun (intern (concat "magit-boost-" (car git-args)
				       (cadr git-args))))
	     (functionp fun))
	(funcall fun default-directory)
      (cl-flet* ((escape-quotes (str)
		   (replace-regexp-in-string "\"" "\\\\\"" str))
		 (format-arg (arg)
		   (format "\"%s\"" (escape-quotes arg))))
	(let ((cmd (mapconcat #'format-arg
			      (magit-process-git-arguments args)
			      " ")))
	(magit-boost-process-cmd (concat "git " cmd) input destination))))))

(defun magit-boost-load-files-attributes (files)
  "Batch load and cache Tramp file attributes for FILES.

This function constructs a single shell command to retrieve file
existence, readability, writability, directory status, symlink status,
and truenames for all provided FILES.  The results are parsed and
injected directly into the Tramp property cache.

This significantly improves performance on slow networks by replacing
multiple synchronous remote calls with a single batch execution."
  (let* ((test-and-props '(("-e" . "file-exists-p")
			   ("-r" . "file-readable-p")
			   ("-w" . "file-writable-p")
			   ("-d" . "file-directory-p")
			   ("-L" . "file-symlink-p")))
	 (tests (mapconcat (lambda (tp)
			     (format "[ %s \"$file\" ] && echo -n ' %s'"
				     (car tp) (cdr tp)))
			   test-and-props " ; "))
	 (truename "[ -e \"$file\" ] && echo '' && readlink -nf \"$file\"")
	 (attributes (format "[ -e \"$file\" ] && echo '' && %s"
			     (with-parsed-tramp-file-name default-directory nil
			       (replace-regexp-in-string
				"\$1" "$file"
				(tramp-expand-script v tramp-stat-file-attributes)))))
	 (cmd (concat "for file in "
		      (mapconcat (lambda (file)
				   (format "'%s' "
					   (tramp-file-name-localname
					    (tramp-dissect-file-name file))))
				 files " ")
		      "; do " tests " ; " truename " ; " attributes
		      "; echo ''"
		      "; done")))
    (with-temp-buffer
      (let ((ret (magit-boost-process-cmd cmd nil t)))
	(unless (= ret 0)
	  (error "Failed to read files attributes")))
      (goto-char (point-min))
      (dolist (file files)
	(let ((res (split-string (buffer-substring
				  (line-beginning-position) (line-end-position)))))
	  (dolist (tp test-and-props)
	    (let ((localname (tramp-file-name-localname (tramp-dissect-file-name file))))
	      (with-parsed-tramp-file-name file nil
		(tramp-set-file-property
		 v localname (cdr tp) (if (member (cdr tp) res) t nil)))))
	  (with-parsed-tramp-file-name file nil
	    (when (member "file-exists-p" res)
	      (forward-line)
	      (tramp-set-file-property
	       v localname "file-truename"
	       (buffer-substring (line-beginning-position) (line-end-position)))
	      (forward-line)
	      (cl-letf (((symbol-function 'tramp-get-file-property)
			 (lambda (key file property &optional default)
			   default)))
		(tramp-convert-file-attributes v localname 'integer
		  (read (buffer-substring (line-beginning-position)
					  (line-end-position)))))
	      (forward-line)))
	  (forward-line))))))

(defun magit-boost-get-file-property (orig-fun &rest args)
  (let* ((key (car args))
	 (localname (cadr args))
	 (filename (tramp-make-tramp-file-name key localname)))
    (with-magit-boost-buffer filename 'pty
      (when (magit-boost-in-git-dir filename)
	(let ((property (caddr args))
	      (default (cadddr args)))
	  (add-to-list 'magit-boost-git-tree-files filename)
	  (let ((value (funcall orig-fun key localname property tramp-cache-undefined)))
	    (when (eq value tramp-cache-undefined)
	      (magit-boost-load-files-attributes magit-boost-git-tree-files))))))
    (apply orig-fun args)))

(defun magit-boost-rev-parse--show-cdup (dir)
  (when-let ((cdup (with-magit-boost-buffer dir 'pty
		     (if (magit-boost-in-git-dir dir)
			 ""
		       (let ((sub (substring dir (length default-directory))))
			 (if (string-empty-p sub)
			     ""
			   (mapconcat (lambda (_) "..")
				      (cl-delete "" (split-string sub "/")) "/")))))))
    (insert cdup "\n")
    0))

(defun magit-boost-rev-parse--show-toplevel (dir)
  (when-let* ((root (with-magit-boost-buffer-create dir 'pty
		      default-directory)))
    (with-parsed-tramp-file-name root r
      (insert r-localname)
      0)))

(defun magit-boost-rev-parse--git-dir (dir)
  (insert (with-magit-boost-buffer dir 'pty
	    magit-boost-git-dir))
  0)

(defun magit-boost-process-git (orig-fun &rest args)
  "Advice for `magit-process-git' to optionally route Git commands through
a persistent Bash process.

If the current buffer's `default-directory' is a TRAMP (remote), execute
the Git command using Magit Boost's accelerated Bash process, passing
DESTINATION (the first element of ARGS) and the prepared Git arguments."
  (if (tramp-tramp-file-p default-directory)
      (magit-boost--process-git (car args) (cdr args))
    (apply orig-fun args)))

(defun magit-boost-run-git-with-input (orig-fun &rest args)
  "Advice for `magit-run-git-with-input' to accelerate Git commands via a
persistent Bash process.

If the current buffer is not a TRAMP (remote) buffer, call the original
function `orig-fun' with ARGS.

Otherwise, execute the Git command using the Magit Bash process, piping
the current buffer's contents as input to the command. This reduces
process startup overhead and improves performance for repeated Git
operations."
  (if (tramp-tramp-file-p default-directory)
      (magit-boost--process-git t args (buffer-string))
    (apply orig-fun args)))

(defun magit-boost--vc-responsible-backend (orig-fun &rest args)
  "If a Magit Boost buffer with a 'pty connection exists, return Git.

This avoids invoking the slower default implementation of
`vc-responsible-backend', which can be a performance bottleneck in Magit."
  (if (magit-boost-buffer (car args) 'pty)
      "Git"
    (apply orig-fun args)))

(define-minor-mode magit-boost-mode
  "Minor mode to accelerate Magit Git commands by routing them through a
persistent Bash process.

When enabled, `magit-boost-mode' intercepts Magit Git commands and
executes them using a dedicated Bash process, reducing process startup
overhead and improving performance, especially for repeated commands.

This mode works by advising `magit-process-git' and
`magit-run-git-with-input' to use the Bash process.  When disabled, the
original Magit behavior is restored.

Enable this mode globally to benefit from faster Magit operations in all
buffers.

Note: This mode may not be compatible with remote (TRAMP) buffers."
  :init-value nil
  :global t
  :lighter " MB"
  (if magit-boost-mode
      (progn
	(advice-add 'magit-process-git
		    :around #'magit-boost-process-git)
	(advice-add 'magit-run-git-with-input
		    :around #'magit-boost-run-git-with-input)
	(advice-add 'vc-responsible-backend
		    :around #'magit-boost-vc-responsible-backend)
	(advice-add 'tramp-get-file-property
		    :around #'magit-boost-get-file-property))
    (advice-remove 'magit-process-git #'magit-boost-process-git)
    (advice-remove 'magit-run-git-with-input #'magit-boost-run-git-with-input)
    (advice-remove 'vc-responsible-backend #'magit-boost-vc-responsible-backend)
    (advice-remove 'tramp-get-file-property #'magit-boost-get-file-property)))

(defcustom magit-boost-progress-entry-points
  '(magit-status magit-refresh magit-checkout magit-rebase
    magit-rebase-interactive-1 magit-log-setup-buffer)
  "List of Magit commands that trigger the progress reporter."
  :type '(repeat symbol))

(defvar magit-boost-progress nil
  "The active progress reporter object updated during Git command
execution.")

(defun magit-boost-progress-init ()
  "Initialize the progress reporter and return its display name."
  (let ((name (propertize (if magit-boost-mode "Magit-Boost" "Magit")
			  'face 'font-lock-function-call-face)))
    (setq magit-boost-progress (make-progress-reporter (format "%s..." name)))
    name))

(defun magit-boost-git-command-progress (orig-fun &rest args)
  "Report Git command execution time."
  (let* ((flatten-args (flatten-list (cdr args)))
	 (cmd (mapconcat #'identity (nconc (list "git") flatten-args) " "))
	 (suffix (propertize (concat cmd "...") 'face 'font-lock-string-face))
	 (start-time (current-time)))
    (unless magit-boost-progress
      (magit-boost-progress-init))
    (progress-reporter-update magit-boost-progress suffix)
    (let ((ret (apply orig-fun args))
	  (duration (float-time (time-subtract (current-time) start-time))))
      (setf suffix (format "%s done, took %.02fs" suffix duration))
      (progress-reporter-update magit-boost-progress nil suffix)
      ret)))

(defun magit-boost-entry-progress (orig-fun &rest args)
  "Wrap Magit entry points to report total execution time."
  (let* ((name (magit-boost-progress-init))
	 (start-time (current-time))
	 (ret (apply orig-fun args))
	 (duration (float-time (time-subtract (current-time) start-time))))
    (message (format "%s... done, took %.02fs" name duration))
    ret))

(define-minor-mode magit-boost-progress-mode
  "Minor mode to display progress and performance timing for Magit
commands.

When enabled, this mode provides visual feedback in the echo area
during Git operations. It uses a progress reporter to show the
currently executing Git command and logs the total execution time
of major Magit entry points (defined in
`magit-boost-progress-entry-points`) once they complete.

This is particularly useful on slow networks to confirm that Magit is
actively working and to measure the performance benefits provided by
Magit Boost."
  :init-value nil
  :global t
  (if magit-boost-progress-mode
      (progn
	(advice-add 'magit-process-git
		    :around #'magit-boost-git-command-progress)
	(dolist (entry magit-boost-progress-entry-points)
	  (advice-add entry :around #'magit-boost-entry-progress)))
    (advice-remove 'magit-process-git
		   #'magit-boost-git-command-progress)
    (dolist (entry magit-boost-progress-entry-points)
      (advice-remove entry #'magit-boost-entry-progress))))

(provide 'magit-boost)
