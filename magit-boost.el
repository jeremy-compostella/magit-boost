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
;; persistent bash process.  This package initializes and manages dedicated Boost
;; buffers, intercepts Git commands from Magit, and executes them using the
;; running bash process.  In my experience, this results in a 2-4x performance
;; boost for commands like `magit-status' and `magit-log'.

(require 'cl-seq)
(require 'magit)
(require 'tramp)

(defcustom magit-boost-buffer-name "*magit-boost*"
  "Name of the magit bash buffer."
  :type 'string)

(defcustom magit-boost-debug nil
  "If non-nil, enable debug messages for Magit Boost."
  :type 'boolean)

(defcustom magit-boost-feedback 'progress
  "Type of feedback to show during long operations.
Could be 'progress or 'performance."
  :type '(choice (const :tag "Progress reporter" progress)
		 (const :tag "Performance messages" performance)))

(defvar-local magit-boost--git-dir nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

(defvar-local magit-boost--git-dir-truename nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

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

(defun magit-boost--git-dir (dir)
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
	(let ((git-dir (magit-boost--git-dir default-directory)))
	  (setq default-directory (concat dir "/")
		magit-boost--git-dir git-dir
		magit-boost--git-dir-truename (file-truename git-dir)
		magit-boost-connection-type connection-type)))
      buffer)
    (error "No git repository found")))

(defsubst magit-boost--bash-buffer-p (connection-type filename buffer)
  "Return non-nil if BUFFER is a Magit Boost buffer."
  (with-current-buffer buffer
    (and (eq magit-boost-connection-type connection-type)
	 (or (string-prefix-p default-directory filename)
	     (string-prefix-p magit-boost--git-dir-truename filename)))))

(defun magit-boost--existing-buffer (filename connection-type)
  (cl-find filename (magit-boost-buffers)
	   :test (apply-partially #'magit-boost--bash-buffer-p
				  connection-type)))

(defun magit-boost-buffer (dir connection-type)
  "Return a Magit Boost buffer associated with DIR."
  (let ((buf (or (magit-boost--existing-buffer dir connection-type)
		 (magit-boost-new-buffer dir connection-type))))
    (unless (get-buffer-process buf)
      (let* ((process-connection-type connection-type)
	     (process (start-file-process "magit-boost-process" buf "bash")))
	(process-send-string process "export PS1=''\n")
	(accept-process-output process 1 nil t)
	(set-process-filter process 'magit-boost-filter)))
    buf))

(defun magit-boost--stderr (file)
  (if file
      (format "; if [ -e '%s' ]; then cat '%s'; fi" file file)
    ""))

(defun magit-boost-process-cmd (cmd input destination)
  "Execute PROGRAM with ARGS via the Magit Bash process."
  (let* ((buffer (magit-boost-buffer default-directory
				    (if (and input
					     (goto-char (point-min))
					     (search-forward "\r" nil t))
					'nil 'pty)))
	 (current-dir (with-parsed-tramp-file-name default-directory c
			c-localname))
	 res found ret)
    (with-current-buffer buffer
      (let* ((process (get-buffer-process (current-buffer)))
	     start stderr
	     (loop 0))
	(when input
	  (setf input (replace-regexp-in-string "'" "'\"'\"'" input)
		cmd (format "echo '%s' | %s" input cmd)))
	(when (and (listp destination)
		   (stringp (cadr destination)))
	  (setf stderr (cadr destination)
		cmd (format "%s 2>'%s'" cmd stderr)))
	(setf cmd (format "%s ; echo __MAGIT_BASH_DONE_$?__ %s"
			  cmd (magit-boost--stderr stderr)))
	(setf cmd (concat "cd " current-dir "; " cmd "; cd - > /dev/null\n"))
	(save-excursion
	  (goto-char (point-max))
	  (insert cmd "\n"))
	(process-send-string process cmd)
	(setf start (point-max-marker))
	(while (not found)
	  (accept-process-output process 1 nil t)
	  (save-excursion
	    (goto-char start)
	    (when (re-search-forward "__MAGIT_BASH_DONE_\\([0-9]+\\)__" nil t)
	      (setf found t
		    res (buffer-substring-no-properties start
							(match-beginning 0))
		    ret (string-to-number (match-string 1)))
	      (when stderr
		(let ((err (buffer-substring-no-properties (match-end 0) (point-max))))
		  (unless (string= err "\n")
		    (with-temp-buffer
 		      (insert err)
		      (write-region (point-min) (point-max) stderr)))))))))
      (unless magit-boost-debug
	(erase-buffer)))
    (when (and (= ret 0) destination)
      (insert res))
    ret))

(defun magit-boost--process-git (destination args &optional input)
  "Execute a Git command via the Magit Bash process."
  (cl-flet ((escape-double-quote (str)
	      (with-temp-buffer
		(insert str)
		(goto-char (point-min))
		(while (search-forward "\"" nil t)
		  (save-excursion
		    (goto-char (match-beginning 0))
		    (insert "\\")))
		(buffer-string))))
    (let ((cmd (concat "git " (mapconcat (apply-partially #'format "\"%s\"")
					 (mapcar #'escape-double-quote
						 (magit-process-git-arguments args))
					 " "))))
      (magit-boost-process-cmd cmd input destination))))

(defun magit-boost-load-files-attributes (files)
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
	 (cmd (concat "for file in "
		      (mapconcat (lambda (file)
				   (format "'%s' "
					   (tramp-file-name-localname
					    (tramp-dissect-file-name file))))
				 files " ")
		      "; do " tests " ; " truename
		      "; echo ''"
		      "; done")))
    (with-temp-buffer
      (let ((ret (magit-boost-process-cmd cmd nil t)))
	(unless (= ret 0)
	  (error "Failed to read files attributes")))
      (goto-char (point-min))
      (dolist (file files)
	(let ((res (split-string (buffer-substring-no-properties
				  (line-beginning-position) (line-end-position)))))
	  (dolist (tp test-and-props)
	    (let ((localname (tramp-file-name-localname (tramp-dissect-file-name file))))
	      ;; (message "%s %s %s" localname (cdr tp) (if (member (cdr tp) res) t nil))
	      (with-parsed-tramp-file-name file nil
		(tramp-set-file-property
		 v localname (cdr tp) (if (member (cdr tp) res) t nil)))))
	  (with-parsed-tramp-file-name file nil
	    (when (member "file-exists-p" res)
	      (forward-line)
	      ;; (message "truename: %s" (buffer-substring-no-properties
	      ;; 			       (line-beginning-position)
	      ;; 			       (line-end-position)))
	      (tramp-set-file-property
	       v localname "file-truename"
	       (buffer-substring-no-properties (line-beginning-position)
					       (line-end-position)))))
	  (forward-line))))))

(defvar-local magit-boost-git-tree-files '())

(defun magit-boost-get-file-property (orig-fun &rest args)
  (let* ((key (car args))
	 (localname (cadr args))
	 (filename (tramp-make-tramp-file-name key localname)))
    (when-let ((buffer (magit-boost--existing-buffer filename 'pty)))
      (with-current-buffer buffer
	(when (or (string-prefix-p (concat default-directory magit-boost--git-dir)
				   filename)
		  (string-prefix-p magit-boost--git-dir-truename filename))
	  (let ((property (caddr args))
		(default (cadddr args)))
	    (add-to-list 'magit-boost-git-tree-files filename)
	    (let ((value (funcall orig-fun key localname property tramp-cache-undefined)))
	      (when (eq value tramp-cache-undefined)
		(magit-boost-load-files-attributes magit-boost-git-tree-files)))))))
    (apply orig-fun args)))

(defun magit-boost--show-cdup (dir)
  (when-let* ((buffer (magit-boost--existing-buffer dir 'pty))
	      (root (with-current-buffer buffer
		      default-directory)))
    (insert (if (with-current-buffer buffer
		  (string-prefix-p magit-boost--git-dir-truename dir))
		""
	      (let ((sub (substring dir (length root))))
		(if (string-empty-p sub)
		    ""
		  (mapconcat (lambda (_) "..")
			     (cl-delete "" (split-string sub "/")) "/"))))
	    "\n")
    0))

(defvar magit-boost--progress nil)

(defun magit-boost--git-cmd-wrapper (orig-fun &rest args)
  (when (and magit-boost-feedback magit-boost--progress)
    (progress-reporter-update magit-boost--progress))
  (let ((start-time (current-time))
	(ret (apply orig-fun args))
	suffix)
    (when magit-boost--progress
      (when (eq magit-boost-feedback 'performance)
	(let ((cmd (propertize (mapconcat #'identity (append (list "git")
							     (flatten-list (cddr args)))
					  " ")
			       'face 'font-lock-string-face)))
	  (setf suffix (format "%s took %.02fs"
  			       cmd (float-time (time-subtract (current-time) start-time))))))
      (progress-reporter-update magit-boost--progress nil suffix))
    ret))

(defun magit-boost--entry-wrapper (orig-fun &rest args)
  (let* ((fun-name (propertize "magit" 'face
			       'font-lock-function-call-face)))
    (when magit-boost-feedback
      (setq magit-boost--progress (make-progress-reporter (format "%s..." fun-name))))
    (let ((start-time (current-time))
	  (ret (apply orig-fun args)))
      (cond ((eq magit-boost-feedback 'performance)
	     (let ((msg (format "%s done. It took %.02f seconds." fun-name
				(float-time (time-subtract (current-time)
							   start-time)))))
	       (message msg)))
	    ((eq magit-boost-feedback 'progress)
	     (progress-reporter-done magit-boost--progress)))
      ret)))

(defcustom magit-boost-entry-points '(magit-status
				      magit-refresh
				      magit-checkout
				      magit-rebase
				      magit-rebase-interactive)
  "")

(dolist (entry '(magit-boost--revparse))
  (advice-add entry :around #'magit-boost--git-cmd-wrapper))

(advice-add 'magit-process-git :around #'magit-boost--git-cmd-wrapper)

(dolist (entry magit-boost-entry-points)
  (advice-add entry :around #'magit-boost--entry-wrapper))

(defun magit-boost--show-toplevel (dir)
  (when-let* ((buffer (magit-boost-buffer dir 'pty))
	      (root (with-current-buffer buffer
		      default-directory)))
    (with-parsed-tramp-file-name root r
      (insert r-localname)
      0)))

(defun magit-boost--revparse (orig-fun &rest args)
  (let ((git-args (flatten-list (cl-copy-list (cdr args)))))
    (if (and (stringp (car git-args)) (string= (car git-args) "rev-parse"))
	(cond ((string= (cadr git-args) "--show-cdup")
	       (magit-boost--show-cdup default-directory))
	      ((string= (cadr git-args) "--show-toplevel")
	       (magit-boost--show-toplevel default-directory))
	      ((string= (cadr git-args) "--git-dir")
	       (when-let* ((buf (magit-boost--existing-buffer
				 default-directory 'pty))
			   (git-dir (with-current-buffer buf
				      magit-boost--git-dir)))
		 (insert git-dir)
		 0))
	      ((apply orig-fun args)))
      (apply orig-fun args))))

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
  (if (magit-boost--existing-buffer (car args) 'pty)
      "Git"
    (apply orig-fun args)))

(defun magit-boost--tramp-sh-handle-file-writable-p (orig-fun &rest args)
  (if (magit-boost--existing-buffer (car args) 'pty)
      t
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
		    :around #'magit-boost--vc-responsible-backend)
	(advice-add 'magit-boost--process-git
		    :around #'magit-boost--revparse)
	(advice-add 'tramp-get-file-property
		    :around #'magit-boost-get-file-property))
    (advice-remove 'magit-process-git #'magit-boost-process-git)
    (advice-remove 'magit-run-git-with-input #'magit-boost-run-git-with-input)
    (advice-remove 'vc-responsible-backend #'magit-boost--vc-responsible-backend)
    (advice-remove 'tramp-sh-handle-file-writable-p
		   #'magit-boost--tramp-sh-handle-file-writable-p)
    (advice-remove 'magit-boost--process-git
		   #'magit-boost--revparse)
    (advice-remove 'tramp-get-file-property
		   #'magit-boost-get-file-property)))

(provide 'magit-boost)
