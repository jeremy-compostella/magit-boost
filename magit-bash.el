;;; magit-bash.el --- Integrate Magit with Bash processes. -*- lexical-binding: t; -*-

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
;; Magit Bash enhances the speed of Git command execution by leveraging a
;; persistent bash process.  This package initializes and manages dedicated Bash
;; buffers, intercepts Git commands from Magit, and executes them using the
;; running bash process.  In my experience, this results in a 2-4x performance
;; boost for commands like `magit-status' and `magit-log'.

(require 'cl-seq)
(require 'magit)
(require 'tramp)

(defcustom magit-bash-buffer-name "*magit-bash*"
  "Name of the magit bash buffer."
  :type 'string)

(defcustom magit-bash-debug nil
  "If non-nil, enable debug messages for Magit Bash."
  :type 'boolean)

(defvar-local magit-bash--git-dir nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

(defvar-local magit-bash--git-dir-truename nil
  "Buffer-local variable to cache the Git tree object for the current
repository.")

(defun magit-bash-filter (process string)
  "Process filter for Magit Bash buffers.

Append STRING, the output from PROCESS, to the end of the buffer
associated with PROCESS. This function is used to collect and display
output from the Bash process in the Magit Bash buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)))

(defun magit-bash-buffers ()
  "Return a list of all Magit Bash buffers."
  (seq-filter (lambda (b)
		(string-prefix-p magit-bash-buffer-name (buffer-name b)))
	      (buffer-list)))

(defvar-local magit-bash-connection-type nil
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

This variable is automatically set when creating a new Magit Bash buffer via
`magit-bash-new-buffer', and is used to ensure the correct process connection
type is chosen for each Git command execution.")

(defun magit-bash--git-dir (dir)
  (with-temp-buffer
    (setq default-directory dir)
    (if (= (process-file "git" nil t nil "rev-parse" "--git-dir") 0)
	(string-trim (buffer-string))
      (error "Failed to read the git tree"))))

(defun magit-bash-new-buffer (dir connection-type)
  "Create and return a new Magit Bash buffer for DIR with
CONNECTION-TYPE."
  (setf dir (with-temp-buffer
	      (setq default-directory dir)
	      (with-parsed-tramp-file-name (expand-file-name dir) nil
		(if (= (process-file "git" nil t nil "rev-parse" "--show-toplevel") 0)
		    (tramp-make-tramp-file-name v (string-trim (buffer-string)))))))
  (if dir
    (let* ((buf-name (format magit-bash-buffer-name))
	   (buffer (generate-new-buffer buf-name)))
      (uniquify-rationalize-file-buffer-names buf-name default-directory buffer)
      (with-current-buffer buffer
	(let ((git-dir (magit-bash--git-dir default-directory)))
	  (setq default-directory (concat dir "/")
		magit-bash--git-dir git-dir
		magit-bash--git-dir-truename (file-truename git-dir)
		magit-bash-connection-type connection-type)))
      buffer)
    (error "No git repository found")))

(defsubst magit-bash--bash-buffer-p (connection-type filename buffer)
  "Return non-nil if BUFFER is a Magit Bash buffer."
  (with-current-buffer buffer
    (and (eq magit-bash-connection-type connection-type)
	 (or (string-prefix-p default-directory filename)
	     (string-prefix-p magit-bash--git-dir-truename filename)))))

(defun magit-bash--existing-buffer (filename connection-type)
  (cl-find filename (magit-bash-buffers)
	   :test (apply-partially #'magit-bash--bash-buffer-p
				  connection-type)))

(defun magit-bash-buffer (dir connection-type)
  "Return a Magit Bash buffer associated with DIR."
  (let ((buf (or (magit-bash--existing-buffer dir connection-type)
		 (magit-bash-new-buffer dir connection-type))))
    (unless (get-buffer-process buf)
      (let* ((process-connection-type connection-type)
	     (process (start-file-process "magit-bash-process" buf "bash")))
	(process-send-string process "export PS1=''\n")
	(accept-process-output process 1 nil t)
	(set-process-filter process 'magit-bash-filter)))
    buf))

(defun magit-bash--stderr (file)
  (if file
      (format "; if [ -e '%s' ]; then cat '%s'; fi" file file)
    ""))

(defun magit-bash-process-cmd (cmd input destination)
  "Execute PROGRAM with ARGS via the Magit Bash process."
  (let* ((buffer (magit-bash-buffer default-directory
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
			  cmd (magit-bash--stderr stderr)))
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
      (unless magit-bash-debug
	(erase-buffer)))
    (when (and (= ret 0) destination)
      (insert res))
    ret))

(defun magit-bash--process-git (destination args &optional input)
  "Execute a Git command via the Magit Bash process."
  (let ((cmd (concat "git " (mapconcat (lambda (x) (format "\"%s\"" x))
				       (magit-process-git-arguments args)
				       " "))))
    (magit-bash-process-cmd cmd input destination)))

(defun magit-bash-load-files-attributes (files)
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
      (let ((ret (magit-bash-process-cmd cmd nil t)))
	(unless (= ret 0)
	  (error "Failed to read files attributes")))
      (goto-char (point-min))
      (dolist (file files)
	(let ((res (split-string (buffer-substring-no-properties
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
	       (buffer-substring-no-properties (line-beginning-position)
					       (line-end-position)))))
	  (forward-line))))))

(defvar-local magit-bash-git-tree-files '())

(defun magit-bash-get-file-property (orig-fun &rest args)
  (let* ((key (car args))
	 (localname (cadr args))
	 (filename (tramp-make-tramp-file-name key localname)))
    (when-let ((buffer (magit-bash--existing-buffer filename 'pty)))
      (with-current-buffer buffer
	(when (or (string-prefix-p (concat default-directory magit-bash--git-dir)
				   filename)
		  (string-prefix-p magit-bash--git-dir-truename filename))
	  (let ((property (caddr args))
		(default (cadddr args)))
	    (add-to-list 'magit-bash-git-tree-files filename)
	    (let ((value (funcall orig-fun key localname property tramp-cache-undefined)))
	      (when (eq value tramp-cache-undefined)
		(magit-bash-load-files-attributes magit-bash-git-tree-files)))))))
    (apply orig-fun args)))

(defun magit-bash--show-cdup (dir)
  (when-let* ((buffer (magit-bash-buffer dir 'pty))
	      (root (with-current-buffer buffer
		      default-directory)))
    (insert (if (with-current-buffer buffer
		  (string-prefix-p magit-bash--git-dir-truename dir))
		""
	      (let ((sub (substring dir (length root))))
		(if (string-empty-p sub)
		    ""
		  (mapconcat (lambda (_) "..")
			     (cl-delete "" (split-string sub "/")) "/"))))
	    "\n")
    0))

(defun magit-bash--show-toplevel (dir)
  (when-let* ((buffer (magit-bash-buffer dir 'pty))
	      (root (with-current-buffer buffer
		      default-directory)))
    (with-parsed-tramp-file-name root r
      (insert r-localname)
      0)))

(defun magit-bash--revparse (orig-fun &rest args)
  (let ((git-args (flatten-list (cdr args))))
    (if (and (stringp (car git-args)) (string= (car git-args) "rev-parse"))
	(cond ((string= (cadr git-args) "--show-cdup")
	       (magit-bash--show-cdup default-directory))
	      ((string= (cadr git-args) "--show-toplevel")
	       (magit-bash--show-toplevel default-directory))
	      ((string= (cadr git-args) "--git-dir")
	       (when-let* ((buf (magit-bash--existing-buffer
				 default-directory 'pty))
			   (git-dir (with-current-buffer buf
				      magit-bash--git-dir)))
		 (insert git-dir)
		 0))
	      ((apply orig-fun args)))
      (apply orig-fun args))))

(defun magit-bash-process-git (orig-fun &rest args)
  "Advice for `magit-process-git' to optionally route Git commands through
a persistent Bash process.

If the current buffer's `default-directory' is a TRAMP (remote), execute
the Git command using Magit Bash's accelerated Bash process, passing
DESTINATION (the first element of ARGS) and the prepared Git arguments."
  (if (tramp-tramp-file-p default-directory)
      (magit-bash--process-git (car args) (cdr args))
    (apply orig-fun args)))

(defun magit-bash-run-git-with-input (orig-fun &rest args)
  "Advice for `magit-run-git-with-input' to accelerate Git commands via a
persistent Bash process.

If the current buffer is not a TRAMP (remote) buffer, call the original
function `orig-fun' with ARGS.

Otherwise, execute the Git command using the Magit Bash process, piping
the current buffer's contents as input to the command. This reduces
process startup overhead and improves performance for repeated Git
operations."
  (if (tramp-tramp-file-p default-directory)
      (magit-bash--process-git t args (buffer-string))
    (apply orig-fun args)))

(defun magit-bash--vc-responsible-backend (orig-fun &rest args)
  "If a Magit Bash buffer with a 'pty connection exists, return Git.

This avoids invoking the slower default implementation of
`vc-responsible-backend', which can be a performance bottleneck in Magit."
  (if (magit-bash--existing-buffer (car args) 'pty)
      "Git"
    (apply orig-fun args)))

(defun magit-bash--tramp-sh-handle-file-writable-p (orig-fun &rest args)
  (if (magit-bash--existing-buffer (car args) 'pty)
      t
    (apply orig-fun args)))

(define-minor-mode magit-bash-mode
  "Minor mode to accelerate Magit Git commands by routing them through a
persistent Bash process.

When enabled, `magit-bash-mode' intercepts Magit Git commands and
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
  (if magit-bash-mode
      (progn
	(advice-add 'magit-process-git
		    :around #'magit-bash-process-git)
	(advice-add 'magit-run-git-with-input
		    :around #'magit-bash-run-git-with-input)
	(advice-add 'vc-responsible-backend
		    :around #'magit-bash--vc-responsible-backend)
	(advice-add 'tramp-sh-handle-file-writable-p
		    :around #'magit-bash--tramp-sh-handle-file-writable-p)
	(advice-add 'magit-bash--process-git
		    :around #'magit-bash--revparse)
	(advice-add 'tramp-get-file-property
		    :around #'magit-bash-get-file-property))
    (advice-remove 'magit-process-git #'magit-bash-process-git)
    (advice-remove 'magit-run-git-with-input #'magit-bash-run-git-with-input)
    (advice-remove 'vc-responsible-backend #'magit-bash--vc-responsible-backend)
    (advice-remove 'tramp-sh-handle-file-writable-p
		   #'magit-bash--tramp-sh-handle-file-writable-p)
    (advice-remove 'magit-bash--process-git
		   #'magit-bash--revparse)
    (advice-remove 'tramp-get-file-property
		   #'magit-bash-get-file-property)))

(provide 'magit-bash)
