;;; tmsu-dired.el --- A basic interface between TMSU and dired.    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A basic interface between TMSU and dired.

;;; Code:

(require 'tmsu)

;;;###autoload
(defun tmsu-edit-dired (parent)
  "Edit the tags of the file at point.

If PARENT is non-nil, edit the current directory instead of
the file at point."
  (interactive "P")
  (let ((file (if parent
                  (dired-current-directory)
                (dired-get-filename t t))))
    (if file
        (tmsu-edit file)
      (user-error "No file on this line"))))

;;;###autoload
(defun tmsu-edit-dired-dwim ()
  (interactive)
  (let* ((file (dired-get-filename t t))
         (is-directory (and file (file-directory-p file))))
    (tmsu-edit-dired (not is-directory))))


;; This section is strongly inspired by find-dired.el (included in GNU
;; Emacs) with lots of code being reused.  Please refer to that file
;; for the original authors.

(require 'find-dired)

(defvar tmsu-ls-switches "-lhb")
(defvar tmsu-ls-subdir-switches "-alhb")

(defvar tmsu-query nil
  "The last `tmsu' query used by \\[tmsu-query-dired].")

;;;###autoload
(defun tmsu-query-dired (query)
  "Display the query results in a virtual dired buffer."
  (interactive (if (tmsu-database-p)
                   (list (completing-read "TMSU query: "
                                          (tmsu--get-tags)))
                 (error "No TMSU database")))

  (let ((dired-buffers dired-buffers))
    (pop-to-buffer-same-window (get-buffer-create "*tmsu*"))

    ;; See if there's still a `tmsu' running, and offer to kill it
    ;; first, if it is.
    (let ((tmsu (get-buffer-process (current-buffer))))
      (when tmsu
	    (if (or (not (eq (process-status tmsu) 'run))
		        (yes-or-no-p
		         (format-message "A `tmsu' process is running; kill it? ")))
	        (condition-case nil
		        (progn
		          (interrupt-process tmsu)
		          (sit-for 1)
		          (delete-process tmsu))
	          (error nil))
	      (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq tmsu-query query)

    (shell-command (format "tmsu files -0 %s | xargs -0 ls -d %s &"
                           (shell-quote-argument query)
                           tmsu-ls-switches)
                   (current-buffer))
    (dired-mode default-directory tmsu-ls-switches)

    (setq-local dired-sort-inhibit t)
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (tmsu-query-dired tmsu-query)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (setq-local dired-subdir-alist
                  (list (cons default-directory (point-min-marker)))))
    (setq-local dired-subdir-switches tmsu-ls-subdir-switches)

    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " default-directory ":\n")
    ;; Make second line a ``tmsu'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (let ((point (point)))
      (insert "  tmsu files " tmsu-query "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; `find-dired-filter' should do with no changes at all.
      (set-process-filter proc #'find-dired-filter)
      ;; `find-dired-filter' needed minor changes (mostly messages),
      ;; so there we go.
      (set-process-sentinel proc #'tmsu-dired-sentinel)
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun tmsu-dired-sentinel (proc state)
  "Sentinel for \\[tmsu-query-dired] processes."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
	    (with-current-buffer buf
	      (let ((inhibit-read-only t))
	        (save-excursion
              (save-restriction
                (widen)
                (let ((point (point-max)))
                  (goto-char point)
                  (insert "\n  tmsu "
                          (substring state 0 -1) ; omit \n at end of STATE.
                          " at " (substring (current-time-string) 0 19))
                  (dired-insert-set-properties point (point))))
              (setq mode-line-process
		            (format ":%s" (process-status proc)))
	          ;; Since the buffer and mode line will show that the
	          ;; process is dead, we can delete it now.  Otherwise it
	          ;; will stay around until M-x `list-processes'.
	          (delete-process proc)
	          (force-mode-line-update))))
	  (message "tmsu-query-dired %s finished." buf))))

(provide 'tmsu-dired)
;;; tmsu-dired.el ends here
