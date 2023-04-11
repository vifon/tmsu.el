;;; tmsu-dired-overlay.el --- Dired overlays with TMSU tags   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; URL: https://github.com/vifon/tmsu.el
;; Keywords: files
;; Version: 0.9

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

;; Dired overlays with TMSU tags.

;;; Code:

(require 'tmsu-dired)


(defvar-local tmsu-dired-overlay-overlays nil)

(defun tmsu-dired-overlay-delete-overlays (&rest _)
  "Delete all overlays in `tmsu-dired-overlay-overlays'."
  (interactive)
  (mapc #'delete-overlay tmsu-dired-overlay-overlays)
  (kill-local-variable 'tmsu-dired-overlay-overlays)
  (remove-function (local 'revert-buffer-function)
                   #'tmsu-dired-overlay-delete-overlays))

(defun tmsu-dired-overlay-create-overlay-at-point (tags &optional file-tags)
  "Add an overlay with TAGS values on the current `dired' line.

The pre-computed tags for the current file can be passed with
FILE-TAGS.  This way no additional TMSU queries are performed.
FILE-TAGS should be a cons cell with the filename in its `car'
and the tag list in its `cdr'.  In such case it's expected for
FILE-TAGS to already be pre-filtered and the value of TAGS
is ignored."
  (when-let ((values (if file-tags
                         (cdr file-tags)
                       (tmsu-get-tags (dired-get-filename nil t) tags))))
    (end-of-line)
    (let ((ov (make-overlay (point) (point) nil t nil)))
      (overlay-put ov 'after-string
                   (concat
                    (propertize " \t"
                                'cursor t)
                    (propertize
                     (mapconcat
                      (lambda (tag)
                        (let ((click-command
                               (lambda ()
                                 (interactive)
                                 ;; This `default-directory' could
                                 ;; have been a call to
                                 ;; `dired-current-directory' for
                                 ;; slightly different semantics.
                                 ;; This choice is deliberate and not
                                 ;; a bug, as I find these
                                 ;; semantics reasonable.
                                 (tmsu-dired-query default-directory
                                                   (cons tag tmsu-dired-query-args)
                                                   tmsu-dired-query-flags))))
                          (propertize
                           tag
                           'mouse-face 'highlight
                           'help-echo (concat "mouse-2: " "TMSU query: " tag)
                           'keymap (let ((map (make-sparse-keymap)))
                                     (define-key map [mouse-2] click-command)
                                     (define-key map (kbd "RET") click-command)
                                     (define-key map [follow-link] 'mouse-face)
                                     map))))
                      values ",")
                     'face 'italic)))
      (push ov tmsu-dired-overlay-overlays))))

;;;###autoload
(defun tmsu-dired-overlay-create-overlays (tags &optional no-replace)
  "Add overlays with the values of TAGS to all `dired' files.

Unless NO-REPLACE is passed (\\[universal-argument]), the
previous such overlays are removed first."
  (interactive "i\nP")
  (unless (tmsu-database-p)
    (error "No TMSU database"))
  (setq tags (or tags (completing-read-multiple
                       "TMSU tags to display: " (tmsu-get-tags)
                       nil nil nil 'tmsu-query-history)))
  (unless no-replace
    (tmsu-dired-overlay-delete-overlays))
  (when tags
    (save-excursion
      (goto-char (dired-subdir-min))
      (let ((max (1- (dired-subdir-max)))
            (file-tags-alist (tmsu-get-tags-for-files
                              (directory-files (dired-current-directory) t)
                              tags)))
        (while (< (point) max)
          (when-let ((file (dired-get-filename nil t)))
            (tmsu-dired-overlay-create-overlay-at-point
             tags (assoc file file-tags-alist)))
          (forward-line 1))))
    (add-function :before (local 'revert-buffer-function)
                  #'tmsu-dired-overlay-delete-overlays)))

;;;###autoload
(defalias 'tmsu-dired-overlay 'tmsu-dired-overlay-create-overlays)

(provide 'tmsu-dired-overlay)
;;; tmsu-dired-overlay.el ends here
