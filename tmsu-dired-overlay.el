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


(defgroup tmsu-dired-overlay nil
  "Dired overlays with TMSU tags."
  :group 'tmsu-dired)


(defface tmsu-dired-overlay-face
  '((t (:inherit italic)))
  "The face used for the tags in the overlays.")

(defvar-local tmsu-dired-overlay-overlays nil)

(defun tmsu-dired-overlay-delete-all-overlays (&rest _)
  "Delete all overlays in `tmsu-dired-overlay-overlays'.

Intended as an advice for `revert-buffer-function', removes
itself from this variable's value."
  (interactive)
  (mapc #'delete-overlay tmsu-dired-overlay-overlays)
  (kill-local-variable 'tmsu-dired-overlay-overlays)
  (remove-function (local 'revert-buffer-function)
                   #'tmsu-dired-overlay-delete-all-overlays))

(defun tmsu-dired-overlay-delete-overlays (&optional beg end)
  "Delete `tmsu-dired-overlay-overlays' between positions BEG and END.

BEG and END default to `dired-subdir-min' and
`dired-subdir-max' respectively."
  (interactive)
  (setq beg (or beg (dired-subdir-min))
        end (or end (dired-subdir-max)))
  (dolist (ov (overlays-in beg end))
    (when (memq ov tmsu-dired-overlay-overlays)
      (setq tmsu-dired-overlay-overlays
            (delq ov tmsu-dired-overlay-overlays))
      (delete-overlay ov))))

(defun tmsu-dired-overlay-follow-link (button)
  "Follow the BUTTON and call `tmsu-dired-query' accordingly."
  ;; This `default-directory' could have been a call to
  ;; `dired-current-directory' for slightly different semantics.
  ;; This choice is deliberate and not a bug, as I find these
  ;; semantics reasonable.
  (tmsu-dired-query default-directory
                    (cons (button-get button 'tmsu-tag) tmsu-dired-query-args)
                    tmsu-dired-query-flags))

(define-button-type 'tmsu-dired-overlay-button
  'follow-link t
  'action #'tmsu-dired-overlay-follow-link
  'help-echo "mouse-2: narrow down the TMSU query"
  'face 'tmsu-dired-overlay-face)

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
                   (with-temp-buffer
                     (insert
                      ;; Display the cursor before the overlay, not after.
                      (propertize " " 'cursor t)
                      ;; Crudely align the tag lists using tab-stops.
                      ;; Won't be pretty, but won't be terrible
                      ;; either.  Should always provide at least two
                      ;; characters of spacing.
                      "\t")
                     (dolist (tag values)
                       (insert-text-button tag
                                           :type 'tmsu-dired-overlay-button
                                           'tmsu-tag tag)
                       (insert ","))
                     ;; Remove the "," after the last tag.  Would be
                     ;; buggy for an empty list, but this scenario
                     ;; will never happen due to the check at the
                     ;; beginning of this function.
                     (delete-char -1)
                     (buffer-string)))
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
    (tmsu-dired-overlay-delete-overlays (dired-subdir-min)
                                        (dired-subdir-max)))
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
                  #'tmsu-dired-overlay-delete-all-overlays)))

;;;###autoload
(defalias 'tmsu-dired-overlay 'tmsu-dired-overlay-create-overlays)

(provide 'tmsu-dired-overlay)
;;; tmsu-dired-overlay.el ends here
