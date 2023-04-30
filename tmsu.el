;;; tmsu.el --- A basic TMSU interface    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; URL: https://github.com/vifon/tmsu.el
;; Keywords: files
;; Version: 0.9
;; Package-Requires: ((emacs "28.1"))

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

;; A TMSU interface.
;; See: https://tmsu.org/

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


(defgroup tmsu nil
  "A TMSU interface."
  :group 'files)


(defface tmsu-added-face
  '((t (:inherit diff-added)))
  "The faced used to display the added tags.")

(defface tmsu-removed-face
  '((t (:inherit diff-removed)))
  "The face used to display the removed tags.")

(defcustom tmsu-tag-list-preprocess-functions nil
  "A list of functions to apply to the current tags list in the `tmsu-edit' UI.

Each function should accept the tag list as its only argument and
return the modified tag list which is then passed to the next
function.  One possible use for this variable would be to sort
the list in a specific way."
  :type 'hook)

(defvar tmsu-edit-history nil
  "Command history of TMSU edits.")

(defvar tmsu-query-history nil
  "Command history of TMSU queries.")


(defun tmsu-get-tags (&optional file tags)
  "Fetch the TMSU tags of FILE or all tags in the database.

If TAGS is provided, filter the results to only these tags'
values.  It can be either a string or a list of strings.

When used in a loop, it's usually better to use
`tmsu-get-tags-for-files' to handle it in a single TMSU query."
  (let ((file-tags (split-string-shell-command
                    (string-trim-right
                     (with-output-to-string
                       (unless (= (apply
                                   #'process-file "tmsu" nil standard-output nil
                                   "tags" "--name=never" "--explicit" "--"
                                   (when file
                                     (list (file-local-name file))))
                                  0)
                         (error "Error when running TMSU")))))))
    (if tags
        (cl-delete-if-not
         (apply-partially #'string-match-p
                          (concat "^" (regexp-opt (ensure-list tags)) "="))
         file-tags)
      file-tags)))

(defun tmsu-get-tags-for-files (files &optional tags)
  "Efficiently query multiple FILES for TAGS.

The result is an alist of files and their tags."
  (let ((file-tags-alist
         (cl-mapcar
          (lambda (file tags)
            (cons file
                  ;; We requested the filename only to force TMSU into
                  ;; the one-line-per-file mode, let's drop the
                  ;; filename now and reuse the one we asked about in
                  ;; the first place.  Alternatively we could use this
                  ;; one, but we'd need to match and unquote
                  ;; it anyway.
                  (when (string-match "[^\\]: \\(.*\\)$" tags)
                    (split-string-shell-command (match-string 1 tags)))))
          files
          (string-lines
           (string-trim-right
            (with-output-to-string
              (unless (= (apply #'process-file "tmsu" nil standard-output nil
                                "tags" "--name=always" "--explicit" "--"
                                (mapcar #'file-local-name files))
                         0)
                (error "Error when running TMSU"))))))))
    (if tags
        (let ((pred (apply-partially
                     #'string-match-p
                     (concat "^" (regexp-opt (ensure-list tags)) "="))))
          (mapcar
           (lambda (file-tags)
             (cons (car file-tags)
                   (cl-delete-if-not
                    pred
                    (cdr file-tags))))
           file-tags-alist))
      file-tags-alist)))

(defun tmsu-get-values (&optional tag)
  "Fetch the TMSU values of TAG of all values in the database."
  (split-string-shell-command
   (string-trim-right
    (with-output-to-string
      (apply #'process-file "tmsu" nil
             (list standard-output nil)
             nil
             "values"
             (when tag
               (list "--" tag)))))))

(defconst tmsu--key-value-regex
  (rx bos
      (group-n 2 (+? any))
      (group "=")
      (group (* any)))
  "A regex matching an assignment of a TMSU tag value.

Example input: year=2000")

(defconst tmsu--expression-regex
  (rx bos
      (group (? (? "not")
                (? (* any)
                   space
                   (or "or"
                       "and")
                   (? (+ space)
                      "not"))
                (+ space)))
      (group (*? any))
      (? (group (* space)
                (or "<="
                    ">="
                    (any "<=>"))
                (* space))
         (group (* any)))
      eos)
  "A regular expression matching a TMSU comparison expression.

Matches both a single expression and compound expressions (`not',
`and', `or'), treating everything before the current argument as
a prefix (a single large match group).

No support for parentheses is currently planned, but that only
affects the completion: they can still be used by hand.

Example input: not year < 2000")

(defun tmsu-tag-key (key-value)
  "Return the key part of KEY-VALUE.

For convenience returns nil when passed nil."
  (when key-value
    (string-match tmsu--key-value-regex key-value)
    (match-string 2 key-value)))

(defun tmsu-tag-value (key-value)
  "Return the value part of KEY-VALUE.

For convenience returns nil when passed nil."
  (when key-value
    (string-match tmsu--key-value-regex key-value)
    (match-string 4 key-value)))

(defun tmsu--completion (regex &optional tags)
  "Generate a completion function for `completing-read'.

REGEX determines what expressions should offer a tag value as
a completion.  Usually either `tmsu--key-value-regex' or
`tmsu--expression-regex'.

TAGS can be provided as a pre-computed list of all tags to
complete.  If nil, calls `tmsu-get-tags' instead."
  (setq tags (or tags (tmsu-get-tags)))
  (completion-table-dynamic
   (lambda (string)
     (if (string-match regex string)
         (let ((prefix (match-string 1 string))
               (key (match-string 2 string))
               (infix (match-string 3 string)))
           (if infix
               (mapcar (lambda (value) (concat prefix key infix value))
                       (tmsu-get-values key))
             (mapcar (lambda (tag) (concat prefix tag))
                     tags)))
       tags))))

(defun tmsu-database-p ()
  "Check whether a TMSU database exists for the current directory.

Should be called near the beginning of all the user-facing
TMSU commands."
  (= (process-file "tmsu" nil nil nil "info") 0))

(defun tmsu-tags-add (file &rest tags)
  "Add TAGS to FILE."
  (apply #'process-file
         "tmsu" nil nil nil
         "tag" "--explicit" "--" (file-local-name file)
         tags))

(defun tmsu-tags-remove (file &rest tags)
  "Remove TAGS from FILE."
  (apply #'process-file
         "tmsu" nil nil nil
         "untag" "--" (file-local-name file)
         tags))

;;;###autoload
(defun tmsu-edit (file)
  "Interactively edit the TMSU tags of FILE."
  (interactive "f")
  (unless (tmsu-database-p)
    (error "No TMSU database"))
  (let* ((file-name (file-name-nondirectory
                     (directory-file-name file)))
         (tags-all (tmsu-get-tags))
         (tags-old (let ((tags (tmsu-get-tags file)))
                     (dolist (func tmsu-tag-list-preprocess-functions tags)
                       (setq tags (funcall func tags)))))
         (tags-new (completing-read-multiple
                    (format-message "Tag `%s': " file-name)
                    (tmsu--completion tmsu--key-value-regex
                                      tags-all)
                    nil nil
                    (string-join tags-old ",")
                    'tmsu-edit-history))
         (tags-added   (cl-set-difference tags-new tags-old :test #'string=))
         (tags-removed (cl-set-difference tags-old tags-new :test #'string=)))
    (unless (or tags-added
                tags-removed)
      (user-error "No changes"))
    (apply #'tmsu-tags-remove file tags-removed)
    (apply #'tmsu-tags-add    file tags-added)
    (message "TMSU tags change on `%s': %S: %s"
             file-name
             (tmsu-get-tags file)
             (string-join
              (nconc (mapcar (lambda (tag)
                               (concat "-"
                                       (propertize tag
                                                   'face 'tmsu-removed-face)))
                             tags-removed)
                     (mapcar (lambda (tag)
                               (concat "+"
                                       (propertize tag
                                                   'face 'tmsu-added-face)))
                             tags-added))
              " "))))

(provide 'tmsu)
;;; tmsu.el ends here
