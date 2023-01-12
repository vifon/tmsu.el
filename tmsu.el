;;; tmsu.el --- A basic TMSU interface    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Wojciech Siewierski

;; Author: Wojciech Siewierski
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

;; A basic TMSU interface.
;; See: https://tmsu.org/

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


(defgroup tmsu nil
  "A basic TMSU interface."
  :group 'files)


(defface tmsu-added-face
  '((t (:inherit diff-added)))
  "The faced used to display the added tags.")

(defface tmsu-removed-face
  '((t (:inherit diff-removed)))
  "The face used to display the removed tags.")

(defcustom tmsu-priority-tag nil
  "A tag (actually a prefix) to always sort as the last one for easy editing.

A sensible example: \"episodes-watched=\""
  :type '(choice
          (const :tag "None" nil)
          (string))
  :safe #'stringp)

(defvar tmsu-edit-history nil
  "Command history of TMSU edits.")

(defvar tmsu-query-history nil
  "Command history of TMSU queries.")


(defun tmsu-get-tags (&optional file)
  "Fetch the TMSU tags of FILE or all tags in the database."
  (split-string-shell-command
   (string-trim-right
    (with-output-to-string
      (unless (= (apply #'call-process "tmsu" nil standard-output nil
                        "tags" "--name=never" "--explicit" "--"
                        (when file
                          (list file)))
                 0)
        (error "Error when running TMSU"))))))

(defun tmsu-get-values (&optional tag)
  "Fetch the TMSU values of TAG of all values in the database."
  (split-string-shell-command
   (string-trim-right
    (with-output-to-string
      (unless (= (apply #'call-process "tmsu" nil standard-output nil
                        "values"
                        (when tag
                          (list "--" tag)))
                 0)
        (error "No such tag: %s" tag))))))

(defconst tmsu--key-value-regex
  (rx bos
      (group-n 2 (+? any))
      (group-n 3 "=")
      (group-n 4 (* any)))
  "A regex matching an assignment of a TMSU tag value.

Example input: year=2000")

(defconst tmsu--comparison-regex
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

Example input: not year < 2000")

(defun tmsu--completion (regex &optional tags)
  "Generate a completion function for `completing-read'.

REGEX determines what expressions should offer a tag value as
a completion.  Usually either `tmsu--key-value-regex' or
`tmsu--comparison-regex'.

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
  (= (call-process "tmsu" nil nil nil "info") 0))

(defun tmsu-tags-add (file &rest tags)
  "Add TAGS to FILE."
  (apply #'call-process
         "tmsu" nil nil nil
         "tag" "--explicit" "--" file
         tags))

(defun tmsu-tags-remove (file &rest tags)
  "Remove TAGS from FILE."
  (apply #'call-process
         "tmsu" nil nil nil
         "untag" "--" file
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
         (tags-old (if tmsu-priority-tag
                       (sort (tmsu-get-tags file)
                             (lambda (a b)
                               (or (string< a b)
                                   (string-prefix-p tmsu-priority-tag b))))
                     (tmsu-get-tags file)))
         (tags-new (completing-read-multiple (format-message "Tag `%s': " file-name)
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
             (mapconcat #'identity
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
