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
  (require 'cl-lib))
  (require 'subr-x)


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


(defun tmsu--get-tags (&optional file)
  (split-string-shell-command
   (string-trim-right
    (shell-command-to-string
     (apply #'concat "tmsu tags --name=never --explicit"
            (when file
              (list " " (shell-quote-argument file))))))))

(defun tmsu--get-values (&optional tag)
  (split-string-shell-command
   (string-trim-right
    (shell-command-to-string
     (apply #'concat "tmsu values"
            (when tag
              (list " " (shell-quote-argument tag))))))))

(defconst tmsu--key-value-regex
  (rx bos
      (group (+ (not "=")))
      "="
      (group (* any))))

(defun tmsu--completion (&optional tags)
  (setq tags (or tags (tmsu--get-tags)))
  (completion-table-dynamic
   (lambda (string)
     (string-match tmsu--key-value-regex string)
     (let* ((key (match-string 1 string))
            ;; (value (match-string 2 string))
            (candidates (if key
                            (mapcar
                             (lambda (value)
                               (concat key "=" value))
                             (tmsu--get-values key))
                          tags)))
       candidates))))

(defun tmsu-database-p ()
  "Check whether a TMSU database exists for the current directory."
  (= (call-process "tmsu" nil nil nil "info") 0))

;;;###autoload
(defun tmsu-edit (file)
  "Interactively edit the TMSU tags of FILE."
  (interactive "f")
  (unless (tmsu-database-p)
    (error "No TMSU database"))
  (let* ((tags-all (tmsu--get-tags))
         (tags-old (if tmsu-priority-tag
                       (sort (tmsu--get-tags file)
                             (lambda (a b)
                               (or (string< a b)
                                   (string-prefix-p tmsu-priority-tag b))))
                     (tmsu--get-tags file)))
         (tags-new (completing-read-multiple "Tags: "
                                             (tmsu--completion tags-all)
                                             nil nil
                                             (string-join tags-old ",")))
         (tags-added (cl-set-difference tags-new tags-old :test #'string=))
         (tags-removed (cl-set-difference tags-old tags-new :test #'string=)))
    (unless (or tags-added
                tags-removed)
      (user-error "No changes"))
    (apply #'call-process
           "tmsu" nil nil nil
           "untag" file
           tags-removed)
    (apply #'call-process
           "tmsu" nil nil nil
           "tag" "--explicit" file
           tags-added)
    (message "%S: %s"
             (tmsu--get-tags file)
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
