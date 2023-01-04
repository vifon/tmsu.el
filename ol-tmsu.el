;;; ol-tmsu.el --- Org-mode links to TMSU queries    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: files, outlines, hypermedia
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

;; org-mode links to TMSU queries.

;;; Code:

(require 'ol)

(eval-when-compile
  (require 'subr-x))

(defconst org-tmsu-link-regex
  (rx (group (*? any))
      "::"
      (group (*? any))
      (? "::"
         (group (* any)))
      eol)
  "A regex matching an `org-mode' link this module generates.")

(defun org-tmsu-open-link (link &optional arg)
  "Implement the :follow handler for `org-store-link'."
  (require 'tmsu-dired)
  (string-match org-tmsu-link-regex link)
  (let ((dir   (match-string 1 link))
        (query (match-string 2 link))
        (flags (match-string 3 link)))
    (tmsu-dired-query dir (split-string query ",") flags)))

(defun org-tmsu-store-link ()
  "Implement the :store handler for `org-store-link'."
  (when (and (eq major-mode 'dired-mode)
             (bound-and-true-p tmsu-query))
    (let* ((dir default-directory)
           (query (string-join tmsu-query ","))
           (flags tmsu-flags)
           (link (if flags
                     (concat "tmsu:" dir "::" query "::" flags)
                   (concat "tmsu:" dir "::" query)))
           (desc (concat "tmsu:"
                         query
                         " @ "
                         (file-name-nondirectory
                          (directory-file-name
                           default-directory)))))
      (org-link-store-props :type "tmsu"
                            :link link
                            :description desc
                            :directory dir
                            :query query))))

(org-link-set-parameters
 "tmsu"
 :follow #'org-tmsu-open-link
 :store #'org-tmsu-store-link)

(provide 'ol-tmsu)
;;; ol-tmsu.el ends here
