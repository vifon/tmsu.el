;;; tmsu-dired.el --- An interface between TMSU and Dired    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Wojciech Siewierski

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

;; An interface between TMSU and Dired.

;;; Code:

(require 'bookmark)
(require 'dired)

(require 'tmsu)


(defgroup tmsu-dired nil
  "An interface between TMSU and Dired."
  :group 'tmsu)


;;;###autoload
(defun tmsu-dired-edit (parent)
  "Edit the tags of the file at point.

If PARENT is non-nil, edit the current directory instead of
the file at point."
  (interactive "P" dired-mode)
  (let ((file-at-point (dired-get-file-for-visit)))
    (let ((file-to-edit
           (if parent
               (dired-current-directory)
             file-at-point)))
      (tmsu-edit file-to-edit))))

;;;###autoload
(defun tmsu-dired-edit-directory (force-edit-file)
  "Edit the tags of a directory at point.

If the file at point isn't a directory, operate on the
parent instead.

\\[universal-argument] sets FORCE-EDIT-FILE to t and allows
editing a regular file's tags."
  (interactive "P" dired-mode)
  (let ((file-at-point (dired-get-file-for-visit)))
    (let ((file-to-edit (if (or (file-directory-p file-at-point)
                                force-edit-file)
                            file-at-point
                          (dired-current-directory))))
      (tmsu-edit file-to-edit))))

(defvar tmsu-dired-tags-history nil
  "Command history for `tmsu-dired-tags-add' and `tmsu-dired-tags-remove'.")

;;;###autoload
(defun tmsu-dired-tags-add ()
  "Add tags to the marked files.

If no files are marked, uses the file at point."
  (interactive nil dired-mode)
  (unless (tmsu-database-p)
    (error "No TMSU database"))
  (let ((tags-to-add (completing-read-multiple
                      "Tags to add: "
                      (tmsu-completion-table-assignment)
                      nil nil nil
                      'tmsu-dired-tags-history)))
    (dolist (file (dired-get-marked-files))
      (apply #'tmsu-tags-add file tags-to-add))))

;;;###autoload
(defun tmsu-dired-tags-remove ()
  "Remove tags from the marked files.

If no files are marked, uses the file at point.

The completing read offers the sum of tags of all the marked files."
  (interactive nil dired-mode)
  (unless (tmsu-database-p)
    (error "No TMSU database"))
  (let* ((files (dired-get-marked-files))
         (tags-in-files (mapcan #'cdr (tmsu-get-tags-for-files files)))
         (tags-to-remove (completing-read-multiple
                          "Tags to remove: "
                          tags-in-files
                          nil t nil
                          'tmsu-dired-tags-history)))
    (dolist (file files)
      (apply #'tmsu-tags-remove file tags-to-remove))))


;; This section is strongly inspired by find-dired.el (included in GNU
;; Emacs) with lots of code being reused.  Please refer to that file
;; for the original authors.

(require 'find-dired)

(defvar tmsu-dired-ls-switches "-alh")
(defvar tmsu-dired-ls-subdir-switches nil)

(defvar-local tmsu-dired-query-args nil
  "The QUERY argument used for this buffer's `tmsu-dired-query' call.")
(defvar-local tmsu-dired-query-flags nil
  "The FLAGS argument used for this buffer's `tmsu-dired-query' call.")

(defvar-local tmsu-dired-goto nil
  "A position to move the point to after loading a `tmsu-dired-query' buffer.

It can also be a function which is called after finishing
a query.")

(defcustom tmsu-dired-buffer-name-function
  #'tmsu-dired-buffer-name
  "A function used to generate `tmsu-dired-query' buffer names.

Customize to change the buffer naming convention."
  :type '(choice
          (const :tag "Directory name with query and optional flags"
                 tmsu-dired-buffer-name)
          (const :tag "Directory name" tmsu-dired-buffer-name-simple)
          (function :tag "Custom")))

(defcustom tmsu-dired-query-in-subtree t
  "Limit the `tmsu-dired-query' results to the current subdirectory.

Set to nil to always search in the whole TMSU database."
  :type 'boolean)

(defun tmsu-dired-buffer-name (dir query flags)
  "The default implementation of `tmsu-dired-buffer-name-function'.

DIR, QUERY and FLAGS are the same arguments `tmsu-dired-query' got."
  (let ((query-pp (string-join (tmsu-dired--preprocess-query query)
                               " and "))
        (dir-name (file-name-nondirectory
                   (directory-file-name
                    dir))))
    (format "tmsu-query: %s @ %s%s"
            query-pp
            dir-name
            (if flags
                (concat " (" flags ")")
              ""))))

(defun tmsu-dired-buffer-name-simple (dir _query _flags)
  "An alternative implementation of `tmsu-dired-buffer-name-function'.

DIR is the same as the first argument of `tmsu-dired-query'."
  (let ((dir-name (file-name-nondirectory
                   (directory-file-name
                    dir))))
    (concat "tmsu-query: " dir-name)))

(defun tmsu-dired--preprocess-query (query)
  "Preprocess QUERY to ensure correct semantics.

The list representation of the query might need some steps for it
to have the expected semantics when combined into a single query.
One such case is wrapping the expressions containing the infix
operators in parentheses as the implicit `and' between each query
list element should have a lower priority.

The result *should not* be stored back as the internal
representation of the query, to avoid applying it multiple times
when recreating the query buffer from this
internal representation."
  (if (cdr query)
      (mapcar (lambda (expr)
                (if (string-match-p (rx space (| "or" "and") space) expr)
                    (concat "(" expr ")")
                  expr))
              query)
    ;; Single element queries are trivial, no need to do anything.
    query))

;;;###autoload
(defun tmsu-dired-query (&optional dir query flags)
  "Display the `tmsu' QUERY results as a `dired' buffer.

The query is anchored at DIR as the working directory.
Interactively it's always the current directory.

Interactively ask for the FLAGS only if \\[universal-argument] got passed."
  (interactive)

  (setq dir (or dir
                (if (eq major-mode 'dired-mode)
                    (dired-current-directory)
                  default-directory)))

  ;; We still didn't open a new buffer, so we can't just set
  ;; `default-directory', and we cannot open a new buffer first
  ;; either, as its name will be based on the values we're about to
  ;; read.  We can either set a local `default-directory' value or use
  ;; a temporary buffer name and rename it later.  The former seems
  ;; cleaner.  The key takeaway: we'll need to set `default-directory'
  ;; again a little lower.
  (let ((default-directory dir))
    (unless (tmsu-database-p)
      (error "No TMSU database"))

    (unless query
      (setq query (completing-read-multiple
                   "TMSU query: "
                   (tmsu-completion-table-expression)
                   nil nil nil 'tmsu-query-history
                   (when tmsu-dired-query-args
                     (string-join tmsu-dired-query-args ","))))))

  (when (and (not flags)
             current-prefix-arg)
    (setq flags (read-from-minibuffer "Additional `tmsu files' flags: ")))

  ;; Done preparing the interactive arguments!

  ;; Do not modify the global value of `dired-buffers'.
  ;; Otherwise `dired' might reuse the tmsu-dired-query-args buffers
  ;; when creating a fresh one would be more appropriate.
  (let ((dired-buffers dired-buffers)
        tmsu-command command
        switches)
    (pop-to-buffer-same-window (get-buffer-create
                                (funcall tmsu-dired-buffer-name-function
                                         dir query flags)))

    (setq default-directory dir)

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

    ;; TODO: Consider respecting `dired-use-ls-dired'.
    (setq switches (concat "--quoting-style=literal " tmsu-dired-ls-switches))

    (setq tmsu-command (format "tmsu files%s%s -0 -- %s"
                               (if flags
                                   (concat " " flags)
                                 "")
                               (if tmsu-dired-query-in-subtree
                                   (concat " --path "
                                           (shell-quote-argument
                                            (file-local-name dir)))
                                 "")
                               (mapconcat #'shell-quote-argument
                                          (tmsu-dired--preprocess-query query)
                                          " ")))
    (setq command (format "%s | xargs -0 %s -d %s"
                          tmsu-command
                          insert-directory-program
                          switches))

    (make-process :name "tmsu-query"
                  :command `("sh" "-c" ,command)
                  :buffer (current-buffer)
                  ;; `find-dired-filter' should do with no changes at all.
                  :filter #'find-dired-filter
                  ;; `find-dired-sentinel' needed minor changes, so there we go.
                  :sentinel #'tmsu-dired-sentinel
                  ;; Handle the remote (TRAMP) directories.
                  :file-handler t)
    (dired-mode dir tmsu-dired-ls-switches)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      ;; It should be safe to reuse `kill-find' verbatim.
      (define-key map "\C-c\C-k" #'kill-find)
      (use-local-map map))

    ;; For later buffer-local access.
    (setq-local tmsu-dired-query-args query)
    (setq-local tmsu-dired-query-flags flags)

    (setq-local bookmark-make-record-function #'tmsu-dired-bookmark-make-record)

    (setq-local dired-sort-inhibit nil)
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (let ((point (point))
                        (tmsu-dired-ls-switches dired-actual-switches))
                    (tmsu-dired-query dir query flags)
                    (setq-local tmsu-dired-goto point))))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v 1.15
        ;; and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (setq-local dired-subdir-alist
                  (list (cons dir (point-min-marker)))))
    (setq-local dired-subdir-switches tmsu-dired-ls-subdir-switches)

    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " (directory-file-name dir) ":\n")
    (when (bound-and-true-p dired-make-directory-clickable)
      (dired--make-directory-clickable))
    ;; Make second line a ``tmsu'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (let ((point (point)))
      (insert "  " (string-replace "-0 " "" tmsu-command) "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun tmsu-dired-sentinel (proc state)
  "Sentinel for \\[tmsu-dired-query] processes.

Enforces `tmsu-dired-goto'.

See `set-process-sentinel' for the details on the PROC and STATE arguments."
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
              (force-mode-line-update)))
          (goto-char (point-min))
          (dired-initial-position default-directory)
          (when tmsu-dired-goto
            (if (functionp tmsu-dired-goto)
                (funcall tmsu-dired-goto)
              (goto-char tmsu-dired-goto))))
      (message "tmsu-dired-query %s finished." buf))))



;;;###autoload
(defun tmsu-dired-bookmark-open (bookmark)
  "Handler for the BOOKMARKs created with `tmsu-dired-bookmark-make-record'."
  (let ((dir (bookmark-get-filename bookmark))
        (position (bookmark-get-position bookmark))
        (forward-str (bookmark-get-front-context-string bookmark))
        (behind-str  (bookmark-get-rear-context-string bookmark))
        (query (bookmark-prop-get bookmark 'tmsu-query))
        (flags (bookmark-prop-get bookmark 'tmsu-flags)))
    (tmsu-dired-query dir
                      query
                      flags)
    (setq-local tmsu-dired-goto
                (lambda ()
                  (when position
                    (goto-char position))
                  (when (and forward-str
                             (search-forward forward-str (point-max) t))
                    (goto-char (match-beginning 0)))
                  (when (and behind-str
                             (search-backward behind-str (point-min) t))
                    (goto-char (match-end 0)))))))

(defcustom tmsu-dired-pretty-description-function
  #'tmsu-dired-pretty-description
  "Function generating pretty-printed buffer names for `tmsu-dired-query'.

Used for the bookmark and link default name generation."
  :type 'function)

(defun tmsu-dired-pretty-description (&optional dir query flags)
  "The default implementation of `tmsu-dired-pretty-description-function'.

If any of the DIR, QUERY and FLAGS arguments are nil, the
respective value is being inferred from the current buffer."
  (setq dir   (or dir default-directory)
        query (tmsu-dired--preprocess-query (or query tmsu-dired-query-args))
        flags (or flags tmsu-dired-query-flags))
  (concat "tmsu:"
          (string-join query " and ")
          " @ "
          (file-name-nondirectory
           (directory-file-name
            dir))))

(defun tmsu-dired-bookmark-make-record ()
  "Implements `bookmark-make-record-function' for `tmsu-dired-query'."
  (let* ((query tmsu-dired-query-args)
         (flags tmsu-dired-query-flags)
         (desc (funcall tmsu-dired-pretty-description-function)))
    `(,desc
      ,@(bookmark-make-record-default)
      (handler . tmsu-dired-bookmark-open)
      (tmsu-query . ,query)
      (tmsu-flags . ,flags))))


(defun tmsu-dired-rename-repair (oldname newname &rest _)
  "Preserve the OLDNAME file TMSU tags when renaming it to NEWNAME."
  (ignore-errors
    (process-file "tmsu" nil nil nil
                  "repair" "-m"
                  (file-local-name oldname)
                  (file-local-name newname))))

;;;###autoload
(define-minor-mode tmsu-dired-rename-repair-mode
  "Preserve the TMSU tags upon file rename in `dired'."
  :global t
  :lighter " tmsu-dired-rename"
  (if tmsu-dired-rename-repair-mode
      (advice-add #'dired-rename-file :after
                  #'tmsu-dired-rename-repair)
    (advice-remove #'dired-rename-file
                   #'tmsu-dired-rename-repair)))

(provide 'tmsu-dired)
;;; tmsu-dired.el ends here
