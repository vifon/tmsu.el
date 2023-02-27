tmsu.el
=======

An integration between GNU Emacs and [TMSU](https://tmsu.org/).

FEATURES
--------

`tmsu.el` doesn't try to replicate the full functionality of TMSU.
Instead it focuses on enhancing the UX of the most common operations:
tag editing and querying.  It's primarily intended to be used from
`dired`, though `tmsu-edit` can be used separately from it.

The two main commands are `tmsu-dired-edit` to interactively edit the
tag list of the currently selected file and `tmsu-dired-query` to
create a `dired` buffer with the TMSU query results.  Both utilize the
Emacs `completing-read-multiple` interface and so greatly benefit from
packages such as `vertico`.

### Persistent queries

The queries can be stored for later recall either with the
[native Emacs bookmarks][1] or with `org-mode` links using
[org-store-link][2].

[1]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html
[2]: https://orgmode.org/manual/Handling-Links.html

The first type is supported out of the box.  The second one needs to be loaded:

```elisp
(require 'ol-tmsu)
```

### Bulk operations (or the lack of them)

Tagging many files at the same time is deliberately not supported.

`dired-do-shell-command` (bound to <kbd>!</kbd> by default) can
already run a command like `tmsu tag ? year=2000 genre=comedy` on the
marked files.  Wrapping such command would be possible but not
necessarily substantially beneficial.  Specifically there is no
meaningful concept of the "current tags" for multiple files with
possibly different tags, so the UI used for the single files cannot
be reused.

INSTALLATION
------------

To install `tmsu.el`, you can use the following code:

```elisp
(use-package tmsu
  :ensure t
  :after dired
  :bind (:map dired-mode-map
         (";" . tmsu-dired-edit)
         ("M-;" . tmsu-dired-query))
  :config (require 'tmsu-dired))
```

If you prefer to attach your TMSU tags to directories and not single
files (think: a directory with a set of movies with common tags), use
`tmsu-dired-edit-directory` instead of `tmsu-dired-edit`.  See their
docstrings for the details.

For the `org-mode` links support, this is the suggested setup:

```elisp
(use-package ol-tmsu
  :ensure t
  :after (:any org tmsu-dired)
  :if (executable-find "tmsu"))
```

**Orderless compatibility**

If you're using [Orderless](https://github.com/oantolin/orderless)
with a style dispatcher using `=` as a suffix (which is the default
since early 2023 when `orderless-affix-dispatch` got added), you may
want to add the following snippet:

```elisp
(defun call-without-orderless-dispatchers (orig &rest args)
  "Use with `advice-add' (`:around') to ignore the dispatchers."
  (let ((orderless-style-dispatchers nil))
    (apply orig args)))

(advice-add 'tmsu-dired-edit :around
            #'call-without-orderless-dispatchers)
(advice-add 'tmsu-dired-edit-directory :around
            #'call-without-orderless-dispatchers)
(advice-add 'tmsu-dired-query :around
            #'call-without-orderless-dispatchers)
```

This modification allows the completion suggestions for inputs like
`tag=` to show up immediately and not only after the next keypress
(that makes the input not end with `=` anymore).
