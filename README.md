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

INSTALLATION
------------

To install `tmsu.el` using `straight.el`, use the following code:

```elisp
(use-package tmsu
  :straight (:host github :repo "vifon/tmsu.el")
  :after dired
  :bind (:map dired-mode-map
         (";" . tmsu-dired-edit)
         ("M-;" . tmsu-dired-query))
  :config (require 'tmsu-dired))
```

If you prefer to attach your TMSU tags to directories and not single
files (think: a directory with a set of movies with common tags), use
`tmsu-dired-edit-dwim` instead of `tmsu-dired-edit`.  See their
docstrings for the details.
