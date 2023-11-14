(require 'ert)

(require 'tmsu)

(ert-deftest tmsu-test-escaping ()
  (should (equal (tmsu--escape-value "foo bar") "foo\\ bar"))
  (should (equal (tmsu--escape-value "foo\\ bar") "foo\\\\\\ bar"))
  (should (equal (tmsu--escape-value "foo <bar>") "foo\\ \\<bar\\>"))
  (should (equal (tmsu--escape-value "foo (bar)") "foo\\ \\(bar\\)")))

(ert-deftest tmsu-test-completion-assignment ()
  (skip-unless (getenv "TMSU_TEST_MEDIA_DIR"))
  (let* ((default-directory (getenv "TMSU_TEST_MEDIA_DIR"))
         (completion-table (tmsu--completion tmsu--key-value-regex)))
    (should (equal
             (try-completion "gen" completion-table)
             "genre"))
    (should-not (try-completion "not gen" completion-table))
    (should (equal
             (try-completion "genre=com" completion-table)
             "genre=comedy"))

    (should (try-completion "year=19" completion-table))
    (should (member
             "year=1979"
             (all-completions "year=19" completion-table)))
    (should-not (member
                 "year=2003"
                 (all-completions "year=19" completion-table)))

    (should-not (try-completion "year<19" completion-table))
    (should-not (try-completion "year>19" completion-table))
    (should-not (try-completion "year < 19" completion-table))
    (should-not (try-completion "year > 19" completion-table))
    (should-not (try-completion "year <= 19" completion-table))
    (should-not (try-completion "year >= 19" completion-table))))

(ert-deftest tmsu-test-completion-query ()
  (skip-unless (getenv "TMSU_TEST_MEDIA_DIR"))
  (let* ((default-directory (getenv "TMSU_TEST_MEDIA_DIR"))
         (completion-table (tmsu--completion tmsu--expression-regex)))
    (should (equal
             (try-completion "gen" completion-table)
             "genre"))
    (should (equal
             (try-completion "not gen" completion-table)
             "not genre"))
    (should (equal
             (try-completion "genre=com" completion-table)
             "genre=comedy"))
    (should (equal
             (try-completion "not genre=com" completion-table)
             "not genre=comedy"))

    (should (try-completion "year=19" completion-table))
    (should (member
             "year=1979"
             (all-completions "year=19" completion-table)))
    (should-not (member
                 "year=2003"
                 (all-completions "year=19" completion-table)))

    (should (try-completion "year<19" completion-table))
    (should (try-completion "year>19" completion-table))
    (should (try-completion "year < 19" completion-table))
    (should (try-completion "year > 19" completion-table))
    (should (try-completion "year <= 19" completion-table))
    (should (try-completion "year >= 19" completion-table))))
