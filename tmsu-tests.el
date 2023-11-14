(require 'ert)
(require 'pcase)

(require 'tmsu)

(ert-deftest tmsu-test-escaping ()
  (should (equal (tmsu--escape-value "foo bar") "foo\\ bar"))
  (should (equal (tmsu--escape-value "foo\\ bar") "foo\\\\\\ bar"))
  (should (equal (tmsu--escape-value "foo <bar>") "foo\\ \\<bar\\>"))
  (should (equal (tmsu--escape-value "foo (bar)") "foo\\ \\(bar\\)")))

(defun tmsu-test-completions (completion-table test-specs)
  (mapc
   (lambda (spec)
     (pcase spec
       (`(,input t)
        (should (try-completion input completion-table)))
       (`(,input nil)
        (should-not (try-completion input completion-table)))
       (`(,input (member ,expected))
        (should (member
                 expected
                 (all-completions input completion-table))))
       (`(,input (member-not ,expected))
        (should-not (member
                     expected
                     (all-completions input completion-table))))
       (`(,input ,expected)
        (should (equal (try-completion input completion-table)
                       expected)))))
   test-specs))

(ert-deftest tmsu-test-completion-assignment ()
  (skip-unless (getenv "TMSU_TEST_MEDIA_DIR"))
  (let* ((default-directory (getenv "TMSU_TEST_MEDIA_DIR"))
         (completion-table (tmsu-completion-table-assignment)))
    (tmsu-test-completions
     completion-table
     '(("gen"            "genre")
       ("genre="         "genre=")
       ("genre = "       nil)
       ("not gen"        nil)
       ("genre=com"      "genre=comedy")
       ("year=19"        t)
       ("year=19"        (member     "year=1979"))
       ("year=19"        (member-not "year=2003"))
       ("year<19"        nil)
       ("year>19"        nil)
       ("year < 19"      nil)
       ("year > 19"      nil)
       ("year <= 19"     nil)
       ("year >= 19"     nil)
       ("staff=Makoto"   "staff=Makoto Shinkai")))))

(ert-deftest tmsu-test-completion-query ()
  (skip-unless (getenv "TMSU_TEST_MEDIA_DIR"))
  (let* ((default-directory (getenv "TMSU_TEST_MEDIA_DIR"))
         (completion-table (tmsu-completion-table-expression)))
    (tmsu-test-completions
     completion-table
     '(("gen"            "genre")
       ("genre="         "genre=")
       ("genre = "       "genre = ")
       ("not gen"        "not genre")
       ("genre=com"      "genre=comedy")
       ("not genre=com"  "not genre=comedy")
       ("year=19"        t)
       ("year=19"        (member     "year=1979"))
       ("year=19"        (member-not "year=2003"))
       ("year<19"        t)
       ("year>19"        t)
       ("year < 19"      t)
       ("year > 19"      t)
       ("year <= 19"     t)
       ("year >= 19"     t)
       ("staff=Makoto"   "staff=Makoto\\ Shinkai")))))
