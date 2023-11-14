(require 'ert)
(require 'pcase)

(require 'tmsu)


;;; Mocks and helpers

(defvar tmsu-mock-tags
  '("genre" "year" "staff"))

(defvar tmsu-mock-values
  '(("genre" . ("comedy" "cyberpunk" "action" "horror"))
    ("year" . ("1979" "1991"
               "2001" "2003" "2009"
               "2011"
               "2023"))
    ("staff" . ("Makoto Shinkai" "Gen Urobuchi"))))

(defun tmsu-mock-get-tags ()
  tmsu-mock-tags)

(defun tmsu-mock-get-values (&optional tag)
  (if tag
      (cdr (assoc tag tmsu-mock-values))
    ;; Do *not* replace with mapcan, it's destructive.
    (apply #'append (mapcar #'cdr tmsu-mock-values))))

(defmacro tmsu-with-mocks (&rest body)
  "Test BODY with the low-level TMSU functions mocked.

If the TMSU_TEST_MEDIA_DIR env var is set, additionally test on
the live TMSU database this variable points to."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'tmsu-get-tags) #'tmsu-mock-get-tags)
             ((symbol-function 'tmsu-get-values) #'tmsu-mock-get-values))
     ,@body)
  (when (getenv "TMSU_TEST_MEDIA_DIR")
    `(let ((default-directory (getenv "TMSU_TEST_MEDIA_DIR")))
       (warn "Testing TMSU on live data: %s" default-directory)
       ,@body)))


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
                       expected)))
       (_ (error "Bad test spec"))))
   test-specs))


(ert-deftest tmsu-test-escaping ()
  (should (equal (tmsu--escape-value "foo bar") "foo\\ bar"))
  (should (equal (tmsu--escape-value "foo\\ bar") "foo\\\\\\ bar"))
  (should (equal (tmsu--escape-value "foo <bar>") "foo\\ \\<bar\\>"))
  (should (equal (tmsu--escape-value "foo (bar)") "foo\\ \\(bar\\)")))

(ert-deftest tmsu-test-completion-assignment ()
  (tmsu-with-mocks
    (tmsu-test-completions
     (tmsu-completion-table-assignment)
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
  (tmsu-with-mocks
    (tmsu-test-completions
     (tmsu-completion-table-expression)
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
