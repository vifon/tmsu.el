(require 'ert)

(require 'tmsu)

(ert-deftest tmsu-test-escaping ()
  (should (equal (tmsu--escape-value "foo bar") "foo\\ bar"))
  (should (equal (tmsu--escape-value "foo\\ bar") "foo\\\\\\ bar"))
  (should (equal (tmsu--escape-value "foo <bar>") "foo\\ \\<bar\\>"))
  (should (equal (tmsu--escape-value "foo (bar)") "foo\\ \\(bar\\)")))
