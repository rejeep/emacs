(When "^I call done$"
  (lambda ()
    (call-interactively 'done)))

(When "^I place the cursor after the word \"\\([^\"]+\\)\"$"
  (lambda (word)
    (beginning-of-buffer)
    (re-search-forward (concat "\\_<" word "\\_>"))))

(Then "^the cursor should be after the word \"\\([^\"]+\\)\"$"
  (lambda (word)
    (should (looking-back (concat "\\_<" word)))))
