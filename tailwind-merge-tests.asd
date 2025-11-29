(defsystem "tailwind-merge-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/tailwind-merge/"
  :class :package-inferred-system
  :description "Provides tests for tailwind-merge."
  :source-control (:git "https://github.com/40ants/tailwind-merge")
  :bug-tracker "https://github.com/40ants/tailwind-merge/issues"
  :pathname "t"
  :depends-on ("tailwind-merge-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
