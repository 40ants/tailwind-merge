#-asdf3.1 (error "tailwind-merge requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "tailwind-merge"
  :description "Utility library to merge Tailwind CSS classes without style conflicts."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/tailwind-merge/"
  :source-control (:git "https://github.com/40ants/tailwind-merge")
  :bug-tracker "https://github.com/40ants/tailwind-merge/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("tailwind-merge/merger")
  :in-order-to ((test-op (test-op "tailwind-merge-tests"))))
