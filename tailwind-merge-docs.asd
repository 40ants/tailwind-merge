(defsystem "tailwind-merge-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/tailwind-merge/"
  :class :package-inferred-system
  :description "Provides documentation for tailwind-merge."
  :source-control (:git "https://github.com/40ants/tailwind-merge")
  :bug-tracker "https://github.com/40ants/tailwind-merge/issues"
  :pathname "docs"
  :depends-on ("tailwind-merge"
               "tailwind-merge-docs/index"))
