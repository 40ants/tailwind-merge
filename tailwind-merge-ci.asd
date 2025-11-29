(defsystem "tailwind-merge-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/tailwind-merge/"
  :class :package-inferred-system
  :description "Provides CI settings for tailwind-merge."
  :source-control (:git "https://github.com/40ants/tailwind-merge")
  :bug-tracker "https://github.com/40ants/tailwind-merge/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "tailwind-merge-ci/ci"))
