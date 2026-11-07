(uiop:define-package #:tailwind-merge-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:tailwind-merge-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2026-11-07
         "* Fixed class parsing error for simple classes without the validators.")
  (0.1.0 2023-02-05
         "* Initial version."))
