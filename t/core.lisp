(uiop:define-package #:tailwind-merge-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:tailwind-merge-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
