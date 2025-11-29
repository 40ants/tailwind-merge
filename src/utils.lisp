(uiop:define-package #:tailwind-merge/utils
  (:use #:cl)
  (:export #:concat-arrays))
(in-package #:tailwind-merge/utils)

;;; Utility functions

(defun concat-arrays (array1 array2)
  "Concatenate two arrays/lists efficiently."
  (append array1 array2))

