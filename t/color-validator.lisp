(uiop:define-package #:tailwind-merge-tests/color-validator
  (:use #:cl)
  (:import-from #:tailwind-merge/validators
                #:colorp)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:tailwind-merge-tests/color-validator)


(deftest test-colorp-valid-colors
  (testing "Valid Tailwind CSS colors"
    (ok (colorp "slate-600"))
    (ok (colorp "red-500"))
    (ok (colorp "blue-300"))
    (ok (colorp "green-700"))
    (ok (colorp "black"))
    (ok (colorp "white"))
    (ok (colorp "transparent"))
    (ok (colorp "current"))
    (ok (colorp "inherit"))
    (ok (colorp "rose-950"))
    (ok (colorp "gray-100"))
    (ok (colorp "amber-400"))))


(deftest test-colorp-invalid-colors
  (testing "Invalid Tailwind CSS colors"
    (ok (not (colorp "invalid-color")))
    (ok (not (colorp "red-999")))  ; 999 is not a valid Tailwind shade
    (ok (not (colorp "purple-1000")))  ; 1000 is not a valid Tailwind shade
    (ok (not (colorp "nonexistent-500")))
    (ok (not (colorp "")))
    (ok (not (colorp "red-a00")))  ; invalid format
    (ok (not (colorp "100-red")))))  ; wrong order


(deftest test-colorp-case-insensitive
  (testing "Case insensitive matching"
    (ok (colorp "SLATE-600"))
    (ok (colorp "Red-500"))
    (ok (colorp "BLACK"))
    (ok (colorp "TRANSPARENT"))))