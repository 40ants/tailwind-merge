(uiop:define-package #:tailwind-merge-tests/core
  (:use #:cl)
  (:import-from #:tailwind-merge
                #:tw-merge
                #:tw-join)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:tailwind-merge-tests/core)


(deftest test-tw-join
  (testing "Basic class joining"
    (ok (string= (tw-join "foo" "bar") "foo bar"))
    (ok (string= (tw-join "foo" nil "bar") "foo bar"))
    (ok (string= (tw-join "foo" "" "bar") "foo bar"))
    (ok (string= (tw-join) ""))))

(deftest test-tw-merge-basic
  (testing "Basic class merging"
    (ok (string= (tw-merge "px-2" "px-3") "px-3"))
    ;; (ok (string= (tw-merge "py-2" "px-3") "py-2 px-3"))
    ;; (ok (string= (tw-merge "bg-red-500" "bg-blue-500") "bg-blue-500"))
    ))

(deftest test-tw-merge-conflicts
  (testing "Conflict resolution"
    (ok (string= (tw-merge "h-10" "h-min") "h-min"))
    (ok (string= (tw-merge "mix-blend-normal" "mix-blend-multiply") "mix-blend-multiply"))))

(deftest test-tw-merge-non-conflicting
  (testing "Non-conflicting classes"
    (ok (string= (tw-merge "stroke-black" "stroke-1") "stroke-black stroke-1"))
    (ok (string= (tw-merge "outline-black" "outline-1") "outline-black outline-1"))))

(deftest test-tw-merge-arbitrary-values
  (testing "Arbitrary values"
    (ok (string= (tw-merge "stroke-2" "stroke-[3]") "stroke-[3]"))
    (ok (string= (tw-merge "grayscale-0" "grayscale-[50%]") "grayscale-[50%]"))
    (ok (string= (tw-merge "grow" "grow-[2]") "grow-[2]"))))
