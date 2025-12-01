(uiop:define-package #:tailwind-merge-tests/core
  (:use #:cl)
  (:import-from #:tailwind-merge/tailwind-classes
                #:merge-tailwind-classes)
  (:import-from #:tailwind-merge
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

(deftest test-merge-tailwind-classes-basic
  (testing "Basic class merging"
    (ok (equal (merge-tailwind-classes '("px-2" "px-3")) '("px-3")))
    (ok (string= (merge-tailwind-classes "py-2" "px-3") "py-2 px-3"))
    (ok (string= (merge-tailwind-classes "bg-red-500" "bg-blue-500") "bg-blue-500"))))

(deftest test-merge-tailwind-classes-conflicts
  (testing "Conflict resolution"
    (ok (equal (merge-tailwind-classes '("h-10" "h-min")) '("h-min")))
    (ok (equal (merge-tailwind-classes '("mix-blend-normal" "mix-blend-multiply")) '("mix-blend-multiply")))))

(deftest test-merge-tailwind-classes-non-conflicting
  (testing "Non-conflicting classes"
    (ok (equal (merge-tailwind-classes '("stroke-black" "stroke-1")) '("stroke-black" "stroke-1")))
    (ok (equal (merge-tailwind-classes '("outline-black" "outline-1")) '("outline-black" "outline-1")))))

(deftest test-merge-tailwind-classes-arbitrary-values
  (testing "Arbitrary values"
    (ok (equal (merge-tailwind-classes '("stroke-2" "stroke-[3]")) '("stroke-[3]")))
    (ok (equal (merge-tailwind-classes '("grayscale-0" "grayscale-[50%]")) '("grayscale-[50%]")))
    (ok (equal (merge-tailwind-classes '("grow" "grow-[2]")) '("grow-[2]")))))
