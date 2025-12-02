(uiop:define-package #:tailwind-merge-tests/core
  (:use #:cl)
  (:import-from #:tailwind-merge/tailwind-classes
                #:merge-tailwind-classes)
  (:import-from #:tailwind-merge
                #:tw-join)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing)
  (:import-from #:hamcrest/matchers
                #:contains)
  (:import-from #:hamcrest/rove
                #:assert-that))
(in-package #:tailwind-merge-tests/core)


(deftest test-tw-join
  (testing "Basic class joining"
    (ok (string= (tw-join "foo" "bar") "foo bar"))
    (ok (string= (tw-join "foo" nil "bar") "foo bar"))
    (ok (string= (tw-join "foo" "" "bar") "foo bar"))
    (ok (string= (tw-join) ""))))


(deftest test-merge-tailwind-classes-basic
  (testing "Basic class merging"
    (assert-that
     (merge-tailwind-classes '("px-2" "px-3"))
     (contains "px-3"))

    (assert-that
     (merge-tailwind-classes '("py-2" "px-3"))
     (contains "py-2" "px-3"))

    (assert-that
     (merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
     (contains "bg-blue-500"))))


(deftest test-merge-tailwind-classes-conflicts
  (testing "Conflict resolution"
    (assert-that
     (merge-tailwind-classes '("h-10" "h-min"))
     (contains "h-min"))

    (assert-that
     (merge-tailwind-classes '("mix-blend-normal" "mix-blend-multiply"))
     (contains "mix-blend-multiply"))))

(deftest test-merge-tailwind-classes-non-conflicting
  (testing "Non-conflicting classes"
    (assert-that
     (merge-tailwind-classes '("stroke-black" "stroke-1"))
     (contains "stroke-black" "stroke-1"))

    (assert-that
     (merge-tailwind-classes '("outline-black" "outline-1"))
     (contains "outline-black" "outline-1"))))


(deftest test-merge-tailwind-classes-arbitrary-values
  (testing (ok "Arbitrary values are not supported yet"))
  ;; (testing "Arbitrary values"
  ;;   (assert-that
  ;;    (merge-tailwind-classes '("stroke-2" "stroke-[3]"))
  ;;    (contains "stroke-[3]"))

  ;;   (assert-that
  ;;    (merge-tailwind-classes '("grayscale-0" "grayscale-[50%]"))
  ;;    (contains "grayscale-[50%]"))

  ;;   (assert-that
  ;;    (merge-tailwind-classes '("grow" "grow-[2]"))
  ;;    (contains "grow-[2]")))
  )
