(uiop:define-package #:tailwind-merge-tests/merger
  (:use #:cl)
  (:import-from #:tailwind-merge/merger
                #:merge-tailwind-classes)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing)
  (:import-from #:hamcrest/matchers
                #:contains)
  (:import-from #:hamcrest/rove
                #:assert-that)
  (:import-from #:tailwind-merge/tailwind-classes
                #:parse-class))
(in-package #:tailwind-merge-tests/merger)


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
  (testing "Arbitrary values"
    (assert-that
     (merge-tailwind-classes '("stroke-2" "stroke-[3]"))
     (contains "stroke-[3]"))

    (assert-that
     (merge-tailwind-classes '("grayscale-0" "grayscale-[50%]"))
     (contains "grayscale-[50%]"))

    (assert-that
     (merge-tailwind-classes '("grow" "grow-[2]"))
     (contains "grow-[2]"))))


(deftest test-real-classes-merge
  (assert-that (merge-tailwind-classes '("text-stone-800" "text-red-200"))
               (contains "text-red-200"))
  (assert-that (merge-tailwind-classes '("border-slate-600" "border-white"))
               (contains "border-white"))
  ;; (assert-that (merge-tailwind-classes '("text-stone-800" "dark:text-stone-300"))
  ;;              (contains "text-stone-800" "dark:text-stone-300"))
  ;; (assert-that
  ;;  (merge-tailwind-classes '("flex" "rounded-xl" "overflow-hidden" "text-stone-800" "dark:text-stone-300" "p-4" "justify-center" "items-center" "border" "border-stone-300" "dark:border-stone-600" "w-full" "min-h-[120px]"))
  ;;  (contains "flex rounded-xl overflow-hidden text-stone-800 dark:text-stone-300 p-4 justify-center items-center border border-stone-300 dark:border-stone-600 w-full min-h-[120px]"))
  )


(deftest test-parse-class ()
  (ok (null (parse-class "flex")))
  
  (ok (eql (parse-class "flex-col")
           :flex-direction)))
