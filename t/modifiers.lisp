(uiop:define-package #:tailwind-merge-tests/modifiers
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
                #:assert-that))
(in-package #:tailwind-merge-tests/modifiers)


(deftest test-modifier-parsing ()
  (testing "Modifier parsing"
    (ok (string= (tailwind-merge/modifiers::parse-modifier "hover:bg-red-500") "hover"))
    (ok (string= (tailwind-merge/modifiers::parse-modifier "focus:p-4") "focus"))
    (ok (string= (tailwind-merge/modifiers::parse-modifier "md:p-2") "md"))
    (ok (string= (tailwind-merge/modifiers::parse-modifier "group-hover:p-4") "group-hover"))
    (ok (null (tailwind-merge/modifiers::parse-modifier "p-4")))
    (ok (null (tailwind-merge/modifiers::parse-modifier "bg-red-500")))
    ;; Test complex modifier with nested brackets and parentheses
    (ok (string= (tailwind-merge/modifiers::parse-modifier "[@supports(display:grid)]:grid") "[@supports(display:grid)]"))
    ;; Test multiple nested modifiers
    (ok (string= (tailwind-merge/modifiers::parse-modifier "hover:[@media(min-width:640px)]:p-4") "hover:[@media(min-width:640px)]"))
    ;; Test modifier with arbitrary values
    (ok (string= (tailwind-merge/modifiers::parse-modifier "[hover]:p-4") "[hover]"))))


(deftest test-merge-tailwind-classes-with-modifiers ()
  (testing "Merging classes with modifiers"
    ;; Non-conflicting modifiers should be preserved
    (let ((result (merge-tailwind-classes '("p-2" "hover:p-4"))))
      (assert-that result (contains "p-2" "hover:p-4")))
    
    ;; Conflicting modifiers with same base class should be resolved to the last one
    (let ((result (merge-tailwind-classes '("hover:p-2" "hover:p-4"))))
      (assert-that result (contains "hover:p-4")))
    
    ;; Different modifiers with same base class should be preserved
    (let ((result (merge-tailwind-classes '("hover:p-2" "focus:p-4"))))
      (assert-that result (contains "hover:p-2" "focus:p-4")))
    
    ;; Complex modifiers like group-hover
    (let ((result (merge-tailwind-classes '("group-hover:p-2" "group-hover:p-4"))))
      (assert-that result (contains "group-hover:p-4")))
    
    ;; Order should matter - last one wins
    (let ((result (merge-tailwind-classes '("p-4" "hover:p-2" "p-3" "hover:p-5"))))
      (assert-that result (contains "p-3" "hover:p-5")))

    ;; Test that classes with same base class and same sorted modifier chain conflict
    ;; hover:focus:p-2 and focus:hover:p-4 have the same sorted modifier chain "focus:hover"
    ;; so they should conflict and the last one should win
    (let ((result (merge-tailwind-classes '("hover:focus:p-2" "focus:hover:p-4"))))
      (assert-that result (contains "focus:hover:p-4")))

    ;; Test pseudo variant group conflicts
    ;; group-empty:p-2 and group-empty:p-3 should conflict
    (let ((result (merge-tailwind-classes '("group-empty:p-2" "group-empty:p-3"))))
      (assert-that result (contains "group-empty:p-3")))
    
    ;; peer-empty:p-2 and peer-empty:p-3 should conflict
    (let ((result (merge-tailwind-classes '("peer-empty:p-2" "peer-empty:p-3"))))
      (assert-that result (contains "peer-empty:p-3")))
    
    ;; group-empty:p-2 and peer-empty:p-3 should not conflict (different modifiers)
    (let ((result (merge-tailwind-classes '("group-empty:p-2" "peer-empty:p-3"))))
      (assert-that result (contains "group-empty:p-2" "peer-empty:p-3")))
    
    ;; hover:group-empty:p-2 and hover:group-empty:p-3 should conflict
    (let ((result (merge-tailwind-classes '("hover:group-empty:p-2" "hover:group-empty:p-3"))))
      (assert-that result (contains "hover:group-empty:p-3")))
    
    ;; group-read-only:p-2 and group-read-only:p-3 should conflict
    (let ((result (merge-tailwind-classes '("group-read-only:p-2" "group-read-only:p-3"))))
      (assert-that result (contains "group-read-only:p-3")))))
