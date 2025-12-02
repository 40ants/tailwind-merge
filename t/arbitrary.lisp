(uiop:define-package #:tailwind-merge-tests/arbitrary
  (:use #:cl)
  (:import-from #:tailwind-merge/arbitrary
                #:arbitrary-value-p
                #:arbitrary-variable-p
                #:any-non-arbitrary-p
                #:arbitrary-length-p
                #:arbitrary-number-p
                #:arbitrary-position-p
                #:arbitrary-image-p
                #:arbitrary-shadow-p
                #:arbitrary-size-p
                #:arbitrary-variable-length-p
                #:arbitrary-variable-position-p
                #:arbitrary-variable-image-p
                #:arbitrary-variable-shadow-p
                #:arbitrary-variable-size-p
                #:arbitrary-variable-family-name-p
                #:parse-arbitrary-value
                #:parse-arbitrary-variable)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:tailwind-merge-tests/arbitrary)


;;; ============================================================================
;;; Parse Functions Tests
;;; ============================================================================

(deftest test-parse-arbitrary-value
  (testing "Basic parsing of [content]"
    (multiple-value-bind (matched label content)
        (parse-arbitrary-value "[3]")
      (ok matched)
      (ok (null label))
      (ok (string= content "3"))))

  (testing "Parsing with label [label:content]"
    (multiple-value-bind (matched label content)
        (parse-arbitrary-value "[length:100px]")
      (ok matched)
      (ok (string= label "length"))
      (ok (string= content "100px"))))

  (testing "Label with dash [bg-size:cover]"
    (multiple-value-bind (matched label content)
        (parse-arbitrary-value "[bg-size:cover]")
      (ok matched)
      (ok (string= label "bg-size"))
      (ok (string= content "cover"))))

  (testing "Returns NIL for non-arbitrary values"
    (ok (not (parse-arbitrary-value "px-4")))
    (ok (not (parse-arbitrary-value "text-red-500")))
    (ok (not (parse-arbitrary-value "(--var)")))))


(deftest test-parse-arbitrary-variable
  (testing "Basic parsing of (content)"
    (multiple-value-bind (matched label content)
        (parse-arbitrary-variable "(--spacing-lg)")
      (ok matched)
      (ok (null label))
      (ok (string= content "--spacing-lg"))))

  (testing "Parsing with label (label:content)"
    (multiple-value-bind (matched label content)
        (parse-arbitrary-variable "(length:--spacing)")
      (ok matched)
      (ok (string= label "length"))
      (ok (string= content "--spacing"))))

  (testing "Returns NIL for non-variable values"
    (ok (not (parse-arbitrary-variable "[3]")))
    (ok (not (parse-arbitrary-variable "px-4")))))


;;; ============================================================================
;;; Basic Checks Tests
;;; ============================================================================

(deftest test-arbitrary-value-p
  (testing "Recognizes arbitrary values"
    (ok (arbitrary-value-p "[3]"))
    (ok (arbitrary-value-p "[20px]"))
    (ok (arbitrary-value-p "[length:100]"))
    (ok (arbitrary-value-p "[url(img.png)]"))
    (ok (arbitrary-value-p "[#ff0000]")))

  (testing "Rejects non-arbitrary values"
    (ok (not (arbitrary-value-p "px-4")))
    (ok (not (arbitrary-value-p "(--var)")))
    (ok (not (arbitrary-value-p nil)))
    (ok (not (arbitrary-value-p "")))))


(deftest test-arbitrary-variable-p
  (testing "Recognizes arbitrary variables"
    (ok (arbitrary-variable-p "(--var)"))
    (ok (arbitrary-variable-p "(--spacing-lg)"))
    (ok (arbitrary-variable-p "(length:--x)")))

  (testing "Rejects non-variable values"
    (ok (not (arbitrary-variable-p "[3]")))
    (ok (not (arbitrary-variable-p "px-4")))
    (ok (not (arbitrary-variable-p nil)))))


(deftest test-any-non-arbitrary-p
  (testing "Recognizes non-arbitrary values"
    (ok (any-non-arbitrary-p "px-4"))
    (ok (any-non-arbitrary-p "text-red-500"))
    (ok (any-non-arbitrary-p "auto")))

  (testing "Rejects arbitrary values and variables"
    (ok (not (any-non-arbitrary-p "[3]")))
    (ok (not (any-non-arbitrary-p "(--var)")))))


;;; ============================================================================
;;; Typed Arbitrary Value Tests
;;; ============================================================================

(deftest test-arbitrary-length-p
  (testing "Recognizes length values by content"
    (ok (arbitrary-length-p "[20px]"))
    (ok (arbitrary-length-p "[2rem]"))
    (ok (arbitrary-length-p "[50%]"))
    (ok (arbitrary-length-p "[100vw]"))
    (ok (arbitrary-length-p "[10vh]"))
    (ok (arbitrary-length-p "[1.5em]"))
    (ok (arbitrary-length-p "[0]")))

  (testing "Recognizes length values by label"
    (ok (arbitrary-length-p "[length:100]"))
    (ok (arbitrary-length-p "[length:auto]")))

  (testing "Recognizes calc/min/max functions"
    (ok (arbitrary-length-p "[calc(100%-20px)]"))
    (ok (arbitrary-length-p "[min(100px,50%)]"))
    (ok (arbitrary-length-p "[max(10rem,100px)]")))

  (testing "Rejects non-length values"
    (ok (not (arbitrary-length-p "[3]")))
    (ok (not (arbitrary-length-p "[red]")))
    (ok (not (arbitrary-length-p "px-4")))))


(deftest test-arbitrary-number-p
  (testing "Recognizes number values by content"
    (ok (arbitrary-number-p "[3]"))
    (ok (arbitrary-number-p "[0.5]"))
    (ok (arbitrary-number-p "[100]"))
    (ok (arbitrary-number-p "[-5]"))
    (ok (arbitrary-number-p "[1.25]")))

  (testing "Recognizes number values by label"
    (ok (arbitrary-number-p "[number:500]"))
    (ok (arbitrary-number-p "[number:abc]")))

  (testing "Rejects non-number values"
    (ok (not (arbitrary-number-p "[20px]")))
    (ok (not (arbitrary-number-p "[red]")))
    (ok (not (arbitrary-number-p "3")))))


(deftest test-arbitrary-position-p
  (testing "Recognizes position values by label only"
    (ok (arbitrary-position-p "[position:center]"))
    (ok (arbitrary-position-p "[position:top_left]"))
    (ok (arbitrary-position-p "[percentage:50%]")))

  (testing "Rejects unlabeled values (position requires explicit label)"
    (ok (not (arbitrary-position-p "[center]")))
    (ok (not (arbitrary-position-p "[top]")))))


(deftest test-arbitrary-image-p
  (testing "Recognizes image values by content"
    (ok (arbitrary-image-p "[url(img.png)]"))
    (ok (arbitrary-image-p "[url('/path/to/image.jpg')]"))
    (ok (arbitrary-image-p "[linear-gradient(to_right,red,blue)]"))
    (ok (arbitrary-image-p "[radial-gradient(circle,red,blue)]"))
    (ok (arbitrary-image-p "[conic-gradient(red,blue)]")))

  (testing "Recognizes image values by label"
    (ok (arbitrary-image-p "[image:something]"))
    (ok (arbitrary-image-p "[url:something]")))

  (testing "Rejects non-image values"
    (ok (not (arbitrary-image-p "[20px]")))
    (ok (not (arbitrary-image-p "[red]")))))


(deftest test-arbitrary-shadow-p
  (testing "Recognizes shadow values by content pattern"
    (ok (arbitrary-shadow-p "[0_0_10px]"))
    (ok (arbitrary-shadow-p "[0_0]"))
    (ok (arbitrary-shadow-p "[2px_4px]"))
    (ok (arbitrary-shadow-p "[inset_0_0]"))
    (ok (arbitrary-shadow-p "[inset_2px_4px_black]")))

  (testing "Recognizes shadow values by label"
    (ok (arbitrary-shadow-p "[shadow:custom]")))

  (testing "Rejects non-shadow values"
    (ok (not (arbitrary-shadow-p "[20px]")))
    (ok (not (arbitrary-shadow-p "[red]")))))


(deftest test-arbitrary-size-p
  (testing "Recognizes size values by label only"
    (ok (arbitrary-size-p "[size:cover]"))
    (ok (arbitrary-size-p "[size:contain]"))
    (ok (arbitrary-size-p "[bg-size:100%_50%]"))
    (ok (arbitrary-size-p "[length:100px]")))

  (testing "Rejects unlabeled values (size requires explicit label)"
    (ok (not (arbitrary-size-p "[cover]")))
    (ok (not (arbitrary-size-p "[100%]")))))


;;; ============================================================================
;;; Typed Arbitrary Variable Tests
;;; ============================================================================

(deftest test-arbitrary-variable-length-p
  (testing "Recognizes length variables"
    (ok (arbitrary-variable-length-p "(length:--spacing)"))
    (ok (arbitrary-variable-length-p "(length:--x)")))

  (testing "Rejects non-length variables"
    (ok (not (arbitrary-variable-length-p "(--var)")))
    (ok (not (arbitrary-variable-length-p "(number:--x)")))))


(deftest test-arbitrary-variable-position-p
  (testing "Recognizes position variables"
    (ok (arbitrary-variable-position-p "(position:--pos)"))
    (ok (arbitrary-variable-position-p "(percentage:--pct)")))

  (testing "Rejects non-position variables"
    (ok (not (arbitrary-variable-position-p "(--var)")))
    (ok (not (arbitrary-variable-position-p "(length:--x)")))))


(deftest test-arbitrary-variable-image-p
  (testing "Recognizes image variables"
    (ok (arbitrary-variable-image-p "(image:--bg)"))
    (ok (arbitrary-variable-image-p "(url:--img)")))

  (testing "Rejects non-image variables"
    (ok (not (arbitrary-variable-image-p "(--var)")))
    (ok (not (arbitrary-variable-image-p "(length:--x)")))))


(deftest test-arbitrary-variable-shadow-p
  (testing "Recognizes shadow variables"
    (ok (arbitrary-variable-shadow-p "(shadow:--shadow)"))
    ;; Shadow variables match without label by default
    (ok (arbitrary-variable-shadow-p "(--shadow)")))

  (testing "Rejects labeled non-shadow variables"
    (ok (not (arbitrary-variable-shadow-p "(length:--x)")))))


(deftest test-arbitrary-variable-size-p
  (testing "Recognizes size variables"
    (ok (arbitrary-variable-size-p "(size:--sz)"))
    (ok (arbitrary-variable-size-p "(bg-size:--bg)"))
    (ok (arbitrary-variable-size-p "(length:--len)")))

  (testing "Rejects non-size variables"
    (ok (not (arbitrary-variable-size-p "(--var)")))
    (ok (not (arbitrary-variable-size-p "(number:--x)")))))


(deftest test-arbitrary-variable-family-name-p
  (testing "Recognizes family-name variables"
    (ok (arbitrary-variable-family-name-p "(family-name:--font)")))

  (testing "Rejects non-family-name variables"
    (ok (not (arbitrary-variable-family-name-p "(--var)")))
    (ok (not (arbitrary-variable-family-name-p "(length:--x)")))))
