(uiop:define-package #:tailwind-merge/validators
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:alexandria
                #:curry)
  (:import-from #:parse-number
                #:parse-number)
  (:import-from #:tailwind-merge/arbitrary
                #:arbitrary-value-p
                #:arbitrary-variable-p
                #:arbitrary-number-p
                #:arbitrary-length-p)
  (:export
   #:integer-value-p
   #:number-value-p
   #:fraction-value-p
   #:tshirt-size-p
   #:percent-value-p
   #:empty-or-number-p))

(in-package #:tailwind-merge/validators)


;;; ==================
;;; Validator Functions
;;; ==================

(-> integer-value-p ((or null string))
    (values boolean &optional))

(defun integer-value-p (value)
  "Matches integer values like 0, 1, 2, 100, etc."
  (when (and value
             (ignore-errors (parse-integer value)))
    (values t)))


(-> number-value-p ((or null string))
    (values boolean &optional))

(defun number-value-p (value)
  "Matches any number including decimals like 0.5, 1.5, etc."
  (when (and value
             (plusp (length value))
             (ignore-errors (parse-number value)))
    (values t)))


(-> fraction-value-p ((or null string))
    (values boolean &optional))

(defun fraction-value-p (value)
  "Matches fractions like 1/2, 1/3, 2/3, etc."
  (when value
    (let ((slash-pos (position #\/ value)))
      (when (and slash-pos
                 (plusp slash-pos)
                 (< slash-pos (1- (length value))))
        (let ((numerator (subseq value 0 slash-pos))
              (denominator (subseq value (1+ slash-pos))))
          (and (every #'digit-char-p numerator)
               (every #'digit-char-p denominator)))))))


(-> tshirt-size-p ((or null string))
    (values boolean &optional))

(defun tshirt-size-p (value)
  "Matches t-shirt sizes like xs, sm, md, lg, xl, 2xl, 3xl, etc."
  (when value
    (let ((val (string-downcase value)))
      (when (or (member val '("xs" "sm" "md" "lg" "xl") :test #'string=)
                ;; Match 2xl, 3xl, etc.
                (and (>= (length val) 3)
                     (string= (subseq val (- (length val) 2)) "xl")
                     (every #'digit-char-p (subseq val 0 (- (length val) 2)))))
        t))))


(-> percent-value-p ((or null string))
    (values boolean &optional))

(defun percent-value-p (value)
  "Matches percentage values like 50%, 100%, etc."
  (when (and value
             (plusp (length value))
             (char= (char value (1- (length value))) #\%))
    (number-value-p (subseq value 0 (1- (length value))))))


(-> empty-or-number-p ((or null string))
    (values boolean &optional))

(defun empty-or-number-p (value)
  "Matches empty string or number values (for classes like 'border' or 'border-2')."
  (or (null value)
      (string= value "")
      (number-value-p value)))