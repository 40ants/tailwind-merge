(uiop:define-package #:tailwind-merge/arbitrary
  (:use #:cl)
  (:export
   ;; Basic checks
   #:arbitrary-value-p
   #:arbitrary-variable-p
   ;; Typed arbitrary values
   #:arbitrary-length-p
   #:arbitrary-number-p
   #:arbitrary-position-p
   #:arbitrary-image-p
   #:arbitrary-shadow-p
   #:arbitrary-size-p
   ;; Typed arbitrary variables
   #:arbitrary-variable-length-p
   #:arbitrary-variable-position-p
   #:arbitrary-variable-image-p
   #:arbitrary-variable-shadow-p
   #:arbitrary-variable-size-p
   #:arbitrary-variable-family-name-p
   ;; Utility
   #:any-non-arbitrary-p
   ;; Parsing helpers
   #:parse-arbitrary-value
   #:parse-arbitrary-variable))
(in-package #:tailwind-merge/arbitrary)

;;; ============================================================================
;;; Arbitrary Values Parser
;;; ============================================================================
;;;
;;; Tailwind CSS supports two forms of arbitrary values:
;;; 1. Square brackets: [value] or [label:value]
;;; 2. Parentheses (CSS variables): (value) or (label:value)
;;;
;;; Labels allow explicit type specification:
;;; - [length:100px] - explicitly a length
;;; - [number:500] - explicitly a number
;;; - [position:center] - explicitly a position

;;; ============================================================================
;;; Parsing Functions
;;; ============================================================================

(defun parse-arbitrary-value (value)
  "Parse an arbitrary value in square brackets format: `[label:content]` or `[content]`.
   Returns (values matched-p label content) where label may be NIL."
  (when (and value
             (>= (length value) 3)
             (char= (char value 0) #\[)
             (char= (char value (1- (length value))) #\]))
    (let* ((content (subseq value 1 (1- (length value))))
           (colon-pos (position #\: content)))
      (if (and colon-pos
               (> colon-pos 0)
               ;; Make sure the part before colon looks like a valid label (alphanumeric + dash)
               (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                      (subseq content 0 colon-pos)))
          (values t
                  (subseq content 0 colon-pos)
                  (subseq content (1+ colon-pos)))
          (values t nil content)))))


(defun parse-arbitrary-variable (value)
  "Parse an arbitrary variable in parentheses format: `(label:content)` or `(content)`.
   Returns (values matched-p label content) where label may be NIL."
  (when (and value
             (>= (length value) 3)
             (char= (char value 0) #\()
             (char= (char value (1- (length value))) #\)))
    (let* ((content (subseq value 1 (1- (length value))))
           (colon-pos (position #\: content)))
      (if (and colon-pos
               (> colon-pos 0)
               ;; Make sure the part before colon looks like a valid label
               (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                      (subseq content 0 colon-pos)))
          (values t
                  (subseq content 0 colon-pos)
                  (subseq content (1+ colon-pos)))
          (values t nil content)))))


;;; ============================================================================
;;; Basic Checks
;;; ============================================================================

(defun arbitrary-value-p (value)
  "Check if VALUE is any arbitrary value (enclosed in square brackets)."
  (parse-arbitrary-value value))


(defun arbitrary-variable-p (value)
  "Check if VALUE is any arbitrary variable (enclosed in parentheses)."
  (parse-arbitrary-variable value))


(defun any-non-arbitrary-p (value)
  "Check if VALUE is NOT an arbitrary value or variable."
  (and (not (arbitrary-value-p value))
       (not (arbitrary-variable-p value))))


;;; ============================================================================
;;; Content Type Detection
;;; ============================================================================

(defparameter *length-units*
  '("%" "px" "em" "rem" "pt" "pc" "in" "cm" "mm"
    "cap" "ch" "ex" "lh" "rlh"
    "vw" "vh" "vi" "vb" "vmin" "vmax"
    "svw" "svh" "dvw" "dvh" "lvw" "lvh"
    "cqw" "cqh" "cqi" "cqb" "cqmin" "cqmax")
  "CSS length units.")


(defparameter *color-functions*
  '("rgb" "rgba" "hsl" "hsla" "hwb" "lab" "lch" "oklab" "oklch" "color-mix")
  "CSS color functions.")


(defparameter *image-functions*
  '("url" "image" "image-set" "cross-fade" "element"
    "linear-gradient" "radial-gradient" "conic-gradient"
    "repeating-linear-gradient" "repeating-radial-gradient" "repeating-conic-gradient")
  "CSS image functions.")


(defun string-ends-with-p (string suffix)
  "Check if STRING ends with SUFFIX."
  (and (>= (length string) (length suffix))
       (string= string suffix
                :start1 (- (length string) (length suffix)))))


(defun string-starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= string prefix
                :end1 (length prefix))))


(defun extract-function-name (value)
  "Extract function name from a CSS function call like `rgb(...)`.
   Returns NIL if not a function call."
  (let ((paren-pos (position #\( value)))
    (when (and paren-pos
               (> paren-pos 0)
               (char= (char value (1- (length value))) #\)))
      (subseq value 0 paren-pos))))


(defun number-string-p (value)
  "Check if VALUE is a valid number string (integer or decimal)."
  (and value
       (> (length value) 0)
       (let ((start 0))
         ;; Handle negative numbers
         (when (and (> (length value) 1)
                    (char= (char value 0) #\-))
           (setf start 1))
         (let* ((rest (subseq value start))
                (dot-pos (position #\. rest)))
           (if dot-pos
               ;; Decimal number
               (and (> dot-pos 0)
                    (< dot-pos (1- (length rest)))
                    (every #'digit-char-p (subseq rest 0 dot-pos))
                    (every #'digit-char-p (subseq rest (1+ dot-pos))))
               ;; Integer
               (every #'digit-char-p rest))))))


(defun content-is-length-p (content)
  "Check if CONTENT looks like a CSS length value.
   Matches values like `100px`, `2rem`, `50%`, `calc(...)`, etc.
   Excludes color functions that might contain percentages."
  (when (and content (> (length content) 0))
    ;; Check if it's a color function first (exclude these)
    (let ((func-name (extract-function-name content)))
      (when (and func-name
                 (member func-name *color-functions* :test #'string-equal))
        (return-from content-is-length-p nil)))
    ;; Check for "0" which is a valid length
    (when (string= content "0")
      (return-from content-is-length-p t))
    ;; Check for calc/min/max/clamp functions
    (let ((func-name (extract-function-name content)))
      (when (member func-name '("calc" "min" "max" "clamp") :test #'string-equal)
        (return-from content-is-length-p t)))
    ;; Check for number + unit
    (dolist (unit *length-units*)
      (when (string-ends-with-p content unit)
        (let ((num-part (subseq content 0 (- (length content) (length unit)))))
          (when (number-string-p num-part)
            (return-from content-is-length-p t)))))
    nil))


(defun content-is-number-p (content)
  "Check if CONTENT is a plain number (no units)."
  (number-string-p content))


(defun content-is-image-p (content)
  "Check if CONTENT looks like a CSS image value.
   Matches `url()`, gradient functions, etc."
  (when content
    (let ((func-name (extract-function-name content)))
      (when func-name
        (and (member func-name *image-functions* :test #'string-equal)
             t)))))


(defun content-is-shadow-p (content)
  "Check if CONTENT looks like a CSS shadow value.
   Shadow values typically start with x and y offsets separated by underscore,
   optionally prepended by 'inset_'.
   Pattern: `(inset_)?-?offset_-?offset...`"
  (when (and content (> (length content) 0))
    (let* ((parts (split-string content #\_))
           (start-idx 0))
      ;; Skip 'inset' if present
      (when (and (> (length parts) 0)
                 (string-equal (first parts) "inset"))
        (setf start-idx 1))
      ;; Need at least 2 parts after optional inset (x and y offsets)
      (when (>= (- (length parts) start-idx) 2)
        (let ((x-offset (nth start-idx parts))
              (y-offset (nth (1+ start-idx) parts)))
          ;; Both offsets should look like numbers (possibly with units)
          (and (looks-like-offset-p x-offset)
               (looks-like-offset-p y-offset)))))))


(defun looks-like-offset-p (value)
  "Check if VALUE looks like a shadow offset (number, possibly with unit, possibly negative)."
  (when (and value (> (length value) 0))
    (let ((val value))
      ;; Handle negative
      (when (char= (char val 0) #\-)
        (setf val (subseq val 1)))
      (when (= (length val) 0)
        (return-from looks-like-offset-p nil))
      ;; "0" is valid
      (when (string= val "0")
        (return-from looks-like-offset-p t))
      ;; Check for number with optional unit
      (or (number-string-p val)
          (content-is-length-p val)))))


(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))


;;; ============================================================================
;;; Label Checkers
;;; ============================================================================

(defun label-is-length-p (label)
  "Check if LABEL indicates a length type."
  (string-equal label "length"))


(defun label-is-number-p (label)
  "Check if LABEL indicates a number type."
  (string-equal label "number"))


(defun label-is-position-p (label)
  "Check if LABEL indicates a position type."
  (or (string-equal label "position")
      (string-equal label "percentage")))


(defun label-is-image-p (label)
  "Check if LABEL indicates an image type."
  (or (string-equal label "image")
      (string-equal label "url")))


(defun label-is-shadow-p (label)
  "Check if LABEL indicates a shadow type."
  (string-equal label "shadow"))


(defun label-is-size-p (label)
  "Check if LABEL indicates a size type."
  (or (string-equal label "length")
      (string-equal label "size")
      (string-equal label "bg-size")))


(defun label-is-family-name-p (label)
  "Check if LABEL indicates a font family name type."
  (string-equal label "family-name"))


;;; ============================================================================
;;; Typed Arbitrary Value Checkers
;;; ============================================================================

(defun arbitrary-length-p (value)
  "Check if VALUE is an arbitrary length value.
   Matches `[length:...]` or `[content]` where content looks like a length."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (when matched
      (if label
          (label-is-length-p label)
          (content-is-length-p content)))))


(defun arbitrary-number-p (value)
  "Check if VALUE is an arbitrary number value.
   Matches `[number:...]` or `[content]` where content is a number."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (when matched
      (if label
          (label-is-number-p label)
          (content-is-number-p content)))))


(defun arbitrary-position-p (value)
  "Check if VALUE is an arbitrary position value.
   Only matches `[position:...]` or `[percentage:...]` labels.
   Position cannot be auto-detected from content."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (declare (ignore content))
    (when matched
      (and label (label-is-position-p label)))))


(defun arbitrary-image-p (value)
  "Check if VALUE is an arbitrary image value.
   Matches `[image:...]`, `[url:...]` or `[content]` where content is an image function."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (when matched
      (if label
          (label-is-image-p label)
          (content-is-image-p content)))))


(defun arbitrary-shadow-p (value)
  "Check if VALUE is an arbitrary shadow value.
   Matches `[shadow:...]` or `[content]` where content looks like a shadow."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (when matched
      (if label
          (label-is-shadow-p label)
          (content-is-shadow-p content)))))


(defun arbitrary-size-p (value)
  "Check if VALUE is an arbitrary size value.
   Only matches `[length:...]`, `[size:...]`, or `[bg-size:...]` labels.
   Size cannot be auto-detected from content alone."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-value value)
    (declare (ignore content))
    (when matched
      (and label (label-is-size-p label)))))


;;; ============================================================================
;;; Typed Arbitrary Variable Checkers
;;; ============================================================================

(defun arbitrary-variable-length-p (value)
  "Check if VALUE is an arbitrary variable with length type.
   Matches `(length:...)`."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (and label (label-is-length-p label)))))


(defun arbitrary-variable-position-p (value)
  "Check if VALUE is an arbitrary variable with position type.
   Matches `(position:...)` or `(percentage:...)`."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (and label (label-is-position-p label)))))


(defun arbitrary-variable-image-p (value)
  "Check if VALUE is an arbitrary variable with image type.
   Matches `(image:...)` or `(url:...)`."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (and label (label-is-image-p label)))))


(defun arbitrary-variable-shadow-p (value &optional (match-no-label t))
  "Check if VALUE is an arbitrary variable with shadow type.
   Matches `(shadow:...)` or any variable if MATCH-NO-LABEL is true."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (if label
          (label-is-shadow-p label)
          match-no-label))))


(defun arbitrary-variable-size-p (value)
  "Check if VALUE is an arbitrary variable with size type.
   Matches `(length:...)`, `(size:...)`, or `(bg-size:...)`."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (and label (label-is-size-p label)))))


(defun arbitrary-variable-family-name-p (value)
  "Check if VALUE is an arbitrary variable with family-name type.
   Matches `(family-name:...)`."
  (multiple-value-bind (matched label content)
      (parse-arbitrary-variable value)
    (declare (ignore content))
    (when matched
      (and label (label-is-family-name-p label)))))

