(uiop:define-package #:tailwind-merge/validators
  (:use #:cl)
  (:export #:is-fraction
           #:is-number
           #:is-integer
           #:is-percent
           #:is-tshirt-size
           #:is-any
           #:is-any-non-arbitrary
           #:is-arbitrary-value
           #:is-arbitrary-variable
           #:is-arbitrary-size
           #:is-arbitrary-length
           #:is-arbitrary-number
           #:is-arbitrary-position
           #:is-arbitrary-image
           #:is-arbitrary-shadow
           #:is-arbitrary-variable-length
           #:is-arbitrary-variable-family-name
           #:is-arbitrary-variable-position
           #:is-arbitrary-variable-size
           #:is-arbitrary-variable-image
           #:is-arbitrary-variable-shadow))
(in-package #:tailwind-merge/validators)

;;; Validator functions for Tailwind CSS class matching
;;; Ported from validators.ts

;; We'll use simple string matching for now. For full regex support, cl-ppcre could be added.
;; For now, implement basic patterns manually.

(defun arbitrary-value-regex-match (value)
  "Match pattern: ^\[(?:(\w[\w-]*):)?(.+)\]$"
  (when (and (>= (length value) 3)
             (char= (char value 0) #\[)
             (char= (char value (1- (length value))) #\]))
    (let ((content (subseq value 1 (1- (length value)))))
      (let ((colon-pos (position #\: content)))
        (if colon-pos
            (list t (subseq content 0 colon-pos) (subseq content (1+ colon-pos)))
            (list t nil content))))))

(defun arbitrary-variable-regex-match (value)
  "Match pattern: ^\((?:(\w[\w-]*):)?(.+)\)$"
  (when (and (>= (length value) 3)
             (char= (char value 0) #\()
             (char= (char value (1- (length value))) #\)))
    (let ((content (subseq value 1 (1- (length value)))))
      (let ((colon-pos (position #\: content)))
        (if colon-pos
            (list t (subseq content 0 colon-pos) (subseq content (1+ colon-pos)))
            (list t nil content))))))

(defun fraction-regex-match (value)
  "Match pattern: ^\d+/\d+$"
  (let ((slash-pos (position #\/ value)))
    (when slash-pos
      (let ((before (subseq value 0 slash-pos))
            (after (subseq value (1+ slash-pos))))
        (when (and (> (length before) 0)
                   (> (length after) 0)
                   (every #'digit-char-p before)
                   (every #'digit-char-p after))
          t)))))

(defun tshirt-unit-regex-match (value)
  "Match pattern: ^(\d+(\.\d+)?)?(xs|sm|md|lg|xl)$"
  (let ((suffixes '("xs" "sm" "md" "lg" "xl")))
    (dolist (suffix suffixes nil)
      (when (and (>= (length value) (length suffix))
                 (string= value suffix :start1 (- (length value) (length suffix))))
        (let ((prefix-len (- (length value) (length suffix))))
          (when (or (= prefix-len 0)  ; Just the suffix, e.g., "xl"
                    (and (> prefix-len 0)
                         (let ((prefix (subseq value 0 prefix-len)))
                           (or (every #'digit-char-p prefix)
                               (let ((dot-pos (position #\. prefix)))
                                 (and dot-pos
                                      (> dot-pos 0)
                                      (< (1+ dot-pos) (length prefix))
                                      (every #'digit-char-p (subseq prefix 0 dot-pos))
                                      (every #'digit-char-p (subseq prefix (1+ dot-pos)))))))))
            (return t)))))))

(defun length-unit-regex-match (value)
  "Match pattern for length units"
  (or (string= value "0")
      (let ((units '("%" "px" "em" "rem" "pt" "pc" "in" "cm" "mm" "cap" "ch" "ex" "lh" "rlh"
                      "vw" "vh" "vi" "vb" "vmin" "vmax" "svw" "lvw" "dvw" "svh" "lvh" "dvh"
                      "cqw" "cqh" "cqi" "cqb" "cqmin" "cqmax")))
        (loop for unit in units
              when (and (>= (length value) (length unit))
                        (string= value unit :start1 (- (length value) (length unit)))
                        (let ((num-part (subseq value 0 (- (length value) (length unit)))))
                          (and (> (length num-part) 0)
                               (every #'digit-char-p num-part))))
                return t))
      (and (>= (length value) 5)
           (let ((prefix (subseq value 0 4)))
             (member prefix '("calc" "min(" "max(") :test #'string=))
           (char= (char value (1- (length value))) #\)))))

(defun color-function-regex-match (value)
  "Match pattern: ^(rgba?|hsla?|hwb|(ok)?(lab|lch)|color-mix)\(.+\)$"
  (and (>= (length value) 4)
       (char= (char value (1- (length value))) #\))
       (let ((open-paren (position #\( value)))
         (when open-paren
           (let ((func-name (subseq value 0 open-paren)))
             (member func-name '("rgb" "rgba" "hsl" "hsla" "hwb" "lab" "lch" "oklab" "oklch" "color-mix")
                     :test #'string=))))))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun shadow-regex-match (value)
  "Match pattern: ^(inset_)?-?((\d+)?\.?(\d+)[a-z]+|0)_-?((\d+)?\.?(\d+)[a-z]+|0)"
  (let ((parts (split-string value #\_)))
    (when (>= (length parts) 2)
      (let ((first-part (first parts))
            (second-part (second parts)))
        (and (or (string= first-part "inset")
                 (every (lambda (c) (or (digit-char-p c) (char= c #\-) (char= c #\.))) first-part))
              (every (lambda (c) (or (digit-char-p c) (char= c #\-) (char= c #\.))) second-part))))))

(defun image-regex-match (value)
  "Match pattern for image functions"
  (and (>= (length value) 4)
       (char= (char value (1- (length value))) #\))
       (let ((open-paren (position #\( value)))
         (when open-paren
           (let ((func-name (subseq value 0 open-paren)))
             (member func-name '("url" "image" "image-set" "cross-fade" "element"
                                  "linear-gradient" "radial-gradient" "conic-gradient"
                                  "repeating-linear-gradient" "repeating-radial-gradient"
                                  "repeating-conic-gradient")
                     :test #'string=))))))

;; Public validator functions

(defun is-fraction (value)
  (fraction-regex-match value))

(defun is-number (value)
  (and value
       (> (length value) 0)
       (let ((parsed (ignore-errors (read-from-string value))))
         (and (numberp parsed)
              (not (string= value ""))))))

(defun is-integer (value)
  (and value
       (> (length value) 0)
       (let ((parsed (ignore-errors (read-from-string value))))
         (and (integerp parsed)
              (= parsed (truncate parsed))))))

(defun is-percent (value)
  (and (>= (length value) 2)
       (char= (char value (1- (length value))) #\%)
       (is-number (subseq value 0 (1- (length value))))))

(defun is-tshirt-size (value)
  (tshirt-unit-regex-match value))

(defun is-any (value)
  (declare (ignore value))
  t)

(defun is-never (value)
  (declare (ignore value))
  nil)

(defun is-length-only (value)
  "Check if value is a length but not a color function"
  (and (length-unit-regex-match value)
       (not (color-function-regex-match value))))

(defun is-shadow (value)
  (shadow-regex-match value))

(defun is-image (value)
  (image-regex-match value))

(defun is-arbitrary-value (value)
  (arbitrary-value-regex-match value))

(defun is-arbitrary-variable (value)
  (arbitrary-variable-regex-match value))

(defun is-any-non-arbitrary (value)
  (and (not (is-arbitrary-value value))
       (not (is-arbitrary-variable value))))

;; Label checkers

(defun is-label-position (label)
  (or (string= label "position")
      (string= label "percentage")))

(defun is-label-image (label)
  (or (string= label "image")
      (string= label "url")))

(defun is-label-size (label)
  (or (string= label "length")
      (string= label "size")
      (string= label "bg-size")))

(defun is-label-length (label)
  (string= label "length"))

(defun is-label-number (label)
  (string= label "number"))

(defun is-label-family-name (label)
  (string= label "family-name"))

(defun is-label-shadow (label)
  (string= label "shadow"))

;; Helper functions

(defun get-is-arbitrary-value (value test-label test-value)
  (let ((match (arbitrary-value-regex-match value)))
    (when match
      (destructuring-bind (matched label val) match
        (declare (ignore matched))
        (if label
            (funcall test-label label)
            (funcall test-value val))))))

(defun get-is-arbitrary-variable (value test-label &optional (should-match-no-label nil))
  (let ((match (arbitrary-variable-regex-match value)))
    (when match
      (destructuring-bind (matched label val) match
        (declare (ignore val))
        (if label
            (funcall test-label label)
            should-match-no-label)))))

;; Exported validators

(defun is-arbitrary-size (value)
  (get-is-arbitrary-value value #'is-label-size #'is-never))

(defun is-arbitrary-length (value)
  (get-is-arbitrary-value value #'is-label-length #'is-length-only))

(defun is-arbitrary-number (value)
  (get-is-arbitrary-value value #'is-label-number #'is-number))

(defun is-arbitrary-position (value)
  (get-is-arbitrary-value value #'is-label-position #'is-never))

(defun is-arbitrary-image (value)
  (get-is-arbitrary-value value #'is-label-image #'is-image))

(defun is-arbitrary-shadow (value)
  (get-is-arbitrary-value value #'is-label-shadow #'is-shadow))

(defun is-arbitrary-variable-length (value)
  (get-is-arbitrary-variable value #'is-label-length))

(defun is-arbitrary-variable-family-name (value)
  (get-is-arbitrary-variable value #'is-label-family-name))

(defun is-arbitrary-variable-position (value)
  (get-is-arbitrary-variable value #'is-label-position))

(defun is-arbitrary-variable-size (value)
  (get-is-arbitrary-variable value #'is-label-size))

(defun is-arbitrary-variable-image (value)
  (get-is-arbitrary-variable value #'is-label-image))

(defun is-arbitrary-variable-shadow (value)
  (get-is-arbitrary-variable value #'is-label-shadow t))

