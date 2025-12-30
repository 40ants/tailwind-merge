(uiop:define-package #:tailwind-merge/modifiers
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:dict))
(in-package #:tailwind-merge/modifiers)


;;; ==================
;;; TailwindCSS Modifiers
;;; ==================
;;;
;;; This file defines the list of TailwindCSS modifiers (variants) and their ordering rules.
;;; Modifiers are prefixes that change when a class is applied, such as `hover:`, `focus:`, etc.
;;;
;;; Based on Tailwind CSS documentation for variants and modifiers.
;;;
;;; Structure:
;;; - *modifiers*: A list of all known TailwindCSS modifiers
;;; - parse-modifier: Function to parse modifier prefixes from class strings


(defparameter *modifiers*
  '(
    ;; Pseudo-class variants
    "hover" "focus" "active" "visited" "focus-within" "focus-visible"
    "first" "last" "odd" "even" "only"
    "first-of-type" "last-of-type" "only-of-type"
    "nth" "nth-last" "nth-of-type" "nth-last-of-type"
    "empty" "disabled" "enabled" "checked" "indeterminate" "default" "optional" "required"
    "valid" "invalid" "user-valid" "user-invalid" "in-range" "out-of-range" "placeholder-shown"
    "target" "details-content" "autofill" "read-only"
    
    ;; Pseudo-element variants
    "before" "after"
    "placeholder" "file" "marker" "selection"
    "first-line" "first-letter"
    "backdrop"
    
    ;; Responsive breakpoints
    "sm" "md" "lg" "xl" "2xl" "max-sm" "max-md" "max-lg" "max-xl" "max-2xl"
    
    ;; Container queries
    "@sm" "@md" "@lg" "@xl" "@2xl"
    
    ;; Dark mode
    "dark"
    
    ;; Motion and contrast variants
    "motion-safe" "motion-reduce"
    "contrast-more" "contrast-less"
    
    ;; Color scheme variants
    "forced-colors" "inverted-colors"
    
    ;; Pointer variants
    "pointer-fine" "pointer-coarse" "pointer-none"
    "any-pointer-fine" "any-pointer-coarse" "any-pointer-none"
    
    ;; Orientation variants
    "portrait" "landscape"
    
    ;; Other state variants
    "noscript" "print" "supports" "not-supports" "starting"
    
    ;; ARIA variants
    "aria-checked" "aria-disabled" "aria-expanded" "aria-hidden" "aria-pressed" 
    "aria-readonly" "aria-required" "aria-selected"
    
    ;; Data attribute variants
    "data"
    
    ;; Direction variants
    "rtl" "ltr"
    
    ;; State variants
    "open" "inert"
    
    ;; Child selector variants
    "*" "**"
    
    ;; Group and peer variants
    "group" "group-hover" "group-focus" "group-active" "group-visited" "group-focus-within"
    "group-focus-visible" "group-first" "group-last" "group-only" "group-even" "group-odd"
    "group-empty" "group-disabled" "group-enabled" "group-checked" "group-indeterminate"
    "group-default" "group-required" "group-valid" "group-invalid" "group-in-range"
    "group-out-of-range" "group-placeholder-shown" "group-autofill" "group-read-only"
    "group-target" "group-open" "group-closed"
    
    "peer" "peer-hover" "peer-focus" "peer-active" "peer-visited" "peer-focus-within"
    "peer-focus-visible" "peer-first" "peer-last" "peer-only" "peer-even" "peer-odd"
    "peer-empty" "peer-disabled" "peer-enabled" "peer-checked" "peer-indeterminate"
    "peer-default" "peer-required" "peer-valid" "peer-invalid" "peer-in-range"
    "peer-out-of-range" "peer-placeholder-shown" "peer-autofill" "peer-read-only"
    "peer-target" "peer-open" "peer-closed"
    
    ;; In variants (for styling based on any parent state)
    "in"))


(defun parse-modifiers (class-string)
  "Parses a class string to extract all modifier prefixes.

   Returns a list of modifier prefixes if found, otherwise returns NIL.

   Examples:
   (parse-modifiers \"hover:bg-red-500\") ; => '(\"hover\")
   (parse-modifiers \"hover:[@media(min-width:640px)]:p-4\") ; => '(\"hover\" \"[@media(min-width:640px)]\")
   (parse-modifiers \"bg-red-500\")       ; => NIL
   "
  (loop with modifiers = nil
        with bracket-depth = 0
        with paren-depth = 0
        with modifier-start = 0
        for index from 0 below (length class-string)
        for current-char = (char class-string index)
        do (cond
             ;; Handle bracket depth
             ((char= current-char #\[)
              (incf bracket-depth))
             ((char= current-char #\])
              (decf bracket-depth))
             ;; Handle parenthesis depth
             ((char= current-char #\()
              (incf paren-depth))
             ((char= current-char #\))
              (decf paren-depth))
             ;; Handle modifier separator only when outside brackets and parentheses
             ((and (char= current-char #\:)
                   (zerop bracket-depth)
                   (zerop paren-depth))
              (let ((modifier (subseq class-string modifier-start index)))
                (push modifier modifiers)
                (setf modifier-start (1+ index)))))
        finally (return (nreverse modifiers))))




(defparameter *order-sensitive-modifiers*
  (loop with result = (make-hash-table :test 'equal)
        for modifier in '("*" "**" "after" "backdrop" "before" "details-content" "file"
                          "first-letter" "first-line" "marker" "placeholder" "selection")
        do (setf (gethash modifier result) t)
        finally (return result))
  "Dictionary of modifiers whose order matters and should be preserved.")


(defun sort-modifiers (modifiers)
  "Sorts modifiers according to the same algorithm as the JavaScript library.

   - Predefined modifiers are sorted alphabetically
   - When an order-sensitive modifier appears, it must be preserved which modifiers are before and after it
   "
  ;; Process modifiers in one pass
  (loop with result = nil
        with current-segment = nil
        for modifier in modifiers
        do (let ((is-arbitrary (and (>= (length modifier) 1)
                                    (char= (char modifier 0) #\[)))
                 (is-order-sensitive (gethash modifier *order-sensitive-modifiers*)))
             (cond
               ((or is-arbitrary is-order-sensitive)
                ;; Sort and flush current segment alphabetically
                (when current-segment
                  (setf current-segment (sort current-segment #'string<))
                  (setf result (append result current-segment))
                  (setf current-segment nil))
                (push modifier result))
               (t
                ;; Regular modifier - add to current segment for batch sorting
                (push modifier current-segment))))
        finally (return
                  (nreverse
                   (cond
                     (current-segment
                      ;; Sort and add any remaining segment items
                      (append result
                              (sort current-segment #'string<)))
                     (t
                      result))))))



