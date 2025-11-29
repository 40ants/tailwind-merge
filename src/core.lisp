(uiop:define-package #:tailwind-merge/core
  (:use #:cl)
  (:nicknames #:tailwind-merge)
  (:import-from #:tailwind-merge/tw-join
                #:tw-join)
  (:import-from #:tailwind-merge/create-tailwind-merge
                #:create-tailwind-merge)
  (:import-from #:tailwind-merge/extend-tailwind-merge
                #:extend-tailwind-merge)
  (:import-from #:tailwind-merge/default-config
                #:get-default-config)
  (:import-from #:tailwind-merge/merge-configs
                #:merge-configs)
  (:import-from #:tailwind-merge/from-theme
                #:from-theme)
  (:import-from #:tailwind-merge/types
                #:parsed-class-name
                #:make-parsed-class-name
                #:parsed-class-name-modifiers
                #:parsed-class-name-has-important-modifier
                #:parsed-class-name-base-class-name
                #:parsed-class-name-maybe-postfix-modifier-position
                #:parsed-class-name-is-external
                #:config
                #:config-prefix
                #:config-theme
                #:config-class-groups
                #:config-conflicting-class-groups
                #:config-conflicting-class-group-modifiers
                #:config-order-sensitive-modifiers
                #:class-part-object
                #:config-utils)
  (:import-from #:tailwind-merge/validators
                #:is-fraction
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
                #:is-arbitrary-variable-shadow)
  (:export
   ;; Main API
   #:tw-merge
   #:tw-join
   #:create-tailwind-merge
   #:extend-tailwind-merge
   #:get-default-config
   #:merge-configs
   #:from-theme

   ;; Types
   #:parsed-class-name
   #:make-parsed-class-name
   #:parsed-class-name-modifiers
   #:parsed-class-name-has-important-modifier
   #:parsed-class-name-base-class-name
   #:parsed-class-name-maybe-postfix-modifier-position
   #:parsed-class-name-is-external
   #:config
   #:config-prefix
   #:config-theme
   #:config-class-groups
   #:config-conflicting-class-groups
   #:config-conflicting-class-group-modifiers
   #:config-order-sensitive-modifiers
   #:class-part-object
   #:config-utils

   ;; Validators
   #:is-fraction
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

(in-package #:tailwind-merge/core)

;;; Main API exports

;; Default tw-merge instance
(defvar *tw-merge* nil)

(defun tw-merge (&rest class-lists)
  "Merge Tailwind CSS classes, resolving conflicts.
   This is the main entry point for the library."
  (unless *tw-merge*
    (setf *tw-merge* (create-tailwind-merge #'get-default-config)))
  (apply *tw-merge* class-lists))
