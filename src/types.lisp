(uiop:define-package #:tailwind-merge/types
  (:use #:cl)
  (:export #:parsed-class-name
           #:make-parsed-class-name
           #:parsed-class-name-modifiers
           #:parsed-class-name-has-important-modifier
           #:parsed-class-name-base-class-name
           #:parsed-class-name-maybe-postfix-modifier-position
           #:parsed-class-name-is-external
           #:class-validator-object
           #:class-group-id
           #:validator
           #:class-part-object
           #:next-part
           #:validators
           #:class-group-id
           #:theme-getter
           #:is-theme-getter-p
           #:theme-getter-function
           #:config
           #:cache-size
           #:config-prefix
           #:config-theme
           #:config-class-groups
           #:config-conflicting-class-groups
           #:config-conflicting-class-group-modifiers
           #:config-order-sensitive-modifiers
           #:config-utils
           #:cache-utils
           #:parse-class-name-fn
           #:sort-modifiers-fn
           #:get-class-group-id-fn
           #:get-conflicting-class-group-ids-fn))
(in-package #:tailwind-merge/types)

;;; Type definitions for tailwind-merge configuration system

(defstruct parsed-class-name
  "Structure representing a parsed Tailwind CSS class name."
  (modifiers nil :type list)
  (has-important-modifier nil :type boolean)
  (base-class-name "" :type string)
  (maybe-postfix-modifier-position nil :type (or null integer))
  (is-external nil :type boolean))

(defclass class-validator-object ()
  ((class-group-id :initarg :class-group-id
                    :reader class-group-id
                    :type string)
   (validator :initarg :validator
              :reader validator
              :type function)))

(defclass class-part-object ()
  ((next-part :initarg :next-part
              :accessor next-part
              :type hash-table
              :initform (make-hash-table :test 'equal))
   (validators :initarg :validators
               :accessor validators
               :type (or null list)
               :initform nil)
   (class-group-id :initarg :class-group-id
                   :accessor class-group-id
                   :type (or null string)
                   :initform nil))
  (:documentation "Trie node for class group matching."))

(defclass theme-getter ()
  ((is-theme-getter :initform t :reader is-theme-getter-p))
  (:documentation "Marker class for theme getter functions."))

(defgeneric theme-getter-function (getter theme)
  (:documentation "Call the theme getter with a theme object."))

(defmethod theme-getter-function ((getter function) theme)
  (funcall getter theme))

(defclass config ()
  ((cache-size :initarg :cache-size
               :reader cache-size
               :type integer
               :initform 500)
   (prefix :initarg :prefix
           :accessor config-prefix
           :type (or null string)
           :initform nil)
   (experimental-parse-class-name :initarg :experimental-parse-class-name
                                  :accessor experimental-parse-class-name
                                  :type (or null function)
                                  :initform nil)
   (theme :initarg :theme
          :accessor config-theme
          :type hash-table)
   (class-groups :initarg :class-groups
                 :accessor config-class-groups
                 :type hash-table)
   (conflicting-class-groups :initarg :conflicting-class-groups
                            :accessor config-conflicting-class-groups
                            :type hash-table)
   (conflicting-class-group-modifiers :initarg :conflicting-class-group-modifiers
                                     :accessor config-conflicting-class-group-modifiers
                                     :type hash-table)
   (order-sensitive-modifiers :initarg :order-sensitive-modifiers
                              :accessor config-order-sensitive-modifiers
                              :type list))
  (:documentation "Main configuration object for tailwind-merge."))

(defclass config-utils ()
  ((cache :initarg :cache
          :reader cache-utils)
   (parse-class-name :initarg :parse-class-name
                     :reader parse-class-name-fn
                     :type function)
   (sort-modifiers :initarg :sort-modifiers
                   :reader sort-modifiers-fn
                   :type function)
   (get-class-group-id :initarg :get-class-group-id
                       :reader get-class-group-id-fn
                       :type function)
   (get-conflicting-class-group-ids :initarg :get-conflicting-class-group-ids
                                    :reader get-conflicting-class-group-ids-fn
                                    :type function))
  (:documentation "Utilities created from a configuration."))

;;; Type aliases (for documentation)
(deftype class-validator () 'function)
(deftype class-group () 'list)
(deftype theme-object () 'hash-table)
(deftype class-group-ids () 'string)
(deftype theme-group-ids () 'string)

