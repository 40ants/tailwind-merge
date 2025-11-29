(uiop:define-package #:tailwind-merge/merge-configs
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:config
                #:config-prefix
                #:config-theme
                #:config-class-groups
                #:config-conflicting-class-groups
                #:config-conflicting-class-group-modifiers
                #:config-order-sensitive-modifiers)
  (:export #:merge-configs))
(in-package #:tailwind-merge/merge-configs)

;;; Configuration merging utility
;;; Ported from merge-configs.ts

(defun merge-configs (base-config config-extension)
  "Merge a configuration extension into a base configuration.
   The base-config will be mutated."
  (let ((cache-size (getf config-extension :cache-size))
        (prefix (getf config-extension :prefix))
        (experimental-parse-class-name (getf config-extension :experimental-parse-class-name))
        (extend (getf config-extension :extend))
        (override (getf config-extension :override)))

    (when cache-size
      (setf (slot-value base-config 'cache-size) cache-size))
    (when prefix
      (setf (config-prefix base-config) prefix))
    (when experimental-parse-class-name
      (setf (experimental-parse-class-name base-config) experimental-parse-class-name))

    (when override
      (override-config-properties (config-theme base-config) (getf override :theme))
      (override-config-properties (config-class-groups base-config) (getf override :class-groups))
      (override-config-properties (config-conflicting-class-groups base-config)
                                   (getf override :conflicting-class-groups))
      (override-config-properties (config-conflicting-class-group-modifiers base-config)
                                   (getf override :conflicting-class-group-modifiers))
      (when (getf override :order-sensitive-modifiers)
        (setf (config-order-sensitive-modifiers base-config)
              (getf override :order-sensitive-modifiers))))

    (when extend
      (merge-config-properties (config-theme base-config) (getf extend :theme))
      (merge-config-properties (config-class-groups base-config) (getf extend :class-groups))
      (merge-config-properties (config-conflicting-class-groups base-config)
                               (getf extend :conflicting-class-groups))
      (merge-config-properties (config-conflicting-class-group-modifiers base-config)
                               (getf extend :conflicting-class-group-modifiers))
      (merge-array-properties base-config extend :order-sensitive-modifiers))

    base-config))

(defun override-config-properties (base-object override-object)
  "Override properties in base object with values from override object."
  (when override-object
    (maphash (lambda (key value)
               (setf (gethash key base-object) value))
             override-object)))

(defun merge-config-properties (base-object merge-object)
  "Merge properties from merge object into base object."
  (when merge-object
    (maphash (lambda (key value)
               (merge-array-properties base-object (list key value) key))
             merge-object)))

(defun merge-array-properties (base-object merge-object key)
  "Merge array properties."
  (let ((merge-value (if (hash-table-p merge-object)
                         (gethash key merge-object)
                         (getf merge-object key))))
    (when merge-value
      (let ((base-value (if (typep base-object 'config)
                            (case key
                              (:order-sensitive-modifiers (config-order-sensitive-modifiers base-object))
                              (t nil))
                            (gethash key base-object))))
        (if base-value
            (if (typep base-object 'config)
                (setf (slot-value base-object key) (append base-value merge-value))
                (setf (gethash key base-object) (append base-value merge-value)))
            (if (typep base-object 'config)
                (setf (slot-value base-object key) merge-value)
                (setf (gethash key base-object) merge-value)))))))

