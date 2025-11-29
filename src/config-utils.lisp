(uiop:define-package #:tailwind-merge/config-utils
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:config
                #:cache-size
                #:config-utils
                #:cache-utils)
  (:import-from #:tailwind-merge/lru-cache
                #:make-lru-cache
                #:lru-cache)
  (:import-from #:tailwind-merge/parse-class-name
                #:create-parse-class-name)
  (:import-from #:tailwind-merge/sort-modifiers
                #:create-sort-modifiers)
  (:import-from #:tailwind-merge/class-group-utils
                #:create-class-group-utils)
  (:export #:create-config-utils
           #:cache-get
           #:cache-set))
(in-package #:tailwind-merge/config-utils)

;;; Configuration utilities factory
;;; Ported from config-utils.ts

(defun create-config-utils (config)
  "Create configuration utilities from a config object."
  (let ((class-group-utils (create-class-group-utils config)))
    (make-instance 'config-utils
                   :cache (make-lru-cache (cache-size config))
                   :parse-class-name (create-parse-class-name config)
                   :sort-modifiers (create-sort-modifiers config)
                   :get-class-group-id (getf class-group-utils :get-class-group-id)
                   :get-conflicting-class-group-ids (getf class-group-utils :get-conflicting-class-group-ids))))

(defmethod cache-get ((utils config-utils) key)
  "Get a value from the cache in config-utils."
  (tailwind-merge/lru-cache:cache-get (cache-utils utils) key))

(defmethod cache-set ((utils config-utils) key value)
  "Set a value in the cache in config-utils."
  (tailwind-merge/lru-cache:cache-set (cache-utils utils) key value))

