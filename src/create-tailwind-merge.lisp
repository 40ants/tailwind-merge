(uiop:define-package #:tailwind-merge/create-tailwind-merge
  (:use #:cl)
  (:import-from #:tailwind-merge/config-utils
                #:create-config-utils
                #:cache-get
                #:cache-set)
  (:import-from #:tailwind-merge/tw-join
                #:tw-join)
  (:import-from #:tailwind-merge/merge-classlist
                #:merge-class-list)
  (:export #:create-tailwind-merge))
(in-package #:tailwind-merge/create-tailwind-merge)

;;; Main factory function for creating tailwind-merge
;;; Ported from create-tailwind-merge.ts

(defun create-tailwind-merge (create-config-first &rest create-config-rest)
  "Create a tailwind-merge function with lazy initialization.

   create-config-first: Function that returns the base configuration
   create-config-rest: Additional functions that modify the configuration"
  (let ((config-utils nil)
        (cache-get-fn nil)
        (cache-set-fn nil)
        (function-to-call nil))

    (labels ((init-tailwind-merge (class-list)
               (let ((config (reduce (lambda (previous-config create-config-current)
                                        (funcall create-config-current previous-config))
                                      create-config-rest
                                      :initial-value (funcall create-config-first))))
                 (setf config-utils (create-config-utils config))
                 (setf cache-get-fn (lambda (key) (cache-get config-utils key)))
                 (setf cache-set-fn (lambda (key value) (cache-set config-utils key value)))
                 (setf function-to-call #'tailwind-merge-fn)
                 (tailwind-merge-fn class-list)))

             (tailwind-merge-fn (class-list)
               (let ((cached-result (funcall cache-get-fn class-list)))
                 (if cached-result
                     cached-result
                     (let ((result (merge-class-list class-list config-utils)))
                       (funcall cache-set-fn class-list result)
                       result)))))

      (setf function-to-call #'init-tailwind-merge)

      (lambda (&rest class-lists)
        (funcall function-to-call (apply #'tw-join class-lists))))))

