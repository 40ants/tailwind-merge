(uiop:define-package #:tailwind-merge/extend-tailwind-merge
  (:use #:cl)
  (:import-from #:tailwind-merge/create-tailwind-merge
                #:create-tailwind-merge)
  (:import-from #:tailwind-merge/default-config
                #:get-default-config)
  (:import-from #:tailwind-merge/merge-configs
                #:merge-configs)
  (:export #:extend-tailwind-merge))
(in-package #:tailwind-merge/extend-tailwind-merge)

;;; Configuration extension system
;;; Ported from extend-tailwind-merge.ts

(defun extend-tailwind-merge (config-extension &rest create-config)
  "Extend the default tailwind-merge configuration.

   config-extension can be either:
   - A function that takes a config and returns a modified config
   - A configuration extension object (plist with :extend, :override, etc.)"
  (if (functionp config-extension)
      (apply #'create-tailwind-merge #'get-default-config config-extension create-config)
      (create-tailwind-merge
       (lambda () (merge-configs (get-default-config) config-extension))
       create-config)))

