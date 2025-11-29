(uiop:define-package #:tailwind-merge/sort-modifiers
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:config
                #:config-order-sensitive-modifiers)
  (:export #:create-sort-modifiers))
(in-package #:tailwind-merge/sort-modifiers)

;;; Modifier sorting
;;; Ported from sort-modifiers.ts

(defun create-sort-modifiers (config)
  "Create a modifier sorting function from a configuration."
  (let ((order-sensitive-modifiers (config-order-sensitive-modifiers config))
        (modifier-weights (make-hash-table :test 'equal)))

    ;; Pre-compute weights for all known modifiers
    (loop for index from 0 below (length order-sensitive-modifiers)
          for mod = (nth index order-sensitive-modifiers)
          do (setf (gethash mod modifier-weights) (+ 1000000 index)))

    (lambda (modifiers)
      (let ((result nil)
            (current-segment nil))
        (dolist (modifier modifiers)
          (let ((is-arbitrary (and (> (length modifier) 0)
                                   (char= (char modifier 0) #\[)))
                (is-order-sensitive (gethash modifier modifier-weights)))
            (cond
              ((or is-arbitrary is-order-sensitive)
               ;; Sort and flush current segment alphabetically
               (when current-segment
                 (setf current-segment (sort current-segment #'string<))
                 (setf result (nconc result current-segment))
                 (setf current-segment nil))
               (push modifier result))
              (t
               ;; Regular modifier - add to current segment for batch sorting
               (push modifier current-segment)))))

        ;; Sort and add any remaining segment items
        (when current-segment
          (setf current-segment (sort current-segment #'string<))
          (setf result (nconc result current-segment)))

        (nreverse result)))))

