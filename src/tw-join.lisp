(uiop:define-package #:tailwind-merge/tw-join
  (:use #:cl)
  (:export #:tw-join
           #:class-name-value))
(in-package #:tailwind-merge/tw-join)

;;; Class list joining utility
;;; Ported from tw-join.ts (based on clsx)

(deftype class-name-value ()
  '(or string null (member nil 0 false) list))

(defun tw-join (&rest class-lists)
  "Join class lists into a single string, filtering out falsy values.
   Similar to clsx library functionality."
  (let ((result ""))
    (dolist (class-list class-lists)
      (when class-list
        (let ((resolved (to-value class-list)))
          (when (and resolved (> (length resolved) 0))
            (when (> (length result) 0)
              (setf result (concatenate 'string result " ")))
            (setf result (concatenate 'string result resolved))))))
    result))

(defun to-value (mix)
  "Convert a class name value to a string."
  (typecase mix
    (string mix)
    (list
     (let ((result ""))
       (dolist (item mix)
         (when item
           (let ((resolved (to-value item)))
             (when (and resolved (> (length resolved) 0))
               (when (> (length result) 0)
                 (setf result (concatenate 'string result " ")))
               (setf result (concatenate 'string result resolved))))))
       result))
    (t "")))

