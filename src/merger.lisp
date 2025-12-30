(uiop:define-package #:tailwind-merge/merger
  (:nicknames #:tailwind-merge)
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:dict)
  (:import-from #:tailwind-merge/validators
                #:integer-value-p
                #:number-value-p
                #:fraction-value-p
                #:tshirt-size-p
                #:percent-value-p
                #:empty-or-number-p
                #:make-validators-from-rule)
  (:import-from #:tailwind-merge/tailwind-classes
                #:*classes*
                #:build-classes-map
                #:parse-class)
  (:import-from #:tailwind-merge/modifiers
                #:sort-modifiers
                #:parse-modifiers)
  (:export #:merge-tailwind-classes))
(in-package #:tailwind-merge/merger)


(-> merge-tailwind-classes ((soft-list-of string))
    (values (soft-list-of string) &optional))

(defun merge-tailwind-classes (classes)
  "Merges Tailwind CSS classes while resolving conflicts between them.

   This function takes a list of CSS class strings and returns a new list with
   conflicting classes resolved. When multiple classes from the same group are
   present, only the last one (in order) is kept, effectively overriding the
   previous ones.

   For example, if both 'px-2' and 'px-3' are in the input, only 'px-3' will
   appear in the output since both belong to the same padding-x group.

   Non-conflicting classes are preserved in the output.

   Args:

   - CLASSES: A list of strings representing Tailwind CSS classes.

   Returns:

   A list of strings with conflicting classes resolved, keeping only the last
   class in case of conflicts.

   Examples:

   ```lisp
   (merge-tailwind-classes '(\"px-2\" \"px-3\"))
   ;; => (\"px-3\")

   (merge-tailwind-classes '(\"py-2\" \"px-3\"))
   ;; => (\"py-2\" \"px-3\")

   (merge-tailwind-classes '(\"bg-red-500\" \"bg-blue-500\"))
   ;; => (\"bg-blue-500\")

   (merge-tailwind-classes '(\"p-2\" \"hover:p-4\"))
   ;; => (\"p-2\" \"hover:p-4\")

   (merge-tailwind-classes '(\"hover:p-2\" \"hover:p-4\"))
   ;; => (\"hover:p-4\")

   (merge-tailwind-classes '(\"hover:focus:p-2\" \"focus:hover:p-4\"))
   ;; => (\"focus:hover:p-4\")
   ```
   "
  (loop with seen-classes = (dict)
        for class in (reverse classes)
        for (modifiers base-class) = (parse-modifiers class)
        for modifier = (when modifiers
                         (format nil "~{~a~^:~}" (sort-modifiers modifiers)))
        for parsed = (parse-class base-class)
        if (null parsed)
          collect class into results
        else unless (gethash (cons modifier parsed) seen-classes)
               collect class into results
               and do (setf (gethash (cons modifier parsed) seen-classes)
                            t)
        finally (return (nreverse results))))
