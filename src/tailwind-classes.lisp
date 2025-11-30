(uiop:define-package #:tailwind-merge/tailwind-classes
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:dict)
  (:import-from #:alexandria
                #:curry))
(in-package #:tailwind-merge/tailwind-classes)


(-> integer-value-p ((or null string))
    (values boolean &optional))

(defun integer-value-p (value)
  (when (and value
             (ignore-errors (parse-integer value)))
    (values t)))


(defparameter *classes*
  '(;; Padding classes are not conflicting to each other.
    ;; We can have p-8 px-2 and it is ok.
    ;; But when user specified p-8 px-2 p-4 px-1, then
    ;; after the merge it should be p-4 px-1
    (:px)
    (:py)
    (:pl)
    (:pt)
    (:pr)
    (:pb)
    (:p)
    (:border-width (:border null)) ;; This is for processing "border" class
    (:border-width (:border integer-value-p)) ;; When we have border-2, border-3, etc, this is boder-width too
    (:border-color (:border
                    ;; Tailwind color predicate function should be created instead:
                    :red
                    :green))
    (:text-color (:text :red :green))
    (:text-size (:text
                 ;; Tailwind sizes predicate function should be created instead:
                 :xl :2xl))
    ;; These are conflicting classes.
    ;; First list item is a class group name, rest of the list - conflicting class names.
    (:text-align :text-left :text-center)
    (:items-align :items-start :items-center :items-end)
    (:justify-items :justify-items-start :justify-items-center :justify-items-end)))


(-> make-validators-from-rule (list)
    (values list &optional))

(defun make-validators-from-rule (rule)
  (cond
    ((every #'keywordp rule)
     (list (lambda (value)
             (member value rule
                     :test #'string-equal))))
    (t
     (loop for item in rule
           collect (etypecase item
                     (keyword (curry #'string-equal item))
                     (function item)
                     (symbol
                        (unless (fboundp item)
                          (error "Symbol ~S should be bound to a function"
                                 item))
                        item))))))


(defun build-classes-map ()
  (loop with result = (serapeum:dict)
        for (class-name . rest) in *classes*
        for rules = (if rest rest (list class-name))
        do (loop for rule in rules
                 do (etypecase rule
                      (keyword
                         (push class-name
                               (gethash (string-downcase rule)
                                        result)))
                      (cons
                         (let ((prefix (string-downcase (first rule)))
                               (validators (make-validators-from-rule (rest rule))))
                           (push (list* class-name
                                        validators)
                                 (gethash prefix
                                          result))))))
        finally (return result)))



(defun parse-class (string &aux (map (build-classes-map)))
  "Parses CSS class and returns an object."
  ;; Finds prefixes from string end to the start while searching a parser for the prefix.

  (flet ((search-for-class (prefix &optional value)
           (let ((class-name-or-validators
                   (gethash prefix map)))
             (when class-name-or-validators
               (loop for (class-name . validators) in class-name-or-validators
                     do (loop for validator in validators
                              always (funcall validator value)
                              finally (return-from parse-class
                                        class-name)))))))
    (loop with end-pos = (length string)
          with separator-was-found = nil
          for separator-pos = (position #\- string
                                        :from-end t
                                        :end end-pos
                                        :test #'char=)
          when separator-pos
            do (setf separator-was-found
                     t)
               (let* ((prefix (subseq string 0 separator-pos))
                      (value (subseq string (1+ separator-pos))))
                 (search-for-class prefix value)
                 (setf end-pos
                       (1- separator-pos)))
          while (and separator-pos
                     (< 0 separator-pos))
          finally (unless separator-was-found
                    (return (search-for-class string))))))



(-> merge-tailwind-classes ((soft-list-of string))
    (values (soft-list-of string) &optional))

(defun merge-tailwind-classes (classes)
  (loop with seen-classes = (dict)
        for class in (reverse classes)
        for parsed = (parse-class class)
        unless (gethash parsed seen-classes)
          collect class
          and do (setf (gethash parsed seen-classes)
                       t)))
