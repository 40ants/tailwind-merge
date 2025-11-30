(uiop:define-package #:tailwind-merge/tailwind-classes
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:dict)
  (:import-from #:alexandria
                #:curry))
(in-package #:tailwind-merge/tailwind-classes)


(defparameter *classes*
  '((:px)
    (:py)
    (:p)
    (:text-color (:text :red :green))
    (:text-size (:text :xl :2xl))
    (:text-align :text-left :text-center)))


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
                         (setf (gethash (string-downcase rule)
                                        result)
                               class-name))
                      (cons
                         (let ((prefix (string-downcase (first rule)))
                               (validators (make-validators-from-rule (rest rule))))
                           (push (list* class-name
                                        validators)
                                 (gethash prefix
                                          result))))))
        finally (return result)))



(defun parse-class (string)
  "Parses CSS class and returns an object."
  ;; Finds prefixes from string end to the start while searching a parser for the prefix.

  (loop with end-pos = (length string)
        with map = (build-classes-map)
        for separator-pos = (position #\- string
                                      :from-end t
                                      :end end-pos
                                      :test #'char=)
        when separator-pos
          do (let* ((prefix (subseq string 0 separator-pos))
                    (value (subseq string (1+ separator-pos)))
                    (class-name-or-validators
                      (gethash prefix map)))
               (when class-name-or-validators
                 (etypecase class-name-or-validators
                   (keyword (return-from parse-class
                              class-name-or-validators))
                   (list (loop for (class-name . validators) in class-name-or-validators
                               do (loop for validator in validators
                                        always (funcall validator value)
                                        finally (return-from parse-class
                                                  class-name))))))
               (setf end-pos
                     (1- separator-pos)))
        while (and separator-pos
                   (< 0 separator-pos))))



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
