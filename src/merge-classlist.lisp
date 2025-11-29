(uiop:define-package #:tailwind-merge/merge-classlist
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:config-utils
                #:parse-class-name-fn
                #:sort-modifiers-fn
                #:get-class-group-id-fn
                #:get-conflicting-class-group-ids-fn
                #:parsed-class-name-modifiers
                #:parsed-class-name-has-important-modifier
                #:parsed-class-name-base-class-name
                #:parsed-class-name-maybe-postfix-modifier-position
                #:parsed-class-name-is-external)
  (:import-from #:tailwind-merge/parse-class-name
                #:+important-modifier+)
  (:export #:merge-class-list))
(in-package #:tailwind-merge/merge-classlist)

;;; Merge class list algorithm
;;; Ported from merge-classlist.ts

(defun merge-class-list (class-list config-utils)
  "Merge a class list, resolving conflicts."
  (let ((parse-class-name (parse-class-name-fn config-utils))
        (get-class-group-id (get-class-group-id-fn config-utils))
        (get-conflicting-class-group-ids (get-conflicting-class-group-ids-fn config-utils))
        (sort-modifiers (sort-modifiers-fn config-utils))
        (class-groups-in-conflict nil)
        (class-names (split-class-list class-list))
        (result ""))

    ;; Process classes in reverse order
    (loop for index from (1- (length class-names)) downto 0
          for original-class-name = (nth index class-names)
          for parsed = (funcall parse-class-name original-class-name)
          do (block process-class
               ;; Handle external classes
               (when (parsed-class-name-is-external parsed)
                 (setf result (if (> (length result) 0)
                                  (concatenate 'string original-class-name " " result)
                                  original-class-name))
                 (return-from process-class))

               (let* ((maybe-postfix-pos (parsed-class-name-maybe-postfix-modifier-position parsed))
                      (has-postfix-modifier (not (null maybe-postfix-pos)))
                      (base-class-name (parsed-class-name-base-class-name parsed))
                      (class-group-id
                        (funcall get-class-group-id
                                 (if has-postfix-modifier
                                     (subseq base-class-name 0 maybe-postfix-pos)
                                     base-class-name))))

                 ;; Try to find class group
                 (when (null class-group-id)
                   (if has-postfix-modifier
                       ;; Try without postfix
                       (setf class-group-id (funcall get-class-group-id base-class-name))
                       ;; Not a Tailwind class, add it directly
                       (progn
                         (setf result (if (> (length result) 0)
                                          (concatenate 'string original-class-name " " result)
                                          original-class-name))
                         (return-from process-class))))

                 ;; Still no class group? Not a Tailwind class
                 (when (null class-group-id)
                   (setf result (if (> (length result) 0)
                                    (concatenate 'string original-class-name " " result)
                                    original-class-name))
                   (return-from process-class))

                 ;; Build modifier ID
                 (let* ((modifiers (parsed-class-name-modifiers parsed))
                        (variant-modifier
                          (cond
                            ((null modifiers) "")
                            ((= (length modifiers) 1) (first modifiers))
                            (t (join-strings (funcall sort-modifiers modifiers) ":"))))
                        (modifier-id
                          (if (parsed-class-name-has-important-modifier parsed)
                              (concatenate 'string variant-modifier +important-modifier+)
                              variant-modifier))
                        (class-id (concatenate 'string modifier-id class-group-id)))

                   ;; Check if already in conflict
                   (when (member class-id class-groups-in-conflict :test #'string=)
                     (return-from process-class))

                   ;; Add to conflict groups
                   (push class-id class-groups-in-conflict)

                   ;; Add all conflicting groups
                   (let ((conflict-groups (funcall get-conflicting-class-group-ids
                                                   class-group-id
                                                   has-postfix-modifier)))
                     (dolist (group conflict-groups)
                       (push (concatenate 'string modifier-id group) class-groups-in-conflict)))

                   ;; Add class to result
                   (setf result (if (> (length result) 0)
                                    (concatenate 'string original-class-name " " result)
                                    original-class-name))))))

    result))

(defun split-class-list (class-list)
  "Split a class list string into individual class names."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) class-list)))
    (if (string= trimmed "")
        nil
        (split-string trimmed))))

(defun split-string (string)
  "Split string by whitespace."
  (let ((result nil)
        (start 0)
        (len (length string)))
    (loop for i from 0 to len
          do (let ((at-whitespace (or (= i len)
                                      (member (char string i) '(#\Space #\Tab #\Newline)))))
               (when (and at-whitespace (< start i))
                 (push (subseq string start i) result))
               (when at-whitespace
                 (setf start (1+ i)))))
    (nreverse result)))

(defun join-strings (strings delimiter)
  "Join strings with delimiter."
  (if (null strings)
      ""
      (reduce (lambda (a b) (concatenate 'string a (string delimiter) b)) strings)))
