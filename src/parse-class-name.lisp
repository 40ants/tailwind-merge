(uiop:define-package #:tailwind-merge/parse-class-name
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:parsed-class-name
                #:make-parsed-class-name
                #:config
                #:config-prefix
                #:experimental-parse-class-name)
  (:export #:create-parse-class-name
           #:+important-modifier+))
(in-package #:tailwind-merge/parse-class-name)

;;; Class name parsing
;;; Ported from parse-class-name.ts

(defparameter +important-modifier+ "!")
(defparameter +modifier-separator+ #\:)

(defun parse-class-name-internal (class-name)
  "Parse a single class name into its components."
  (let ((modifiers nil)
        (bracket-depth 0)
        (paren-depth 0)
        (modifier-start 0)
        (postfix-modifier-position nil)
        (len (length class-name)))
    ;; Parse modifiers and track bracket/paren depth
    (loop for index from 0 below len
          for current-char = (char class-name index)
          do (cond
               ;; At top level (not inside brackets or parens)
               ((and (zerop bracket-depth) (zerop paren-depth))
                (cond
                  ((char= current-char +modifier-separator+)
                   (push (subseq class-name modifier-start index) modifiers)
                   (setf modifier-start (1+ index)))
                  ((char= current-char #\/)
                   (setf postfix-modifier-position index))))
               ;; Track bracket depth
               ((char= current-char #\[)
                (incf bracket-depth))
               ((char= current-char #\])
                (decf bracket-depth))
               ;; Track paren depth
               ((char= current-char #\()
                (incf paren-depth))
               ((char= current-char #\))
                (decf paren-depth))))

    (let ((base-class-name-with-important
            (if (zerop (length modifiers))
                class-name
                (subseq class-name modifier-start))))

      ;; Check for important modifier
      (let ((base-class-name base-class-name-with-important)
            (has-important-modifier nil))
        (cond
          ;; Trailing !
          ((and (>= (length base-class-name-with-important) 1)
                (char= (char base-class-name-with-important
                              (1- (length base-class-name-with-important)))
                       (char +important-modifier+ 0)))
           (setf base-class-name
                 (subseq base-class-name-with-important 0
                         (1- (length base-class-name-with-important))))
           (setf has-important-modifier t))
          ;; Leading !
          ((and (>= (length base-class-name-with-important) 1)
                (char= (char base-class-name-with-important 0)
                       (char +important-modifier+ 0)))
           (setf base-class-name
                 (subseq base-class-name-with-important 1))
           (setf has-important-modifier t)))

        (let ((maybe-postfix-modifier-pos
                (when (and postfix-modifier-position
                           (> postfix-modifier-position modifier-start))
                  (- postfix-modifier-position modifier-start))))
          (make-parsed-class-name
           :modifiers (nreverse modifiers)
           :has-important-modifier has-important-modifier
           :base-class-name base-class-name
           :maybe-postfix-modifier-position maybe-postfix-modifier-pos
           :is-external nil))))))

(defun create-parse-class-name (config)
  "Create a class name parser function from a configuration."
  (let ((prefix (config-prefix config))
        (exp-parse-class-name (experimental-parse-class-name config)))
    (let ((parse-fn #'parse-class-name-internal))
      ;; Apply prefix handling if needed
      (when prefix
        (let ((full-prefix (concatenate 'string prefix ":")))
          (setf parse-fn
                (lambda (class-name)
                  (if (and (>= (length class-name) (length full-prefix))
                           (string= class-name full-prefix
                                    :end1 (length full-prefix)))
                      (parse-class-name-internal
                       (subseq class-name (length full-prefix)))
                      (make-parsed-class-name
                       :modifiers nil
                       :has-important-modifier nil
                       :base-class-name class-name
                       :maybe-postfix-modifier-position nil
                       :is-external t))))))

      ;; Apply experimental parse if needed
      (when exp-parse-class-name
        (let ((original-parse parse-fn))
          (setf parse-fn
                (lambda (class-name)
                  (funcall exp-parse-class-name
                           :class-name class-name
                           :parse-class-name original-parse)))))

      parse-fn)))
