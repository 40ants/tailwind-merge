(uiop:define-package #:tailwind-merge/class-group-utils
  (:use #:cl)
  (:import-from #:tailwind-merge/types
                #:class-part-object
                #:class-validator-object
                #:config
                #:config-theme
                #:config-class-groups
                #:config-conflicting-class-groups
                #:config-conflicting-class-group-modifiers
                #:config-utils
                #:create-class-part-object
                #:next-part
                #:validators
                #:class-group-id)
  (:import-from #:tailwind-merge/utils
                #:concat-arrays)
  (:import-from #:tailwind-merge/from-theme
                #:theme-getter-p
                #:call-theme-getter)
  (:export #:create-class-group-utils
           #:create-class-map))
(in-package #:tailwind-merge/class-group-utils)

;;; Class group matching utilities
;;; Ported from class-group-utils.ts

(defparameter +class-part-separator+ "-")
(defparameter +arbitrary-property-prefix+ "arbitrary..")

(defun create-class-validator-object (class-group-id validator)
  "Create a class validator object."
  (make-instance 'class-validator-object
                 :class-group-id class-group-id
                 :validator validator))

(defun create-class-part-object (&key (next-part (make-hash-table :test 'equal))
                                      (validators nil)
                                      (class-group-id nil))
  "Create a class part object (trie node)."
  (make-instance 'class-part-object
                 :next-part next-part
                 :validators validators
                 :class-group-id class-group-id))

(defun create-class-group-utils (config)
  "Create class group utilities from a configuration."
  (let ((class-map (create-class-map config))
        (conflicting-class-groups (config-conflicting-class-groups config))
        (conflicting-class-group-modifiers (config-conflicting-class-group-modifiers config)))

    (flet ((get-class-group-id (class-name)
             (if (and (>= (length class-name) 2)
                      (char= (char class-name 0) #\[)
                      (char= (char class-name (1- (length class-name))) #\]))
                 (get-group-id-for-arbitrary-property class-name)
                 (let ((class-parts (split-string class-name +class-part-separator+)))
                   ;; Classes like `-inset-1` produce an empty string as first classPart
                   (let ((start-index (if (and (> (length class-parts) 0)
                                                (string= (first class-parts) "")
                                                (> (length class-parts) 1))
                                          1
                                          0)))
                     (get-group-recursive class-parts start-index class-map)))))

           (get-conflicting-class-group-ids (class-group-id has-postfix-modifier)
             (if has-postfix-modifier
                 (let ((modifier-conflicts (gethash class-group-id conflicting-class-group-modifiers))
                       (base-conflicts (gethash class-group-id conflicting-class-groups)))
                   (if modifier-conflicts
                       (if base-conflicts
                           (concat-arrays base-conflicts modifier-conflicts)
                           modifier-conflicts)
                       (or base-conflicts nil)))
                 (or (gethash class-group-id conflicting-class-groups) nil))))

      (list :get-class-group-id #'get-class-group-id
            :get-conflicting-class-group-ids #'get-conflicting-class-group-ids))))

(defun get-group-recursive (class-parts start-index class-part-object)
  "Recursively match class parts to find class group ID."
  (let ((class-parts-length (- (length class-parts) start-index)))
    (if (zerop class-parts-length)
        (class-group-id class-part-object)
        (let* ((current-class-part (nth start-index class-parts))
               (next-class-part-object (gethash current-class-part (next-part class-part-object))))
          (if next-class-part-object
              (let ((result (get-group-recursive class-parts (1+ start-index) next-class-part-object)))
                (when result
                  (return-from get-group-recursive result))))

          (let ((validators (validators class-part-object)))
            (if (null validators)
                (return-from get-group-recursive nil)
                (let ((class-rest (if (zerop start-index)
                                      (join-strings class-parts +class-part-separator+)
                                      (join-strings (subseq class-parts start-index) +class-part-separator+))))
                  (loop for validator-obj in validators
                        when (funcall (validator validator-obj) class-rest)
                          do (return-from get-group-recursive (class-group-id validator-obj))))))))))

(defun get-group-id-for-arbitrary-property (class-name)
  "Get the class group ID for an arbitrary property like [property:value]."
  (let ((content (subseq class-name 1 (1- (length class-name)))))
    (if (not (find #\: content))
        nil
        (let ((colon-index (position #\: content)))
          (if colon-index
              (let ((property (subseq content 0 colon-index)))
                (if (> (length property) 0)
                    (concatenate 'string +arbitrary-property-prefix+ property)
                    nil))
              nil)))))

(defun create-class-map (config)
  "Create the class map (trie structure) from configuration."
  (let ((theme (config-theme config))
        (class-groups (config-class-groups config)))
    (process-class-groups class-groups theme)))

(defun process-class-groups (class-groups theme)
  "Process class groups into a trie structure."
  (let ((class-map (create-class-part-object)))
    (maphash (lambda (class-group-id group)
               (process-classes-recursively group class-map class-group-id theme))
             class-groups)
    class-map))

(defun process-classes-recursively (class-group class-part-object class-group-id theme)
  "Process a class group recursively."
  (dolist (class-definition class-group)
    (process-class-definition class-definition class-part-object class-group-id theme)))

(defun process-class-definition (class-definition class-part-object class-group-id theme)
  "Process a single class definition."
  (cond
    ((stringp class-definition)
     (process-string-definition class-definition class-part-object class-group-id))
    ((functionp class-definition)
     (process-function-definition class-definition class-part-object class-group-id theme))
    ((theme-getter-p class-definition)
     ;; Theme getter - expand from theme
     (let ((theme-result (call-theme-getter class-definition theme)))
       (when theme-result
         (process-classes-recursively theme-result class-part-object class-group-id theme))))
    ((hash-table-p class-definition)
     (process-object-definition class-definition class-part-object class-group-id theme))
    ((consp class-definition)
     ;; Handle lists - check if it's a key-value pair like (:key values...) or (key values...)
     (let ((first-elem (first class-definition)))
       (cond
         ;; If first element is a keyword like :px, treat as object definition
         ((keywordp first-elem)
          (let ((key (string-downcase (symbol-name first-elem)))
                (values (rest class-definition)))
            (process-classes-recursively values
                                         (get-part class-part-object key)
                                         class-group-id
                                         theme)))
         ;; If first element is a string, treat as object definition  
         ((stringp first-elem)
          (process-classes-recursively (rest class-definition)
                                       (get-part class-part-object first-elem)
                                       class-group-id
                                       theme))
         ;; Otherwise treat as a list of definitions
         (t
          (process-classes-recursively class-definition class-part-object class-group-id theme)))))))

(defun process-string-definition (class-definition class-part-object class-group-id)
  "Process a string class definition."
  (let ((class-part-object-to-edit
          (if (string= class-definition "")
              class-part-object
              (get-part class-part-object class-definition))))
    (setf (class-group-id class-part-object-to-edit) class-group-id)))

(defun process-function-definition (class-definition class-part-object class-group-id theme)
  "Process a function class definition (validator)."
  (declare (ignore theme))
  (push (create-class-validator-object class-group-id class-definition)
        (validators class-part-object)))

(defun process-object-definition (class-definition class-part-object class-group-id theme)
  "Process an object class definition."
  (maphash (lambda (key value)
             (process-classes-recursively value
                                          (get-part class-part-object key)
                                          class-group-id
                                          theme))
           class-definition))

(defun get-part (class-part-object path)
  "Get or create a class part object for the given path."
  (let ((current class-part-object)
        (parts (split-string path +class-part-separator+)))
    (dolist (part parts)
      (let ((next (gethash part (next-part current))))
        (unless next
          (setf next (create-class-part-object))
          (setf (gethash part (next-part current)) next))
        (setf current next)))
    current))

(defun split-string (string delimiter)
  "Split string by delimiter character."
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun join-strings (strings delimiter)
  "Join strings with delimiter."
  (if (null strings)
      ""
      (reduce (lambda (a b) (concatenate 'string a delimiter b)) strings)))

;; Removed is-theme-getter-p - now using theme-getter-p from from-theme package

