(uiop:define-package #:tailwind-merge/lru-cache
  (:use #:cl)
  (:export #:lru-cache
           #:make-lru-cache
           #:cache-get
           #:cache-set))
(in-package #:tailwind-merge/lru-cache)

;;; LRU Cache implementation
;;; Ported from lru-cache.ts

(defclass lru-cache ()
  ((max-size :initarg :max-size
             :reader max-size
             :type integer)
   (cache-size :initarg :cache-size
               :accessor cache-size
               :type integer
               :initform 0)
   (cache :initarg :cache
          :accessor cache-table
          :type hash-table
          :initform (make-hash-table :test 'equal))
   (previous-cache :initarg :previous-cache
                   :accessor previous-cache-table
                   :type hash-table
                   :initform (make-hash-table :test 'equal))))

(defun make-lru-cache (max-cache-size)
  "Create an LRU cache with the given maximum size."
  (if (< max-cache-size 1)
      (make-instance 'lru-cache
                     :max-size 0
                     :cache-size 0)
      (make-instance 'lru-cache
                     :max-size max-cache-size
                     :cache-size 0)))

(defmethod cache-get ((cache lru-cache) key)
  "Get a value from the cache."
  (if (zerop (max-size cache))
      nil
      (let ((value (gethash key (cache-table cache))))
        (if value
            value
            (let ((prev-value (gethash key (previous-cache-table cache))))
              (when prev-value
                (cache-update cache key prev-value)
                prev-value))))))

(defmethod cache-set ((cache lru-cache) key value)
  "Set a value in the cache."
  (when (> (max-size cache) 0)
    (if (gethash key (cache-table cache))
        (setf (gethash key (cache-table cache)) value)
        (cache-update cache key value))))

(defmethod cache-update ((cache lru-cache) key value)
  "Update the cache, handling overflow."
  (setf (gethash key (cache-table cache)) value)
  (incf (cache-size cache))
  (when (> (cache-size cache) (max-size cache))
    (setf (cache-size cache) 0)
    (setf (previous-cache-table cache) (cache-table cache))
    (setf (cache-table cache) (make-hash-table :test 'equal))))

