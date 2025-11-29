(uiop:define-package #:tailwind-merge
  (:use #:cl)
  (:nicknames #:tailwind-merge/core)
  (:export #:hello
           #:make-hello
           #:say
           #:user-name))
(in-package #:tailwind-merge)


(defclass hello ()
  ((name :initarg :name
         :reader user-name))
  (:documentation "Example class."))


(defun make-hello (name)
  "Makes hello world example"
  (make-instance 'hello
                 :name name))


(defgeneric say (obj)
  (:documentation "Say what should be said.")
  (:method ((obj hello))
    (format nil "Hello, ~A!~%"
            (user-name obj))))
