(uiop:define-package #:tailwind-merge/from-theme
  (:use #:cl)
  (:export #:from-theme
           #:theme-getter
           #:theme-getter-p
           #:call-theme-getter))
(in-package #:tailwind-merge/from-theme)

;;; Theme getter factory
;;; Ported from from-theme.ts

(defstruct (theme-getter (:constructor make-theme-getter (key)))
  "Struct to identify theme getter functions."
  (key nil :type string))

(defun from-theme (key)
  "Create a theme getter for the given theme key."
  (make-theme-getter key))

(defun call-theme-getter (getter theme)
  "Call a theme getter with a theme hash-table."
  (or (gethash (theme-getter-key getter) theme) nil))
