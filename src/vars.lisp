(uiop:define-package #:tailwind-merge/vars
  (:use #:cl)
  (:export #:*color-names*
           #:*color-shades*
           #:*basic-color-names*))
(in-package #:tailwind-merge/vars)


(defvar *color-names*
  '("slate" "gray" "zinc" "neutral" "stone" "red" "orange" "amber" "yellow" "lime" "green" "emerald" "teal" "cyan" "sky" "blue" "indigo" "violet" "purple" "fuchsia" "pink" "rose" "black" "white")
  "TailwindCSS color names.")


(defvar *basic-color-names*
  '("black" "white" "transparent" "current" "inherit")
  "TailwindCSS basic color names which do not have a shade.")


(defvar *color-shades*
  '("50" "100" "200" "300" "400" "500" "600" "700" "800" "900" "950")
  "TailwindCSS color names.")
