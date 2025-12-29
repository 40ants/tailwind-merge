(uiop:define-package #:tailwind-merge/vars
  (:use #:cl)
  (:export #:*color-names*
           #:*color-shades*))
(in-package #:tailwind-merge/vars)


(defvar *color-names*
  '("slate" "gray" "zinc" "neutral" "stone" "red" "orange" "amber" "yellow" "lime" "green" "emerald" "teal" "cyan" "sky" "blue" "indigo" "violet" "purple" "fuchsia" "pink" "rose" "black" "white")
  "TailwindCSS color names.")


(defvar *color-shades*
  '("50" "100" "200" "300" "400" "500" "600" "700" "800" "900" "950")
  "TailwindCSS color names.")
