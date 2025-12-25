(uiop:define-package #:tailwind-merge-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:tailwind-merge-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:tailwind-merge-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "tailwind-merge-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "tailwind-merge - Utility library to merge Tailwind CSS classes without style conflicts."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "CSS"
                                   "Unlicense"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"))
  (tailwind-merge system)
  "
[![](https://github-actions.40ants.com/40ants/tailwind-merge/matrix.svg?only=ci.run-tests)](https://github.com/40ants/tailwind-merge/actions)

![Quicklisp](http://quickdocs.org/badge/tailwind-merge.svg)
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :tailwind-merge)
```
""")


(defsection @usage (:title "Usage")
  """
The TAILWIND-MERGE:MERGE-TAILWIND-CLASSES function allows you to merge Tailwind CSS classes while resolving conflicts between them. It keeps the last class in case of conflicts.

```lisp
;; Basic class merging - conflicting classes are resolved by keeping the last one
(merge-tailwind-classes '("px-2" "px-3"))
;; => ("px-3")

(merge-tailwind-classes '("py-2" "px-3"))
;; => ("py-2" "px-3")  ; Non-conflicting classes are both kept

(merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
;; => ("bg-blue-500")

;; Conflict resolution - the last class wins
(merge-tailwind-classes '("h-10" "h-min"))
;; => ("h-min")

(merge-tailwind-classes '("mix-blend-normal" "mix-blend-multiply"))
;; => ("mix-blend-multiply")

;; Non-conflicting classes are preserved
(merge-tailwind-classes '("stroke-black" "stroke-1"))
;; => ("stroke-black" "stroke-1")

(merge-tailwind-classes '("outline-black" "outline-1"))
;; => ("outline-black" "outline-1")

;; Arbitrary values support
(merge-tailwind-classes '("stroke-2" "stroke-[3]"))
;; => ("stroke-[3]")

(merge-tailwind-classes '("grayscale-0" "grayscale-[50%]"))
;; => ("grayscale-[50%]")

(merge-tailwind-classes '("grow" "grow-[2]"))
;; => ("grow-[2]")
```
""")


(defautodoc @api (:system "tailwind-merge"))
