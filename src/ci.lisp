(uiop:define-package #:tailwind-merge-ci/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package #:tailwind-merge-ci/ci)


(defworkflow linter
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          ;; Until the problem with
          ;; Bug in readtable iterators or concurrent access?
          ;; will not be resolved:
          :lisp "sbcl-bin/2.5.10"
          :asdf-systems ("tailwind-merge"
                         "tailwind-merge-docs"
                         "tailwind-merge-tests"))))

(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((build-docs
          ;; Until the problem with
          ;; Bug in readtable iterators or concurrent access?
          ;; will not be resolved:
          :lisp "sbcl-bin/2.5.10"
          :asdf-system "tailwind-merge-docs")))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((run-tests
          :asdf-system "tailwind-merge"
          :lisp (;; "sbcl-bin"
                 ;; Until the problem with
                 ;; Bug in readtable iterators or concurrent access?
                 ;; will not be resolved:
                 "sbcl-bin/2.5.10"
                 ;; Issue https://github.com/roswell/roswell/issues/534
                 ;; is still reproduces on 2023-02-06:
                 "ccl-bin/1.12.0")
          :coverage t)))
