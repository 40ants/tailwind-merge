#!/bin/bash

qlot exec ros run --eval '(handler-case (ql:quickload :tailwind-merge-tests) (serious-condition (condition) (format *error-output* "Unable to load tests asdf system: ~A~%" condition) (uiop:quit 2)))' --eval '(handler-case (asdf:test-system :tailwind-merge-tests) (serious-condition () (uiop:quit 1)))' --quit
