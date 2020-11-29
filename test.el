;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/jpl-mode")
(require 'jpl-mode)

(defun fake-test ()
  (j-over-mini "arr =: i. 2 4 3")
  (j->emacs WWJ "arr"))

(fake-test)
