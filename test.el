;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/jpl-mode")
(require 'jpl-mode)

(defun fake-test ()
  (j-over-mini "vs =: a.{~65+i.26")
  (j-over-mini "vi =: i. 2 2 2")
  (j-over-mini "vf =: %: i. 3 2")
  (j-over-mini "vc =: j./~ i:1")
  (list (j->emacs WWJ "vs")
	(j->emacs WWJ "vi")
	(j->emacs WWJ "vf")
	(j->emacs WWJ "vc")))

(fake-test)
