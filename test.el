;;; -*- lexical-binding: t; -*-
;; not really a test, obvi

(add-to-list 'load-path "/home/jrn/code/jpl-mode")
(require 'jpl-mode)

(defun fake-test ()
  (j-over-mini "echo JVERSION [ require 'viewmat'")
  (j-do WWJ "f =: [: (,\"_1/)^:2 (3 3 $ 1 0)&(*\"0 _)")
  (j-do WWJ "((,: 255&-) ? 3 $ 256) viewmat (f ^: 4) 1"))

(fake-test)
(j-viewmat)
