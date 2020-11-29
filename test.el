;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "/home/jrn/code/jpl-mode")
(require 'jpl-mode)

(defun j-test-do/get (var speech expecting)
  (assert (= 0 (j-eval WWJ (concat var " =: " speech))))
  (let ((result (J->emacs WWJ var)))
    (assert (equal result expecting) t)))

(defun fake-test ()
  (j-test-do/get "vb" "i. 3" '[0 1 2])
  (j-test-do/get "vs" "a.{~65+i.26" "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (j-test-do/get "vi" "i. 2 2 2" '[[[0 1] [2 3]] [[4 5] [6 7]]])
  (j-test-do/get "fv" "0.1 + i. 4" '[0.1 1.1 2.1 3.1])
  (j-test-do/get "pi" "1p1" (* 2 (acos 0))))

(fake-test)
