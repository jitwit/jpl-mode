;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "/home/jrn/code/jpl-mode")
(require 'jpl-mode)

(defun j-test-do/get (var speech expecting)
  (assert (= 0 (j-eval WWJ (concat var " =: " speech))))
  (let ((result (J->emacs WWJ var)))
    (assert (equal result expecting) t)))

(defun j-test-set (var val)
  (emacs->J WWJ var val)
  (cond ((listp val) (assert (equal (vconcat val) (J->emacs WWJ var))))
	(t           (assert (equal val (J->emacs WWJ var))))))

(defun simple-test ()
  (j-test-do/get "vb" "i. 3" '[0 1 2])
  (j-test-do/get "vs" "a.{~65+i.26" "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (j-test-do/get "vi" "i. 2 2 2" '[[[0 1] [2 3]] [[4 5] [6 7]]])
  (j-test-do/get "fv" "0.1 + i. 4" '[0.1 1.1 2.1 3.1])
  (j-test-do/get "pi" "1p1" (* 2 (acos 0)))
  ;; issue is chop, need to handle 0 properly
  ;; (j-test-do/get "ve" "2 0 1$1" '[[] []])
  (j-test-set "abc" "def")
  (j-test-set "abc" "")
  (j-test-set "abc" "ABcd.")
  (j-test-set "abc" "A⍉B")
  (j-test-set "abc" '[0 1 2])
  (j-test-set "abc" '[0.1 1.2 2.3])
  (j-test-set "abc" '[])
  (j-test-set "abc" nil)
  (j-test-set "abc" '(1 2 3))
;;   (j-test-set "abc" '(1 2.3 3)) why wrong argument type floatp ?
  (j-test-set "abc" 2)
  (j-test-set "abc" (* 2 (acos 0))))

(simple-test)
