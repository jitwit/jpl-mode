;;; -*- lexical-binding: t; -*-
(require 'jpl-module)
(require 'jpl-font-lock)
(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)
(require 'filenotify)

;;;; group
(defgroup jpl-mode nil
  "A mode for J"
  :group 'languages
  :prefix "j-")

(defcustom jpl-mode-hook nil
  "`jpl-mode'"
  :type 'hook
  :group 'jpl)

;;;; jfe/dynamic module
(defcustom j-profile-ijs
  "~/code/jpl-mode/profile.ijs"
  "your J initialization script"
  :group 'jpl)

(defcustom j-viewmat-png
  "~/j902-user/temp/viewmat.png"
  "viewmat file"
  :group 'jpl)

(defcustom j-docs-help-index
  "~/.guix-profile/share/j/addons/docs/help/index.htm"
  "old documentation from the docs/help addon"
  :group 'jpl)

(defvar j-viewmat-buffer
  (get-buffer-create "viewmat"))

(defun j-new ()
  "create and initialize a J engine"
  (let ((J (j-engine)))
    (j-do J (concat "0!:0 < '" (expand-file-name j-profile-ijs) "'"))
    J))

(defvar jpl-place->j
  (make-hash-table :test 'equal)
  "Table mapping files? to J instances")

;; j instances should have J engine, home directory, optionally:
;; project main, project test... maybe should just learn projectile
;; (or similar)?
(defun j-create-instance (where)
  "associate a location with a J, unless already associated"
  (unless (gethash where jpl-place->j)
    (let ((J (j-new))
	  (out (get-buffer-create (concat "J <" where ">"))))
      (j-do J (concat "1!:44 '" (expand-file-name where) "'"))
      (puthash where
	       `((engine . ,J)
		 (where . ,where)
		 (out . ,out))
	       jpl-place->j))))

(defun j-eval (J speech &optional foreign-verb)
  "have `J' interpret given `speech', using a given
`foreign-verb' such as 0!:0 or 0!:1 on a temporary file
containing the `speech' or as a single sentence if `nil'."
  (unless (member foreign-verb '("0!:0" "0!:1" "0!:2" "0!:10" "0!:11"
				 "0!:100" "0!:101" "0!:110" "0!:111"
				 "0!:2" "0!:3")))
  (let ((j-out (make-temp-file "jpl/")))
    (j-smx J j-out)
    (j-do J
	  (if (null foreign-verb)
	      speech
	    (concat foreign-verb
		    " < '"
		    (make-temp-file "jpl/" nil nil speech)
		    "'")))
    (with-temp-buffer
      (insert-file-contents j-out)
      (buffer-string))))

(defun j-over-mini (sentence)
  "execute J sentence from mini buffer with global J instance"
  (interactive "sJ: ")
  (display-message-or-buffer
   (j-eval (cdr (assq 'engine (gethash "~" jpl-place->j)))
	   sentence)))

(defun j-over-region (J a b)
  "Send region to J"
  (interactive "r")
  (let ((engine (cdr (assq 'engine J)))
	(out (cdr (assq 'out J))))
    ;; todo get-buffer-create doesn't make a new one
    ;; (unless (buffer-live-p out) (setq out (get-buffer out)))
    (let ((sentences (buffer-substring-no-properties a b)))
      (j-do engine (concat "1!:44 '" default-directory "'"))
      (pop-to-buffer out)
      (goto-char (point-max))
      (insert (j-eval engine sentences))
      (goto-char (point-max))
      (other-window 1))))

(defun j-over-region* (J a b)
  "Send region to J"
  (interactive "r")
  (let ((engine (cdr (assq 'engine J)))
	(out (cdr (assq 'out J))))
    ;; todo get-buffer-create doesn't make a new one
    ;; (unless (buffer-live-p out) (setq out (get-buffer out)))
    (let ((sentences (buffer-substring-no-properties a b)))
      (j-do engine (concat "1!:44 '" default-directory "'"))
      (pop-to-buffer out)
      (goto-char (point-max))
      (insert (j-eval engine sentences "0!:0"))
      (other-window 1))))

(defun j-over-line ()
  "Send line to J"
  (interactive)
  (let* ((where (buffer-file-name))
	 (J (gethash where jpl-place->j)))
    (cond (J
	   (let ((t0 (current-time)))
	     (j-over-region J (point-at-bol) (point-at-eol))
	     (princ (format "[jpl] dt : %fs"
			    (float-time (time-subtract (current-time) t0))))))
	  (t (j-create-instance where) (j-over-line)))))

(defun j-over-buffer ()
  "Send buffer to J"
  (interactive)
  (let* ((where (buffer-file-name))
	 (J (gethash where jpl-place->j)))
    (cond (J
	   (let ((t0 (current-time)))
	     (j-over-region* J (point-min) (point-max))
	     (princ (format "[jpl] dt : %fs"
			    (float-time (time-subtract (current-time) t0))))))
	  (t (j-create-instance where) (j-over-buffer)))))

;;;; documentation
(defun j-find-thing (thing)
  "Find information about thing (exact match)"
  (interactive "sthing: ")
  (seq-find #'(lambda (jentity)
                (member thing (cdadr jentity)))
            j-nuvoc))

(defun j-urls (thing)
  "Look up urls related to a thing (exact match)"
  (let ((entity (j-find-thing thing)))
    (if entity
        (seq-map #'(lambda (info)
                     ;; guaranteed fields
                     (append (cdr (assoc 'description (cdr info)))
                             (cdr (assoc 'url (cdr info)))))
                 (seq-filter #'(lambda (kv)
                                 (equal (car kv) 'info))
                             (cdr entity)))
      nil)))

(defun j-names (thing)
  "Look up english names for thing"
  (seq-map #'car (j-urls thing)))

(defun joogle (thing)
  "Present a popup with links to information about thing"
  (interactive "sJOOGLE: ")
  (let ((urls (seq-map #'(lambda (url)
                           (popup-make-item (seq-elt url 0)
					    :value
					    (seq-elt url 1)))
                       (j-urls thing))))
    (when urls
      (browse-url (popup-menu* urls)))))

(defun j-docs ()
  "only works on my guix when j-docs-help addon is present"
  (interactive)
  (browse-url j-docs-help-index))

;;;; viewmat
(defun j-viewmat ()
  "open and view a viewmat image"
  (when (buffer-live-p j-viewmat-buffer)
    (kill-buffer j-viewmat-buffer))
  (setq j-viewmat-buffer (get-buffer-create "viewmat"))
  (with-current-buffer j-viewmat-buffer
    (insert-image-file j-viewmat-png))
  (view-buffer j-viewmat-buffer))

;; probably want `make-process' with argument `:command' as `nil'?
;;;; evaluation

;;;; mode
(defvar jpl-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c")   'j-over-buffer)
    (define-key map (kbd "C-c l")   'j-over-line)
    (define-key map (kbd "C-c i")   'j-docs)
    (define-key map (kbd "C-c j")   'joogle)
    (define-key map (kbd "M-p")     'prettify-symbols-mode)
    map)
  "Keymap for J major mode")

(define-derived-mode jpl-mode prog-mode "J"
  "Major mode for wielding J."
  :syntax-table j-syntax-table
  (setq ; one day: font-lock-multiline t
        font-lock-defaults j-font-locks
	prettify-symbols-alist j->apl) ;; (pretty-add-keywords nil j->apl)
  (use-local-map jpl-mode-keymap))

(let ((/tmp/jpl (concat (temporary-file-directory) "jpl")))
  (unless (file-exists-p /tmp/jpl)
    (mkdir /tmp/jpl))
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . jpl-mode))
  (global-set-key (kbd "M-j") 'j-over-mini)
  (j-create-instance "~"))
  ;; too shoddy for now
  ;; (file-notify-add-watch jpl-viewmat-png
  ;; 		       '(change)
  ;; 		       (lambda (e)
  ;; 			 ;; (princ e)
  ;; 			 (j-viewmat)))

(defvar WWJ
  (cdr (assq 'engine (gethash "~" jpl-place->j)))
  "world wide J")

(provide 'jpl-mode)
