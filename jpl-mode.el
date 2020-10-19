;;; -*- lexical-binding: t; -*-
(require 'jpl-module)
(require 'jpl-font-lock)
(require 'pretty-mode)
(require 'NuVoc)
(require 'popup)
(require 'browse-url)
(require 'filenotify)
(require 's)

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
    (j-getr J (concat "0!:0 < '" (expand-file-name j-profile-ijs) "'"))
    J))

(defvar jpl-place->j
  (make-hash-table :test 'equal)
  "Table mapping files? to J instances")

(defun jpl-delete-instance (engine)
  "delete references to J engines based on `engine's user-pointer
address from `jpl-place->j' table. I presume they are freed by
the user pointer finalizer specified in the dynamic module?"
  (mapc (lambda (buffer)
	  (let ((j (gethash buffer jpl-place->j)))
	    (when (and j (equal engine (cdr (assq 'engine j))))
	      (kill-buffer (cdr (assq 'out j)))
	      (remhash buffer jpl-place->j))))
	(hash-table-keys jpl-place->j)))

(defun jpl-delete-buffer-engine ()
  "Delete any references to engine associated with current
  buffer."
  (interactive)
  (let ((engine (gethash (buffer-file-name) jpl-place->j)))
    (and engine
	 (jpl-delete-instance (cdr (assq 'engine engine)))
	 t)))

;; j instances should have J engine, home directory, optionally:
;; project main, project test... maybe should just learn projectile
;; (or similar)?
(defun j-create-instance (where)
  "associate a location with a J, unless already associated"
  (unless (gethash where jpl-place->j)
    (let ((J (j-new))
	  (out (get-buffer-create (concat "J <" where ">"))))
      (j-getr J (concat "1!:44 '" (expand-file-name where) "'"))
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
				 "0!:2" "0!:3" nil))
    (error "j-eval invalid `foreign-verb'" foreign-verb))
  (j-getr J
	  (if (null foreign-verb)
	      speech
	    (concat foreign-verb
		    " < '"
		    (make-temp-file "jpl/" nil nil speech)
		    "'"))))

(defun j-over-mini (sentence)
  "execute J sentence from mini buffer with global J instance"
  (interactive "sJ: ")
  (jpl-check-wwj)
  (let ((vm0 (file-attributes j-viewmat-png)))
    (display-message-or-buffer
     (j-eval (cdr (assq 'engine (gethash "~" jpl-place->j)))
	     sentence))
    (unless (equal vm0 (file-attributes j-viewmat-png))
      (j-viewmat))))

(defun j-over-region (a b)
  "Send region to J"
  (interactive "r")
  (let* ((where (buffer-file-name))
	 (J (gethash where jpl-place->j))
	 (engine (cdr (assq 'engine J)))
	 (out (assq 'out J))
	 (vm0 (file-attributes j-viewmat-png)))
    (cond (J
	   (let ((sentences (buffer-substring-no-properties a b)))
	     (j-getr engine (concat "1!:44 '" default-directory "'"))
	     (pop-to-buffer (cdr out))
	     (goto-char (point-max))
	     (insert (j-eval engine sentences "0!:1"))
	     (unless (equal vm0 (file-attributes j-viewmat-png))
	       (j-viewmat))
	     (other-window 1)))
	  (t (j-create-instance where) (j-over-region a b)))))

(defun j-over-line ()
  "Send line to J"
  (interactive)
  (let ((t0 (current-time)))
    (j-over-region (point-at-bol) (point-at-eol))
    (princ (format "[jpl] dt : %fs"
		   (float-time (time-subtract (current-time) t0))))))

(defun j-over-buffer ()
  "Send buffer to J"
  (interactive)
  (let ((t0 (current-time)))
    (j-over-region (point-min) (point-max))
    (princ (format "[jpl] dt : %fs"
		   (float-time (time-subtract (current-time) t0))))))

;;;; documentation
(defun j-find-thing (thing)
  "Find information about `thing' (exact match)"
  (interactive "sthing: ")
  (seq-find #'(lambda (jentity)
                (member thing (cdadr jentity)))
            j-nuvoc))

(defun j-find-things (thing)
  "Find information about `thing' (fuzzy matches)"
  (interactive "sthing: ")
  (seq-filter #'(lambda (jentity)
		  (not
		   (null
		    (seq-find #'(lambda (tok)
				  (s-contains? thing tok))
			      (cdadr jentity)))))
              j-nuvoc))

(defun j-urls (speech)
  "Look up urls related to a string of `speech' (exact match)"
  (seq-map #'(lambda (info)
               ;; guaranteed fields
               (append (cdr (assoc 'description (cdr info)))
                       (cdr (assoc 'url (cdr info)))))
	   (apply 'append
		  (seq-map #'(lambda (entity)
			       (seq-filter #'(lambda (kv)
					       (equal (car kv) 'info))
					   (cdr entity)))
			   (j-find-things speech)))))

(defun j-names (speech)
  "Look up english names for `speech'"
  (seq-map #'car (j-urls speech)))

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
  (setq j-viewmat-buffer
	(get-buffer-create "viewmat"))
  (with-current-buffer j-viewmat-buffer
    (insert-image-file j-viewmat-png))
  (view-buffer j-viewmat-buffer))

;; probably want `make-process' with argument `:command' as `nil'?
;;;; evaluation

;;;; mode
(defvar jpl-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'j-over-buffer)
    (define-key map (kbd "C-c l") 'j-over-line)
    (define-key map (kbd "C-c i") 'j-docs)
    (define-key map (kbd "C-c j") 'joogle)
    (define-key map (kbd "M-p")   'prettify-symbols-mode)
    (define-key map (kbd "C-c k") 'jpl-delete-buffer-engine)
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
  (j-create-instance "~")
  (setq comment-start "NB. "
	commend-end ""))

(defvar WWJ
  (cdr (assq 'engine (gethash "~" jpl-place->j)))
  "world wide J")

(defun jpl-check-wwj ()
  "recreate WWJ if it's been shut down"
  (unless (gethash "~" jpl-place->j)
    (j-create-instance "~")
    (set WWJ (cdr (assq 'engine (gethash "~" jpl-place->j))))))

(provide 'jpl-mode)


