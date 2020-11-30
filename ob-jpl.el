;;; ob-jpl.el --- Babel Functions for J using jpl-mode -*- lexical-binding: t; -*-

(require 'ob)
(require 'org-macs)

;; (declare-function j-console-ensure-session "ext:j-console" ())

(defcustom org-babel-jpl-command nil
  "Command to call jpl."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-babel-expand-body:jpl (body _params &optional _processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
PROCESSED-PARAMS isn't used yet."
  body)

(defun org-babel-execute:jpl (body params)
  "Execute a block of J code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing J source code block")
  (let* ((processed-params (org-babel-process-params params))
	 ;; no session => global, session => file local, 
	 (session-id (let ((session (assq :session params)))
		       (if (equal "none" (cdr session))
			   "~"
			 (buffer-file-name))))
	 ;; not needed with jpl-mode
	 (sit-time (let ((sit (assq :sit params)))
		     (if sit (cdr sit) .1)))
	 (foreign-verb (let ((verb (assq :verb params)))
			 (if verb (cdr verb) "0!:0")))
	 (plot (let ((plot (assq :plot params)))
		 (if plot (cdr plot) nil)))
	 (viewmat (let ((viewmat (assq :viewmat params)))
		    (if viewmat (cdr viewmat) nil)))
         (full-body (org-babel-expand-body:J body params processed-params))
	 (J (org-babel-j-session session-id)))
    (cond (plot
	   (j-getr J (concat "1!:44 '" default-directory "'"))
	   (j-script J body foreign-verb)
	   (j-save-plot (concat default-directory plot))
	   plot ;; (concat "[[file:" plot "]]")
	   )
	  (viewmat
	   (j-getr J (concat "1!:44 '" default-directory "'"))
	   (j-script J body foreign-verb)
	   (j-save-viewmat (concat default-directory viewmat))
	   viewmat ;; (concat "[[file:" plot "]]")
	   )
	  (t
	   (j-getr J (concat "1!:44 '" default-directory "'"))
	   (j-script J body foreign-verb)))))

(defalias 'org-babel-execute:j 'org-babel-execute:jpl)
(defalias 'org-babel-execute:J 'org-babel-execute:jpl)

(defun org-babel-j-session (session-id)
  "Get the given session's J instance, creating it if necessary.
SESSION is a parameter given by org-babel."
  (j-create-instance session-id)
  (cdr (assq 'engine (gethash session-id jpl-place->j))))

(provide 'ob-jpl)

;;; ob-J.el ends here
