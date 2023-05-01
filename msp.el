(defgroup msp nil
  "Msp customization group."
  :group 'convenience)

(defcustom msp-process-buffer-name " *msp-process-buffer*"
  "Name of the buffer that hold prettier buffer"
  :group 'msp
  :type 'string)


(defcustom msp-prettier-path "prettier"
  "Name of file to look for files to ignore by prettier"
  :group 'msp
  :type 'string)

(defcustom msp-ignore-file '(".prettierignore")
  "Name of file to look for files to ignore by prettier"
  :group 'msp
  :type '(string))

(defcustom msp-config-file '(".prettierrc" ".prettierrc.js")
  "Name of file to look for files to ignore by prettier"
  :group 'msp
  :type '(string))

(defun msp--get-root-directory ()
  (locate-dominating-file default-directory ".git"))


(defun msp--find-config-file (dir patterns)
  "Find config file in DIR according to
`msp-config-file'"
  (let ((default-directory dir))
	(catch 'match
	  (dolist (pattern patterns)
		(when (file-exists-p (format "%s" pattern))
		  (throw 'match (expand-file-name pattern)))))))



(defun msp--sentinel (process event)
  (when (eq 0 (process-exit-status process))
	(let ((output (with-current-buffer (process-buffer process)
					(buffer-substring (point-min) (point-max))))
		  (file (process-name process)))
	  (with-current-buffer (get-file-buffer file)
		(let ((point (point))
			  (scroll (window-start)))
		  (erase-buffer)
		  (insert output)
		  (goto-char point)
		  (set-window-start nil scroll)))
	  )))

(defun msp-prettify ()
  "Takes current buffer content format it with prettier
and replace the content"
  (interactive)
  (let* ((root-folder (msp--get-root-directory))
		 (ignore-file (msp--find-config-file root-folder msp-ignore-file))
		 (config-file (msp--find-config-file root-folder msp-config-file))
		 (file-name (buffer-file-name))
		 (input (buffer-substring (point-min) (point-max)))
		 (extension (format ".%s" (file-name-extension (buffer-file-name))))
		 (tmp-file (make-temp-file "prettier" nil extension input))
		 )
	(when (get-buffer msp-process-buffer-name)
	  (with-current-buffer msp-process-buffer-name
		(erase-buffer)))

	(make-process
	 :name file-name
	 :buffer msp-process-buffer-name
	 :sentinel #'msp--sentinel
	 :command `(,msp-prettier-path
				,tmp-file
				"--config" ,config-file
				"--ignore-path" ,config-file
				"--loglevel" "silent"))
	)
  )

(provide 'msp)