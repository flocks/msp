(defgroup msp nil
  "Msp customization group."
  :group 'convenience)

(defcustom msp-process-buffer-name " *msp-process-buffer*"
  "Name of the buffer that hold prettier process"
  :group 'msp
  :type 'string)


(defcustom msp-process-name "msp-prettier-process"
  "Name of the process launched by msp to run prettier"
  :group 'msp
  :type 'string)


(defcustom msp-prettier-path "prettier"
  "Path of prettier binary"
  :group 'msp
  :type 'string)

(defcustom msp-ignore-file '(".prettierignore")
  "Specify the name of the files that should be searched to determine
the --ignore-path option"
  :group 'msp
  :type '(string))

(defcustom msp-config-file '(".prettierrc" ".prettierrc.js")
  "Specify the name of the files that should be searched to determine
the --config option"
  :group 'msp
  :type '(string))

(defun msp--get-root-directory ()
  (locate-dominating-file default-directory ".git"))


(defun msp--find-config-file (dir patterns)
  "Find the first file in DIR that is inside PATTERNS."
  (let ((default-directory dir))
	(catch 'match
	  (dolist (pattern patterns)
		(when (file-exists-p (format "%s" pattern))
		  (throw 'match (expand-file-name pattern)))))))



(defun msp--sentinel (process event)
  "Sentinel run by `msp-prettify'.

When status is 0, takes the output of prettier and replace the
file with it"
  (when (eq 0 (process-exit-status process))
	(delete-file (process-get process :tmp-file))
	(let ((output (with-current-buffer (process-buffer process)
					(buffer-substring (point-min) (point-max))))
		  (file (process-get process :orig-file)))
	  (with-current-buffer (get-file-buffer file)
		(let ((point (point))
			  (scroll (window-start)))
		  (erase-buffer)
		  (insert output)
		  (goto-char point)
		  (set-window-start nil scroll)))
	  )))

(defun msp-prettify ()
  "Run prettier on current buffer.

First grap --ignore-path and --config files"
  (interactive)
  (let* ((root-folder (msp--get-root-directory))
		 (ignore-file (msp--find-config-file root-folder msp-ignore-file))
		 (config-file (msp--find-config-file root-folder msp-config-file))
		 (file-name (buffer-file-name))
		 (input (buffer-substring (point-min) (point-max)))
		 (extension (format ".%s" (file-name-extension (buffer-file-name))))
		 (tmp-file (make-temp-file "prettier" nil extension input)))
	(when (get-buffer msp-process-buffer-name)
	  (with-current-buffer msp-process-buffer-name
		(erase-buffer)))

	(let ((process (make-process
					:name msp-process-name
					:buffer msp-process-buffer-name
					:sentinel #'msp--sentinel
					:command `(,msp-prettier-path
							   ,tmp-file
							   "--config" ,config-file
							   "--ignore-path" ,ignore-file
							   "--loglevel" "silent"))))
	  (process-put process :orig-file file-name)
	  (process-put process :tmp-file tmp-file))))

(provide 'msp)
