;;;
;; pastie.el -- Emacs integration for pastie.org.
;;
;; Copyright (C) 2006  Christian Neukirchen <purl.org/net/chneukirchen>
;; Licensed under the GPL.
;;
;; 2007-12-27 Updated to work with more recent changes to the pastie API. 
;;            (Rob Christie)
;; 2007-12-30 Added more major mode sniffs (Ryan McGeary)
;; 2007-12-31 Added some minor mode sniffs that are Rails specific. 
;             (Rob Christie)
;; 2008-01-07 Added pastie-browse (Dan McKinley)
;; 2008-06-01 Added support for private pastes, js2-mode, and a timeout. 
;;            (Dan McKinley)
;; 2008-08-04 Fixed xml escaping. Fixed naming conflict with pastie-buffer.
;;            Updated for the new pastie.org domain. (Dan McKinley)
;;

(defcustom *pastie-last-url* ""
  "The last url pasted.")

(defcustom *pastie-restricted* t
  "When non-nil, creates private pastes.")

(defcustom *pastie-timeout* 10
  "The time, in seconds, to wait for a pastie request.")

(defun pastie-language ()
  "Sniffs for the language of the region that is being pasted"
  (or (when (boundp 'rails-view-minor-mode) 
	(if rails-view-minor-mode "html_rails"))
      (when (boundp 'rails-minor-mode) (if rails-minor-mode "ruby_on_rails"))
      (cdr (assoc major-mode '((c-mode . "c++")
                               (c++-mode . "c++")
                               (css-mode . "css")
                               (diff-mode . "diff")
                               (html-mode . "html")
                               (java-mode . "java")
			       (python-mode . "python")
                               (javascript-mode . "javascript")
			       (js2-mode . "javascript")
                               (jde-mode . "java")
                               (php-mode . "php")
                               (ruby-mode . "ruby")
                               (text-mode . "plain_text")
                               (sql-mode . "sql")
                               (sh-mode . "shell-unix-generic"))))
      "plain_text"))

(defun pastie-url-format ()
  (if *pastie-restricted* "http://pastie.org/private/%s"
    "http://pastie.org/paste/%s"))

(defun pastie-region (begin end)
  "Post the current region as a new paste at pastie.org.
Copies the URL into the kill ring."
  (interactive "r")
  (let* ((body-raw (buffer-substring-no-properties begin end))
         (body (replace-regexp-in-string
                "[<>&]"
                (lambda (match)
                  (case (string-to-char match)
                    (?< "&lt;")
                    (?> "&gt;")
                    (?& "&amp;")))
                body-raw))
         (mode (pastie-language))
         (url-request-method "POST")
         (url-mime-accept-string "application/xml")
         (url-request-extra-headers '(("Content-Type" . "application/xml")))
         (url (url-generic-parse-url "http://pastie.org/pastes"))
	 (restricted (if *pastie-restricted* "<restricted>1</restricted>"
		       ""))
         (url-request-data
          (concat "<paste>"
                  "<parser>" mode "</parser>"
                  "<authorization>burger</authorization>"
		  restricted
                  "<body>" body "</body>"
                  "</paste>")))
    (with-timeout (*pastie-timeout* (error "Pastie timed out."))
      (pastie-retrieve url))))

(defun pastie-retrieve (url)
  "Submits the request and validates the response."
  (let ((*pastie-buffer* (url-retrieve-synchronously url)))
    (with-current-buffer *pastie-buffer*
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
	(if (string-match "^20[01]" status)
	    (progn
	      (goto-char (point-max))
	      (beginning-of-line)
	      (let ((id (buffer-substring (point) (point-max))))
		(let ((url (format (pastie-url-format) id)))
		  (message "Paste created: %s" url)
		  (setq *pastie-last-url* url)
		  (kill-new url))))
	  (message "Error occured: %s" status))))
    (kill-buffer *pastie-buffer*)))

(defun pastie-buffer ()
  "Post the current buffer as a new paste at pastie.org.
Copies the URL into the kill ring."
  (interactive)
  (pastie-region (point-min) (point-max)))

(defun pastie-get (id)
  "Fetch the contents of the paste from pastie.org into a new buffer."
  (interactive "nPastie #: ")

  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
              (format "http://pastie.org/pastes/%s/download" id))))
    (setq *pastie-buffer* (url-retrieve-synchronously url))

    (with-current-buffer *pastie-buffer*
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
        (if (string-match "^200" status)
            (progn
              (search-forward-regexp
               "^Content-Disposition: attachment; filename=\"\\(.*\\)\"")
              (set-visited-file-name (match-string 1))
              (search-forward-regexp "\n\n")
              (delete-region (point-min) (point))
              (normal-mode)
              (set-buffer-modified-p nil)
              (switch-to-buffer *pastie-buffer*))
          (message "Error occured: %s" status)
          (kill-buffer *pastie-buffer*))))))

(defun pastie-browse ()
  (interactive)
  (browse-url *pastie-last-url*))

(provide 'pastie)
