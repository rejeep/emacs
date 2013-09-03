;;; rejeep-smartparens.el --- Smartparens configuration

(require 'smartparens-config)
(require 'smartparens-ruby)

(setq smartparens-strict-mode t)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-autoinsert-if-followed-by-word t)

(define-key sp-keymap (kbd "C-k") 'sp-kill-sexp-with-a-twist)

(defun sp-kill-sexp-with-a-twist ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (if (or (comment-only-p beg end)
            (s-matches? "\\s+" (buffer-substring-no-properties beg end)))
        (kill-line)
      (sp-kill-sexp))))

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)

(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
(define-key sp-keymap (kbd "M-r") 'sp-splice-sexp-killing-around)
;; (define-key sp-keymap (kbd "M-?") 'sp-convolute-sexp) ;; hmm

(define-key sp-keymap (kbd "C-)")  'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-}")  'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-(")  'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-{")  'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-S") 'sp-split-sexp)
(define-key sp-keymap (kbd "M-J") 'sp-join-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))

(sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)

(provide 'rejeep-smartparens)
