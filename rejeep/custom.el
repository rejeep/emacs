;;; custom.el --- Custom settings and colors

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(flymake-allowed-file-name-masks (quote (("\\.java\\'" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup))))
 '(jde-compiler (quote (("eclipse java compiler server" "/usr/share/eclipse-ecj-3.3/lib/ecj.jar"))))
 '(jde-complete-function (quote jde-complete-ido))
 '(jde-global-classpath (quote ("." "/usr/lib/jvm/sun-jdk-1.6")))
 '(jde-help-browser-function "w3m-browse-url")
 '(jde-help-docsets (quote (("JDK API" "/usr/share/doc/java-sdk-docs-1.6.0/html/api/" nil) ("User (javadoc)" "~/dev/apes/doc/javadoc" nil))))
 '(jde-jdk (quote ("1.6")))
 '(jde-jdk-doc-url "/usr/share/doc/java-sdk-docs-1.6.0/html/api/index.html")
 '(jde-jdk-registry (quote (("1.6" . "/usr/lib/jvm/sun-jdk-1.6"))))
 '(jde-sourcepath "/usr/lib/jvm/sun-jdk-1.6/src.zip")
 '(openwith-associations (quote (("\\.odt|\\.doc?\\'" "oowriter" (file)) ("\\.pdf\\'" "pdf" (file)) ("\\.mp3\\'" "mocp" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mov\\|ogg\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "img" (file)) ("\\.xls" "oocalc" (file)))))
 '(truncate-lines t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white"))))
 '(compilation-error ((t (:background "sienna4" :bold t))))
 '(compilation-info ((t (:inherit (quote font-lock-string-face) :bold t))))
 '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
 '(completion-dynamic-face ((t (:inherit (quote match)))))
 '(erb-comment-delim-face ((t (:foreground "#5f5a60" :background "grey15"))))
 '(erb-comment-face ((t (:italic t :foreground "#5f5a60" :background "grey15"))))
 '(erb-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
 '(erb-face ((t (:background "grey15"))))
 '(erb-out-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
 '(erb-out-face ((t (:background "grey15"))))
 '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(font-lock-builtin-face ((t (:foreground "#cf6a4c"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5f5a60"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#5f5a60"))))
 '(font-lock-constant-face ((t (:foreground "#cf6a4c"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#9B703F"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "#cda869"))))
 '(font-lock-preprocessor-face ((t (:background "#202020"))))
 '(font-lock-string-face ((t (:foreground "#8f9d6a"))))
 '(font-lock-type-face ((t (:bold t :foreground "#4b6983"))))
 '(font-lock-variable-name-face ((t (:foreground "#7587a6"))))
 '(font-lock-warning-face ((t (:underline "red"))))
 '(highlight ((t (:inherit (quote match)))))
 '(hl-line ((t :background "#0000")))
 '(ido-first-match ((t (:inherit (quote font-lock-string-face)))))
 '(ido-subdir ((t (:inherit (quote font-lock-function-name-face)))))
 '(magit-branch ((t (:foreground "red" :bold t))))
 '(match ((t :background "#4A6152")))
 '(minibuffer-noticeable-prompt ((t (:inherit (quote font-lock-builtin-face) :bold t))))
 '(minibuffer-prompt ((t (:foreground "#cf6a4c"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(region ((t :background "#FF8F8F")))
 '(show-paren-match ((t (:background "#DA44FF" :foreground "#F6CCFF" :bold t))))
 '(yas/field-highlight-face ((t (:background "#cf6a4c" :foreground "black"))))
 '(yas/mirror-highlight-face ((t nil))))
