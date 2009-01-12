;;; custom.el --- Custom settings and colors

(custom-set-variables
 '(jde-compiler (quote (("eclipse java compiler server" "/usr/share/eclipse-ecj-3.3/lib/ecj.jar"))))
 '(flymake-allowed-file-name-masks (quote (("\\.java\\'" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup))))
 '(jde-jdk-registry (quote (("1.6" . "/opt/sun-jdk-1.6.0.10"))))
 '(jde-jdk (quote ("1.6")))
 '(jde-global-classpath (quote ("." "/opt/sun-jdk-1.6.0.10/jre/lib/rt.jar")))
 '(jde-complete-function (quote jde-complete-ido))
 '(ecb-options-version "2.32")
 '(truncate-lines t)
 '(openwith-associations (quote (("\\.odt|\\.doc?\\'" "oowriter" (file))
                                 ("\\.pdf\\'" "epdfview" (file))
                                 ("\\.mp3\\'" "mocp" (file))
                                 ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mov\\)\\'" "mplayer" ("-idx" file))
                                 ("\\.\\(?:jp?g\\|png\\)\\'" "xli" (file)))))
 )

(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "#cf6a4c"))))
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

 '(yas/field-highlight-face ((t (:background "#cf6a4c" :foreground "black"))))
 '(yas/mirror-highlight-face ((t nil)))

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

 '(match ((t :background "#4A6152")))
 '(minibuffer-noticeable-prompt ((t (:inherit (quote font-lock-builtin-face) :bold t))))

 '(region ((t :background "#27292A")))
 '(show-paren-match ((t (:background "#DA44FF" :foreground "#F6CCFF" :bold t))))
 
 '(mode-line ((t (:background "grey75" :foreground "black")))) 
)