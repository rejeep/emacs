(require 'color-theme)
(color-theme-initialize)

;; To see all faces in effect: list-faces-display

(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "#cf6a4c"))))
 '(default ((t (:background "#191919" :foreground "#FFFFFF"))))
 '(compilation-error ((t (:background "sienna4" :bold t))))
 '(compilation-info ((t (:inherit (quote font-lock-string-face) :bold t))))
 '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
 '(completion-dynamic-face ((t (:inherit (quote match)))))

 '(diff-added ((t (:foreground "#d7ffaf"))))
 '(diff-changed ((t (:foreground "#ffc28d"))))
 '(diff-context ((t (:foreground "#888888"))))
 '(diff-indicator-added ((t (:background "#d7ffaf" :foreground "#000000"))))
 '(diff-indicator-chnaged ((t (:background "#ffc28d" :foreground "#000000"))))
 '(diff-indicator-removed ((t (:background "#ff9999" :foreground "#000000"))))
 '(diff-removed ((t (:foreground "#ff9999"))))

 '(erb-comment-delim-face ((t (:foreground "#5f5a60" :background "grey15"))))
 '(erb-comment-face ((t (:italic t :foreground "#5f5a60" :background "grey15"))))
 '(erb-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
 '(erb-face ((t (:background "grey15"))))
 '(erb-out-delim-face ((t (:foreground "#FFAEEA" :background "grey15"))))
 '(erb-out-face ((t (:background "grey15"))))

 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green")))

 '(font-lock-builtin-face ((t (:foreground "#cf6a4c"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5f5a60"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#5f5a60"))))
 '(font-lock-constant-face ((t (:foreground "#cf6a4c"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#9b703f"))))
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

 '(region ((t :background "#686868")))
 '(show-paren-match ((t (:background "#DA44FF" :foreground "#F6CCFF" :bold t))))
 )