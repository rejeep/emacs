(custom-set-variables
 '(jde-compiler (quote (("eclipse java compiler server" "/usr/share/eclipse-ecj-3.3/lib/ecj.jar"))))
 '(flymake-allowed-file-name-masks (quote (("\\.java\\'" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup))))
 )
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 )