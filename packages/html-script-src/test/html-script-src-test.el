(ert-deftest html-script-src-re-test ()
 (let ((textarea "<textarea id=\"fe_text_jquery\" class=\"fetext\" onChange=\"jquery.setText(this.value)\">http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js</textarea>"))
   (string-match html-script-src-re textarea)
   (should (equal (match-string-no-properties 1 textarea) "jquery"))
   (should (equal (match-string-no-properties 2 textarea) "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"))))

(ert-deftest html-script-src-scriptsrc-url-test ()
 (should (equal html-script-src-scriptsrc-url "http://scriptsrc.net/")))

(ert-deftest html-script-src-tag-format-html-test ()
 (let ((major-mode 'html-mode))
   (should (equal (html-script-src-tag-format) html-script-src-html-script-format))))

(ert-deftest html-script-src-tag-format-html-test ()
 (let ((major-mode 'haml-mode))
   (should (equal (html-script-src-tag-format) html-script-src-haml-script-format))))

(ert-deftest html-script-src-tag-html-test ()
 (let ((url "URL") (major-mode 'html-mode))
   (should (equal (html-script-src-tag url) "<script src='URL' type='text/javascript' charset='utf-8'></script>"))))

(ert-deftest html-script-src-tag-haml-test ()
 (let ((url "URL") (major-mode 'haml-mode))
   (should (equal (html-script-src-tag url) "%script{ :src => 'URL', :type => 'text/javascript', :charset => 'utf-8' }"))))
