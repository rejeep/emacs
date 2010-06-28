# Script Src for Emacs

Inserts a script tag for HTML and HAML documents with a URL to a given
JavaScript framework taken from: <http://scriptsrc.net/>

This is simply an Emacs clone of 
[Dr. Nic's bundle for TextMate](http://github.com/drnic/html-script-src.tmbundle).


## Usage

    (add-to-list 'load-path "/path/to/directory/or/file")
    (require 'html-script-src)

Then call the function **(html-script-src)** to insert the script tag.
