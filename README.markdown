# rejeep emacs
This is my emacs configuration files. If you want to use them, the
best thing is probably to for the project on Github
(<http://github.com/rejeep/emacs>) and then make your changes to that
branch.

## Installation
### Source
Fetch the emacs source files
    $ git clone git://github.com/rejeep/emacs.git ~/.emacs.d

### Submodules
Fetch all packages that are Git submodules.
    $ cd ~/.emacs.d
    $ git submodule init
    $ git submodule update

### ELPA
Install all ELPA packages simply by running the function
**rejeep-elpa-install** in **rejeep-elpa.el** with **M-x
rejeep-elpa-install**. Or install them from the command line:
    $ cd ~/.emacs.d
    $ emacs -Q -l rejeep-elpa.el --eval "(rejeep-elpa-install)"

### Manual installation
Some packages have to be installed manually.

#### Yasnippet
    $ cd ~/.emacs.d/packages
    $ svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet
