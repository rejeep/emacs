# rejeep emacs
This is my emacs configuration files. If you want to use them, the
best thing is probably to fork the project at Github
(<http://github.com/rejeep/emacs>) and then make your changes to that
branch.

## Installation
First of all make sure you have installed Emacs version 23 or higher.

### Source
Fetch the emacs source files
    $ git clone git://github.com/rejeep/emacs.git ~/.emacs.d

### Submodules
Fetch all packages that are Git submodules.
    $ cd ~/.emacs.d
    $ git submodule init
    $ git submodule update

### ELPA
Install all [ELPA](http://tromey.com/elpa/install.html) packages
simply by running the function **package-refresh-contents** and then
**rejeep-elpa-install** (in **rejeep-elpa.el**). This command does all
that for you:
    $ cd ~/.emacs.d
    $ emacs --batch -l rejeep-elpa.el -f package-refresh-contents -f rejeep-elpa-install

### Rinari
Rinari is installed through submodules. Rinari however has submodules
of it's own. To install them:
    $ cd ~/.emacs.d/packages/rinari
    $ git submodule init
    $ git submodule update

### Binaries
Some handy BASH-scripts are included in the **bin** folder. If you
find them useful, add this to **~/.bashrc**.
    export PATH="$PATH:~/.emacs.d/bin"
