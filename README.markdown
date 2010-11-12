# rejeep emacs
This is my emacs configuration files. If you want to use them, the
best thing is probably to fork the project at Github
(<http://github.com/rejeep/emacs>) and then make your changes to that
branch.

## Installation
First of all make sure you have installed Emacs version 23 or higher.

### Source
Fetch the emacs source files:
    $ git clone git://github.com/rejeep/emacs.git ~/.emacs.d

### Submodules
Fetch all submodules:
    $ cd ~/.emacs.d
    $ git submodule init
    $ git submodule update
    
### Rinari
Rinari requires some submodules:
    $ cd ~/.emacs.d/vendor/rinari
    $ git submodule init
    $ git submodule update

### Binaries
Some handy BASH-scripts are included in the **bin** folder. If you
find them useful, add this to **~/.bashrc**.
    export PATH="$PATH:~/.emacs.d/bin"
