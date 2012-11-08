# rejeep emacs
This is my emacs configuration files. If you want to use them, the
best thing is probably to fork the project at Github
(<http://github.com/rejeep/emacs>) and then make your changes to that
branch.

## Installation
First of all make sure you have installed Emacs version 23 or higher.

Fetch the emacs source files:

    $ git clone git://github.com/rejeep/emacs.git ~/.emacs.d

Fetch my snippets submodule:

    $ cd ~/.emacs.d
    $ git submodule init
    $ git submodule update
    
Install all ELPA packages (make sure you have [Carton](https://github.com/rejeep/carton) installed):

    $ cd ~/.emacs.d
    $ carton

### Binaries
Some handy BASH-scripts are included in the `bin` folder. If you
find them useful, add this to `~/.bashrc`.

    export PATH="$PATH:~/.emacs.d/bin"
