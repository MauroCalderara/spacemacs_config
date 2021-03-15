-------------------
spacemacs config
-------------------

My configuration for spacemacs. Ideally this would be a layer but for now it is
just a folder in ``~/.emacs.d/private`` containing a few files. To have a
customized ``~/.spacemacs`` I use a symlink.

Installation
-------------

The configuration assumes that the development branch of
`spacemacs <https://github.com/syl20bnr/spacemacs>`_ is checked out to
``~/.emacs.d/`` and that the Fira Code font is available.

Following these instructions will get you started::

   # Optional: download and install a font supporting fancy symbols, e.g.
   # https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/FiraCode
   #
   # To install the font for your user extract the archive to
   # ~/.local/share/fonts, then run fc-cache -f -v and finally adjust the font
   # in the terminal that you use to run zsh.
   
   # Fetch spacemacs (note that 'develop' is the standard branch at this time so
   # the explicit checkout is not really necessary).
   git clone --depth=1 https://github.com/syl20bnr/spacemacs.git ~/.emacs.d
   (cd ~/.emacs.d && git checkout develop)

   # Fetch this repo
   git clone --depth=1 https://github.com/MauroCalderara/spacemacs_config.git \
      ~/.emacs.d/private/mmc
   
   # Create the symlink for the dot-spacemacs config
   (cd ~ && ln -s .emacs.d/private/mmc/dot_spacemacs .spacemacs)

There are some helper scripts in ``~/.emacs.d/private/mmc/scripts`` that I find
useful. You might want to add that folder to your path (note that my
`zsh config <https://github.com/MauroCalderara/zsh_config>`_ does that already).


Updating ...
---------------------------------

... my specific config
~~~~~~~~~~~~~~~~~~~~~~

Should be pretty simple::

   cd ~/.spacemacs/private/mmc
   git pull


... spacemacs
~~~~~~~~~~~~~

e.g. to get the latest developments from upstream. Note that this can break your
configuration, depending on the changes made to the overall spacemacs
development branch::

    cd ~/.spacemacs
    git pull


Customization
---------------

Most changes to the spacemacs configuration happen in ``~/.spacemacs``, which in
this case is a symlink to ``.emacs.d/private/mmc/dot_spacemacs``. I factored some
of my specific settings out into separate files, see
``.emacs.d/private/mmc/config/*.el`` and the ``dotspacemacs/user-config()``
function in ``~/.spacemacs``.
