=====
p4.el
=====
Perforce/Emacs integration


Introduction
------------
**p4.el** integrates the `Perforce`_ software version management system into `GNU Emacs`_ and `XEmacs`_. It provides Emacs interfaces that map directly to Perforce commands, and so is most useful if you are already familiar with Perforce and want to access it from Emacs. (If, on the other hand, you are already familiar with the Emacs `VC`_ interface, and want to add Perforce as a VC backend, then you might look at Jonathan Kamens' `VC-P4`_.)

.. _Perforce: http://www.perforce.com/
.. _GNU Emacs: http://www.gnu.org/software/emacs/
.. _XEmacs: http://www.xemacs.org/
.. _VC: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
.. _VC-P4: http://public.perforce.com/wiki/Emacs_VC-P4


Installation
------------
Download `p4.el`_. Then, in your ``.emacs``:

.. _p4.el: https://github.com/gareth-rees/p4.el/blob/master/p4.el

1. Ensure the directory containing ``p4.el`` is on your ``load-path``::

    (push "/full/path/to/dir/containing/p4.el" load-path)

2. Load the library::

    (require 'p4)

By default, the P4 global key bindings start with ``C-x p``. If you
prefer a different key prefix, then you should customize the setting
``p4-global-key-prefix``.

To compile the Perforce help text into the Emacs documentation
strings for each command, you must byte-compile ``p4.el``::

    $ emacs -Q -batch -f batch-byte-compile p4.el


Use
---

.. note::

    This section assumes that you are using the default key prefix,
    ``C-x p``. If you've customized ``p4-global-key-prefix``, then
    change the key sequences accordingly.

``p4.el`` provides an Emacs command for nearly all Perforce commands,
and they have the same name: for example the Perforce command ``p4
edit`` corresponds to the Emacs command ``p4-edit``. You can type
``C-x p C-h`` to see a list of all key bindings (but not every
Perforce command has a key binding).

Commands in ``p4.el`` operate on the "current" file by default -- this
is the file you're visiting in the current buffer, if any, or the file
on the current line in a Dired buffer. But if they are given a prefix
argument then you can enter any arguments to the command. For example
``C-x p e`` opens the current file for edit. But ``C-u C-x p e * RET``
opens all files in the current directory for edit.

These are the most useful commands:

================  ============  ===========================================
Perforce command  Key sequence  Description
================  ============  ===========================================
``add``           ``C-x p a``   Open file for add.
``annotate``      ``C-x p V``   Annotate each line with the revision it was
                                last updated.
``client``        ``C-x p c``   Edit client workspace mapping.
``edit``          ``C-x p e``   Open file for edit.
``delete``        ``C-x p x``   Open file for delete.
``diff``          ``C-x p =``   Diff local file against the depot.
``filelog``       ``C-x p f``   Show revision history of file.
``move``          ``C-x p m``   Move (rename) a file that's open for edit.
``opened``        ``C-x p o``   List open files.
``revert``        ``C-x p r``   Revert file, discarding local changes.
``submit``        ``C-x p S``   Submit changes to the depot.
``update``        ``C-x p g``   Get files from depot.
================  ============  ===========================================

Commands like ``submit`` and ``client`` open a form for editing in
Emacs. When done, submit the form to the Perforce server by typing
``C-c C-c``.


Customization
-------------

Type ``M-x customize-group RET p4 RET`` to see all the options. The
most important options are ``p4-executable`` (the location of the
Perforce client executable, in case it can't be found on your
``PATH``), and ``p4-global-key-prefix`` (the prefix for all Perforce
key bindings, in case the default ``C-x p`` is unsuitable).


License
-------
This program is free software; you can redistribute it and/or modify
it under the terms of the `GNU General Public License`_ as published by
the `Free Software Foundation`_; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the `GNU
General Public License`_ for more details.

.. _GNU General Public License: http://www.gnu.org/copyleft/gpl.html
.. _Free Software Foundation: http://www.fsf.org/
