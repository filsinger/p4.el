=====
p4.el
=====
Perforce/Emacs integration


Introduction
------------
**p4.el** integrates the `Perforce`_ software version management system into `GNU Emacs`_ and `XEmacs`_. It provides Emacs interfaces that map directly to Perforce commands, and so is most useful if you are already familiar with Perforce and want to access it from Emacs. (If, on the other hand, you are already familiar with the Emacs `VC`_ interface, and want to add Perforce as a VC backend, then you might look at Jonathan Kamens' `VC-P4`_.)


Installation
------------
Download `p4.el`_. Then, in your `.emacs`:

1. Add the location of `p4.el` to your `load-path` if necessary:

    (push "/full/path/to/dir/containing/file" load-path)

2. Load the library::

    (require 'p4)

3. Create a global keybinding for the Emacs/P4 keymap. Traditionally this goes on `C-x p`::

    (define-key global-map "\C-xp" 'p4-prefix-map)


License
-------
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the `GNU
General Public License`_ for more details.


.. _Perforce: http://www.perforce.com/
.. _GNU General Public License: http://www.gnu.org/copyleft/gpl.html
.. _GNU Emacs: http://www.gnu.org/software/emacs/
.. _XEmacs: http://www.xemacs.org/
.. _VC: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
.. _VC-P4: http://public.perforce.com/wiki/Emacs_VC-P4
.. _p4.el: https://github.com/gareth-rees/p4.el/blob/master/p4.el
