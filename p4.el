;;; p4.el --- Simple Perforce-Emacs Integration

;;    Copyright (c) 1996-1997 Eric Promislow
;;    Copyright (c) 1997-2004 Rajesh Vaidheeswarran
;;    Copyright (c) 2005      Peter Osterlund
;;    Copyright (c) 2009      Fujii Hironori
;;    Copyright (c) 2012      Jason Filsinger
;;    Copyright (c) 2013      Gareth Rees <gdr@garethrees.org>
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; NOTES:
;; ------
;;
;; It is best if you take this file and byte compile it. To do that, you
;; need to do the following:
;;
;; % emacs -batch -f batch-byte-compile /full/path/to/file/p4.el
;;
;; This creates a binary file p4.elc in the path. Add the path to your
;; load-path variable in .emacs like this:
;;
;; (push "/full/path/to/dir/containing/file" load-path)
;;
;; Then load the library and assign the keymap like this:
;;
;; (require 'p4)
;; (define-key global-map "\C-cp" 'p4-prefix-map)

;;; Code:

(require 'comint)
(require 'dired)
(require 'easymenu)
(require 'ediff)
(require 'timer)
(eval-when-compile
  (require 'cl))

(defvar p4-emacs-version "10.8" "The Current P4-Emacs Integration Revision.")


;;; User options:

(defgroup p4 nil "Perforce VC System." :group 'tools)

(eval-and-compile
  ;; This is needed at compile time by p4-help-text.
  (defcustom p4-executable
    (let ((exe-name (if (memq system-type '(ms-dos windows-nt)) "p4.exe" "p4")))
      (loop for dir in (append exec-path '("/usr/local/bin" "~/bin" ""))
            for exe = (concat (file-name-as-directory dir) exe-name)
            when (and (file-executable-p exe) (not (file-directory-p exe)))
            return exe))
    "The p4 executable."
    :type 'string
    :group 'p4))

(defcustom p4-cygpath-exec "cygpath" "Path to cygpath binary on cygwin
systems."
  :type 'string
  :group 'p4)

(defcustom p4-default-diff-options "-du"
  "Options to pass to \"diff\", \"diff2\", and \"describe\" commands.
Set to:
-dn     (RCS)
-dc[n]  (context; optional argument specifies number of context lines)
-ds     (summary)
-du[n]  (unified; optional argument specifies number of context lines)
-db     (ignore whitespace changes)
-dw     (ignore whitespace)
-dl     (ignore line endings)"
  :type 'string
  :group 'p4)

(defcustom p4-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'p4)

(defcustom p4-auto-refresh t
  "Set this to automatically refresh p4 submitted files in buffers."
  :type 'boolean
  :group 'p4)

(defcustom p4-check-empty-diffs nil
  "Set this to check for files with empty diffs before submitting."
  :type 'boolean
  :group 'p4)

(defcustom p4-follow-symlinks nil
  "When set, p4 will call `file-truename' on all opened files."
  :type 'boolean
  :group 'p4)

(defcustom p4-mode-hook nil
  "Hook run by `p4-mode'."
  :type 'hook
  :group 'p4)

(defcustom p4-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `p4-set-client-name'.
"
  :type 'boolean
  :group 'p4)

;; This is also set by the command `p4-toggle-vc-mode'.
(defcustom p4-do-find-file t
  "If non-nil, the `p4-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'p4)

(defvar p4-my-clients nil
  "This variable holds the alist of p4 clients that the function
`p4-set-client-name' can complete on.

Set this variable *only* if you don't want P4 to complete on all the clients
in the P4 server.

This is an alist, and should be set using the function
`p4-set-my-clients'. For example, in your .emacs:

\(require 'p4\)
\(p4-set-my-clients \'(client1 client2 client3)\)")

(defface p4-diff-file-face
  '((((class color) (background light)) (:background "gray90"))
    (((class color) (background dark)) (:background "gray10")))
  "Face used for file pathnames in difference buffers."
  :group 'p4-faces)

(defface p4-diff-head-face
  '((((class color) (background light)) (:background "gray95"))
    (((class color) (background dark)) (:background "gray5")))
  "Face used for ?"
  :group 'p4-faces)

(defface p4-diff-inserted-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Face used for new (inserted) text in difference buffers.
When the newer revision contains text not in the older revision, that text will
be marked with this face."
  :group 'p4-faces)

(defface p4-diff-deleted-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "pink")))
  "Face used for old (deleted) text in difference buffers.
When the older revision contains text not in the newer revision, that text will
be marked with this face."
  :group 'p4-faces)

(defface p4-diff-changed-face
  '((((class color) (background light)) (:foreground "dark green"))
    (((class color) (background dark)) (:foreground "light green")))
  "Face used for changed text in difference buffers.
When a section of text is in both the newer and older revision, but differs
between them, that text will be marked with this face."
  :group 'p4-faces)

(defface p4-depot-add-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Face used for files open for add."
  :group 'p4-faces)

(defface p4-depot-branch-face
  '((((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "sky blue")))
  "Face used for files open for integrate."
  :group 'p4-faces)

(defface p4-depot-delete-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "pink")))
  "Face used for files open for delete."
  :group 'p4-faces)

(defface p4-depot-edit-face
  '((((class color) (background light)) (:foreground "dark green"))
    (((class color) (background dark)) (:foreground "light green")))
  "Face used for files open for edit."
  :group 'p4-faces)

(defface p4-form-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comment in P4 form mode."
  :group 'p4-faces)

(defface p4-form-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keyword in P4 form mode."
  :group 'p4-faces)

;; Local variables in all buffers.
(defvar p4-mode nil "P4 minor mode.")
(defvar p4-offline-mode nil "Is this file under p4 but handled in offline mode?")
(defvar p4-vc-revision nil "P4 revision to which this buffer's file is synced.")
(defvar p4-vc-status nil
  "P4 status for this buffer. A symbol:
NIL if file is not known to be under control of P4.
`add' if file is opened for add.
`branch' if file opened for integration.
`delete' if file is opened for delete.
`edit' if file is opened for edit.
`sync' if file is synced but not opened.")

;; Local variables in P4 process buffers.
(defvar p4-process-args nil "List of P4 command and arguments.")
(defvar p4-process-callback nil
  "Function run when P4 command completes successfully.")
(defvar p4-process-after-show-callback nil
  "Function run when P4 command completes successfully.")
(defvar p4-process-no-auto-login nil
  "If non-NIL, don't automatically prompt user to log in.")
(defvar p4-process-buffer nil "Original buffer that prompted this process.")

;; Local variables in P4 form buffers.
(defvar p4-form-commit-command nil
  "P4 command to run when committing this form.")
(defvar p4-form-committed nil "Form successfully committed?")

(dolist (var '(p4-mode p4-offline-mode p4-vc-revision p4-vc-status
               p4-process-args p4-process-callback
               p4-process-after-show-callback p4-process-no-auto-login
               p4-form-commit-command p4-form-committed))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defvar p4-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'p4-toggle-read-only)
    map)
  "Keymap for p4 minor mode")
(fset 'p4-minor-map p4-minor-map)
(add-to-list 'minor-mode-alist '(p4-mode p4-mode))
(add-to-list 'minor-mode-map-alist '(p4-mode . p4-minor-map))
(add-to-list 'minor-mode-alist '(p4-offline-mode p4-offline-mode))
(add-to-list 'minor-mode-map-alist '(p4-offline-mode . p4-minor-map))

(defvar p4-set-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defvar p4-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom p4-cleanup-time 600 "seconds after which `p4-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'p4)

(defcustom p4-cleanup-cache t "`p4-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'p4)


;;; Keymap:

(defvar p4-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'p4-add)
    (define-key map "b" 'p4-branches)
    (define-key map "B" 'p4-branch)
    (define-key map "c" 'p4-client)
    (define-key map "C" 'p4-changes)
    (define-key map "d" 'p4-diff2)
    (define-key map "D" 'p4-describe)
    (define-key map "e" 'p4-edit)
    (define-key map "E" 'p4-reopen)
    (define-key map "\C-f" 'p4-depot-find-file)
    (define-key map "f" 'p4-filelog)
    (define-key map "F" 'p4-files)
    (define-key map "g" 'p4-get-client-name)
    (define-key map "G" 'p4-get)
    (define-key map "h" 'p4-help)
    (define-key map "H" 'p4-have)
    (define-key map "i" 'p4-info)
    (define-key map "I" 'p4-integ)
    (define-key map "j" 'p4-job)
    (define-key map "J" 'p4-jobs)
    (define-key map "l" 'p4-label)
    (define-key map "L" 'p4-labels)
    (define-key map "\C-l" 'p4-labelsync)
    (define-key map "m" 'p4-move)
    (define-key map "o" 'p4-opened)
    (define-key map "p" 'p4-print)
    (define-key map "P" 'p4-set-p4-port)
    (define-key map "q" 'p4-pop-window-config)
    (define-key map "r" 'p4-revert)
    (define-key map "R" 'p4-refresh)
    (define-key map "\C-r" 'p4-resolve)
    (define-key map "s" 'p4-set-client-name)
    (define-key map "S" 'p4-submit)
    (define-key map "t" 'p4-toggle-vc-mode)
    (define-key map "u" 'p4-user)
    (define-key map "U" 'p4-users)
    (define-key map "v" 'p4-emacs-version)
    (define-key map "V" 'p4-annotate)
    (define-key map "w" 'p4-where)
    (define-key map "x" 'p4-delete)
    (define-key map "X" 'p4-fix)
    (define-key map "=" 'p4-diff)
    (define-key map "-" 'p4-ediff)
    map)
  "The Prefix for P4 Library Commands.")

(fset 'p4-prefix-map p4-prefix-map)


;;; Menu:

;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defvar p4-menu-spec
  '(["Specify Arguments..." universal-argument t]
    ["--" nil nil]
    ["Add Current to P4" p4-add
     (and buffer-file-name (or (not p4-do-find-file) (not p4-vc-status)))]
    ["Check out/Edit" p4-edit
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'sync)))]
    ["Re-open" p4-reopen
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'edit)))]
    ["Revert File" p4-revert
     (and buffer-file-name (or (not p4-do-find-file) (memq p4-vc-status '(add branch edit delete))))]
    ["Delete File from Depot" p4-delete
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'sync)))]
    ["Move Depot File" p4-move
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'edit)))]
    ["Submit Changes"  p4-submit t]
    ["--" nil nil]
    ["Sync Files with Depot" p4-sync t]
    ["--" nil nil]
    ["Show Opened Files" p4-opened t]
    ["Filelog" p4-filelog
     (and buffer-file-name (or (not p4-do-find-file) p4-vc-status))]
    ["Changes" p4-changes t]
    ["Describe Change" p4-describe t]
    ["--" nil nil]
    ["Diff 2 Versions" p4-diff2
     (and buffer-file-name (or (not p4-do-find-file) p4-vc-status))]
    ["Diff Current" p4-diff
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'edit)))]
    ["Diff All Opened Files" p4-diff-all-opened t]
    ["Diff Current with Ediff" p4-ediff
     (and buffer-file-name (or (not p4-do-find-file) (eq p4-vc-status 'edit)))]
    ["Diff 2 Versions with Ediff" p4-ediff2
     (and buffer-file-name (or (not p4-do-find-file) p4-vc-status))]
    ["--" nil nil]
    ["Schedule Integrations" p4-integ t]
    ["Resolve Conflicts" p4-resolve t]
    ["--" nil nil]
    ["Print" p4-print
     (and buffer-file-name (or (not p4-do-find-file) p4-vc-status))]
    ["Print with Revision History" p4-blame
     (and buffer-file-name (or (not p4-do-find-file) p4-vc-status))]
    ["Find File using Depot Spec" p4-depot-find-file t]
    ["--" nil nil]
    ["Edit a Branch Specification" p4-branch t]
    ["Edit a Label Specification" p4-label t]
    ["Edit a Client Specification" p4-client t]
    ["Edit a User Specification" p4-user t]
    ["--" nil nil]
    ["Show Version" p4-emacs-version t]
    ["Disable P4 VC Check" p4-toggle-vc-mode-off p4-do-find-file]
    ["Enable P4 VC Check" p4-toggle-vc-mode-on (not p4-do-find-file)]
    ["--" nil nil]
    ["Set P4 Config" p4-set-client-config t]
    ["Set P4 Client" p4-set-client-name t]
    ["Set P4 Port" p4-set-p4-port t]
    ["Show client info" p4-set t]
    ["Show server info" p4-info t]
    ["--" nil nil]
    )
  "The P4 menu definition")

(easy-menu-add-item nil '("tools")
		    (easy-menu-create-menu "P4" p4-menu-spec)
		    "Version Control")


;;; Environment:

(defun p4-emacs-version ()
  "Describe the (X)Emacs-P4 Integration version."
  (interactive)
  (message "%sEmacs-P4 Integration version %s"
           (if (featurep 'xemacs) "X" "")
	   p4-emacs-version))

(defun p4-current-client ()
  "Return the current P4 client."
  (p4-with-temp-buffer '("set")
    (when (re-search-forward "^P4CLIENT=\\(\\S-+\\)" nil t)
      (match-string 1))))

(defun p4-get-client-name ()
  "To get the current Perforce client,type \\[p4-get-client-name]."
  (interactive)
  (message "P4CLIENT=%s" (p4-current-client)))

(defun p4-current-server-port ()
  "Return the current P4 port."
  (p4-with-temp-buffer '("set")
    (when (re-search-forward "^P4PORT=\\(\\S-+\\)" nil t)
      (match-string 1))))

(defvar p4-server-version-cache nil
  "Association list mapping P4PORT to P4 server version on that port.")

(defun p4-server-version ()
  "Return the version number of the P4 server, or NIL if unknown."
  (let ((p4-port (p4-current-server-port)))
    (or (cdr (assoc p4-port p4-server-version-cache))
        (p4-with-temp-buffer '("info")
          (when (re-search-forward "^Server version: .*/\\([0-9]+\\)\.[0-9]+/" nil t)
            (let ((version (string-to-number (match-string 1))))
              (push (cons p4-port version) p4-server-version-cache)
              version))))))

(defun p4-set-client-name (p4-new-client-name)
  "To set the current value of P4CLIENT, type \\[p4-set-client-name].

This will change the current client from the previous client to the new
given value.

Setting this value to nil would disable P4 Version Checking.

`p4-set-client-name' will complete any client names set using the function
`p4-set-my-clients'. The strictness of completion will depend on the
variable `p4-strict-complete' (default is t).

Argument P4-NEW-CLIENT-NAME The new client to set to. The default value is
the current client."
  (interactive (list
		(completing-read "Change Client to: "
				 (if p4-my-clients
				     p4-my-clients
				   'p4-clients-completion)
				 nil p4-strict-complete (p4-current-client)
				 'p4-clients-history)
		))
  (if (or (null p4-new-client-name) (equal p4-new-client-name "nil"))
      (progn
	(setenv "P4CLIENT" nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid client name to enable."
	     )))
    (setenv "P4CLIENT" p4-new-client-name)
    (message "P4CLIENT changed to %s" p4-new-client-name)
    (run-hooks 'p4-set-client-hooks)))

(defun p4-set-client-config (p4config)
  "To set the P4CONFIG variable, for use with the current versions of the p4
client.

P4CONFIG is a more flexible mechanism wherein p4 will find the current
client automatically by checking the config file found at the root of a
directory \(recursing all the way to the top\).

In this scenario, a P4CLIENT variable need not be explicitly set.
"
  (interactive "sP4 Config: ")
  (if (or (null p4config) (equal p4config ""))
      (message "P4CONFIG not changed.")
    (setenv "P4CONFIG" p4config)
    (message "P4CONFIG changed to %s" p4config)))

(defun p4-set-my-clients (client-list)
  "To set the client completion list used by `p4-set-client-name', use
this function in your .emacs (or any lisp interaction buffer).

This will change the current client list from the previous list to the new
given value.

Setting this value to nil would disable client completion by
`p4-set-client-name'.

The strictness of completion will depend on the variable
`p4-strict-complete' (default is t).

Argument CLIENT-LIST is the 'list' of clients.

To set your clients using your .emacs, use the following:

\(require 'p4\)
\(p4-set-my-clients \'(client1 client2 client3)\)"
  (setq p4-my-clients nil)
  (let (p4-tmp-client-var)
    (while client-list
      (setq p4-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq p4-my-clients (append p4-my-clients
				  (list (list p4-tmp-client-var)))))))

(defun p4-set-p4-port (new-p4-port)
  "To set the P4PORT environment variable, type \\[p4-set-p4-port]."
  (interactive
   (list
    (read-string "Change server:port to: " (getenv "P4PORT"))))
  (setenv "P4PORT" new-p4-port)
  (if new-p4-port
      (message "P4PORT unset.")
    (message "P4PORT=%s" new-p4-port)))


;;; Window configuration:

(defvar p4-window-config-stack nil
  "Stack of saved window configurations.")

(defcustom p4-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'p4)

(defun p4-push-window-config ()
  "Push the current window configuration on the `p4-window-config-stack'
stack."
  (interactive)
  (setq p4-window-config-stack
	(cons (current-window-configuration)
	      p4-window-config-stack))
  (while (> (length p4-window-config-stack) p4-window-config-stack-size)
    (setq p4-window-config-stack
	  (nreverse (cdr (nreverse p4-window-config-stack))))))

(defun p4-pop-window-config (&optional num)
  "Pop `num' elements (default: 1) from the
`p4-window-config-stack' stack and use the last popped element to
restore the window configuration."
  (interactive "p")
  (setq num (or num 1))
  (when (> num 0)
    (setq p4-window-config-stack (nthcdr (1- num) p4-window-config-stack))
    (unless p4-window-config-stack
      (error "window config stack empty"))
    (set-window-configuration (car p4-window-config-stack))
    (setq p4-window-config-stack (cdr p4-window-config-stack))))


;;; Utilities:

(defun p4-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (p4-depot-find-file depot-name)))

(defun p4-depot-find-file (filespec)
  (interactive (list (completing-read "Enter filespec: "
				      'p4-depot-completion
				      nil nil "//"
				      'p4-depot-filespec-history)))
  (p4-with-temp-buffer (list "where" filespec)
    (cond ((looking-at "//[^ \n]+ - file(s) not in client view")
           (p4-print (list filespec)))
          ((looking-at "//[^ \n]+ //[^ \n]+ \\(.*\\)$")
           (find-file (match-string 1)))
          (t
           (error "%s" (buffer-substring (point) (line-end-position)))))))

(defun p4-make-derived-map (base-map)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map base-map)
    map))

(defun p4-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun p4-join-list (list) (mapconcat 'identity list " "))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the p4
;; buffers.
(defun p4-buffer-file-name-2 ()
  (cond ((p4-buffer-file-name))
	((get-char-property (point) 'link-client-name))
	((get-char-property (point) 'link-depot-name))
	((get-char-property (point) 'block-client-name))
	((get-char-property (point) 'block-depot-name))
	((if (and (fboundp 'dired-get-filename)
		  (dired-get-filename nil t))
	     (p4-follow-link-name (dired-get-filename nil t))))
	((p4-basic-list-get-filename))))

(defun p4-buffer-file-name ()
  (cond (buffer-file-name
	 (p4-follow-link-name buffer-file-name))
	(t nil)))

(defun p4-follow-link-name (name)
  (p4-cygpath
   (if p4-follow-symlinks
       (file-truename name)
     name)))

(defun p4-cygpath (name)
  (if (memq system-type '(cygwin32))
      (if (featurep 'xemacs)
          (replace-in-string (exec-to-string (format "%s -w %s" p4-cygpath-exec name)) "\n" "")
        (replace-regexp-in-string "\n" "" (shell-command-to-string (format "%s -w %s" p4-cygpath-exec name))))
    name))


;;; Running Perforce:

(defun p4-executable ()
  "Check if the `p4-executable' is nil, and if so, prompt the user for a
valid `p4-executable'."
  (interactive)
  (or p4-executable (call-interactively 'p4-set-p4-executable)))

(defun p4-set-p4-executable (filename)
  "Set `p4-executable' to the argument `filename'.
To set the executable for future sessions, customize
`p4-executable' instead."
    (interactive "fFull path to your P4 executable: ")
    (if (and (file-executable-p filename) (not (file-directory-p filename)))
        (setq p4-executable filename)
      (error "%s is not an executable file." filename)))

(defun p4-make-output-buffer (buffer-name &optional mode)
  "Make read only buffer and return the buffer."
  (let ((dir default-directory)
	(inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (funcall (or mode 'p4-basic-mode))
      (setq buffer-read-only t)
      (setq buffer-undo-list t)
      (cd dir)
      (current-buffer))))

(defvar p4-no-session-regexp
  (concat "\\(?:error: \\)?"
          "\\(?:Perforce password (P4PASSWD) invalid or unset\\|"
          "Your session has expired, please login again\\)")
  "Regular expression matching output from P4 when you are logged out.")

(defun p4-run (args)
  "Run p4 ARGS in the current buffer, with output after point.
Return the status of the command. If the command cannot be run
because the user is not logged in, prompt for a password and
re-run the command."
  (let ((incomplete t) status)
    (while incomplete
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point))
          (setq status (apply 'call-process (p4-executable) nil t nil args))
          (goto-char (point-min))
          (setq incomplete (looking-at p4-no-session-regexp))
          (when incomplete
            (p4-login)
            (delete-region (point-min) (point-max))))))
    status))

(defmacro p4-with-temp-buffer (args &rest body)
  "Run p4 ARGS in a temporary buffer, place point at the start of
the output, and evaluate BODY if the command completed successfully."
  `(with-temp-buffer (when (zerop (p4-run ,args)) ,@body)))

(put 'p4-with-temp-buffer 'lisp-indent-function 1)

(defun p4-output-matches (args regexp &optional group)
  "Run p4 ARGS and return a list of matches for REGEXP in the output.
With optional argument GROUP, return that matched group."
  (p4-with-temp-buffer args
    (let (result)
      (while (re-search-forward regexp nil t)
        (push (match-string (or group 0)) result))
      (nreverse result))))

(defun p4-refresh-callback (&optional revert all-buffers)
  "Return a callback function that refreshes the status of the
current buffer after a P4 command successfully completes. If
optional argument `revert' is non-NIL, revert the buffer if
modified; if optional `all-buffers' is non-NIL, refresh all
buffers."
  (lexical-let ((buffer (current-buffer))
                (revert revert)
                (all-buffers all-buffers))
    (lambda ()
      (with-current-buffer buffer
        (p4-refresh-buffer revert)
        (when all-buffers (p4-refresh-buffers))))))

(defun p4-process-show-output ()
  "Show the current buffer to the user and maybe kill it.
Return NIL if it was shown in minibuffer and killed, or non-NIL
if it was shown in a window."
  (goto-char (point-min))
  (let ((lines (count-lines (point-min) (point-max))))
    (if (or p4-process-after-show-callback (> lines 1))
        (progn
          (p4-push-window-config)
          (display-buffer (current-buffer)))
      (when (eql lines 1)
        (message (buffer-substring (point) (line-end-position))))
      (kill-buffer (current-buffer))
      nil)))

(defun p4-process-show-error (&rest args)
  "Show the contents of the current buffer as an error message.
If there's no content in the buffer, pass `args' to error instead."
  (goto-char (point-min))
  (cond ((eobp)
         (kill-buffer (current-buffer))
         (apply 'error args))
        ((eql (count-lines (point-min) (point-max)) 1)
         (let ((message (buffer-substring (point) (line-end-position))))
           (kill-buffer (current-buffer))
           (error message)))
        (t
         (p4-push-window-config)
         (display-buffer (current-buffer))
         (apply 'error args))))

(defun p4-process-sentinel (process message)
  (let ((inhibit-read-only t)
	(buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
	(cond ((and (not p4-process-no-auto-login)
                    (looking-at p4-no-session-regexp))
               (p4-login)
               (delete-region (point-min) (point-max))
               (p4-process-restart))
              ((not (string-equal message "finished\n"))
               (p4-process-show-error "Process %s %s" (process-name process)
                                      (replace-regexp-in-string "\n$" ""
                                                                message)))
              (t
               (when p4-process-callback (funcall p4-process-callback))
               (set-buffer-modified-p nil)
               (p4-process-show-output)
               (when p4-process-after-show-callback
                 (funcall p4-process-after-show-callback))))))))

(defun p4-process-restart ()
  "Start a background Perforce process in the current buffer with
command and arguments taken from the local variable `p4-process-args'."
  (set-process-sentinel
   (apply 'start-process "P4" (current-buffer) (p4-executable)
          p4-process-args)
   'p4-process-sentinel))

(defun p4-process-buffer-name (args)
  "Return a suitable buffer name for the P4 command."
  (let* ((s (p4-join-list args))
         (l (length s)))
    (if (<= l 43)
        (format "*P4 %s*" s)
      (format "*P4 %s...%s*" (substring s 0 20) (substring s (- l 20) l)))))

(defun p4-call-command (cmd &optional args mode callback after-show-callback no-auto-login)
  "Start a Perforce command in the background.
`cmd' is the P4 command to run.
`args' is a list of arguments to pass to the P4 command.
`mode' is run when creating the output buffer.
`callback' is run when the P4 command completes successfully.
`after-show-callback' is run after displaying the output.
If `no-auto-login' is non-NIL, don't try logging in if logged out."
  (with-current-buffer
      (p4-make-output-buffer (p4-process-buffer-name (cons cmd args)) mode)
    (setq p4-process-args (cons cmd args)
          p4-process-callback callback
          p4-process-after-show-callback after-show-callback
          p4-process-no-auto-login no-auto-login)
    (p4-process-restart)
    (message "Running p4 %s %s..." cmd (p4-join-list args))))


;;; Form commands:


(defun p4-form-callback (regexp cmd)
  (goto-char (point-min))
  (insert "# Created using " (p4-emacs-version) ".\n"
          "# Type C-c C-c to submit changes and exit buffer.\n"
          "# Type C-x k to kill current changes.\n"
          "#\n")
  (p4-form-mode)
  (select-window (get-buffer-window (current-buffer)))
  (when regexp (re-search-forward regexp nil t))
  (setq p4-form-commit-command cmd)
  (setq p4-form-committed nil)
  (setq buffer-offer-save t)
  (set-buffer-modified-p nil)
  (setq buffer-read-only nil)
  (message "C-c C-c to finish editing and exit buffer."))

(defun p4-form-command (cmd &optional args regexp commit-cmd)
  "Start a form-editing session.
cmd is the P4 command to run \(it must take -o and output a form\).
args is a list of arguments to pass to the P4 command.
regexp is an optional regular expression to set the cursor on.
commit-cmd is the command that will be called when
`p4-form-commit' is called \(it must take -i and a form on
standard input\). If not supplied, cmd is reused."
  (when (member "-i" args) (error "Do not specify the -i flag."))
  (when (member "-o" args) (error "Do not specify the -o flag."))

  ;; Is there already an uncommitted form with the same name? If so,
  ;; just switch to it.
  (lexical-let* ((args (cons "-o" args))
                 (regexp regexp)
                 (commit-cmd (or commit-cmd cmd))
                 (buf (get-buffer (p4-process-buffer-name (cons cmd args)))))
    (if (and buf (with-current-buffer buf (not p4-form-committed)))
        (if (get-buffer-window buf)
            (select-window (get-buffer-window buf))
          (switch-to-buffer-other-window buf))
      (p4-call-command cmd args nil nil
                       (lambda () (p4-form-callback regexp commit-cmd))))))

(defun p4-form-commit ()
  "Commit the form in the current buffer to the server."
  (interactive)
  (when p4-form-committed (error "Form already committed successfully."))
  (let* ((cmd p4-form-commit-command)
         (args '("-i"))
         (buffer (p4-make-output-buffer (p4-process-buffer-name (cons cmd args)))))
    (if (zerop (apply 'call-process-region (point-min)
                      (point-max) (p4-executable)
                      nil buffer nil
                      cmd args))
        (progn
          (p4-basic-mode)
          (set-buffer-modified-p nil)
          (setq p4-form-committed t
                buffer-read-only t
                mode-name "P4 Form Committed")
          (p4-pop-window-config)
          (with-current-buffer buffer
            (p4-process-show-output)
            (p4-quit-current-buffer)
            (p4-partial-cache-cleanup cmd)
            (when (string= cmd "submit")
              (p4-refresh-buffers))))
      (p4-process-show-error "%s -i failed to complete successfully." cmd))))


;;; P4 mode:

(defun p4-update-mode (buffer status revision)
  "Turn p4-mode on or off in `buffer' according to P4 status.
Argument `status' is one of the following symbols, based on the
file that the buffer is visiting:
`NIL' -- not known to be under P4 control;
`add' -- opened for add;
`branch' -- opened for integrate;
`delete' -- opened for delete;
`edit' -- opened for edit;
`sync' -- under P4 control but not opened.
Argument `revision' is the revision number of the file on the
client, or NIL if this is not known."
  (with-current-buffer buffer
    (setq p4-vc-status status
          p4-vc-revision revision)
    (let ((new-mode (case status
                      (sync (format " P4:%d" revision))
                      ((add branch edit) (format " P4:%s" status))
                      (t nil))))
      (when (and new-mode (not p4-mode))
        (run-hooks 'p4-mode-hook))
      (setq p4-mode new-mode))))

(defun p4-update-status-sentinel-2 (process message)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (cond ((not (string-equal message "finished\n")))
              ((not (buffer-live-p p4-process-buffer)))
              ((looking-at "^info: //[^#\n]+#\\([0-9]+\\) - ")
               (p4-update-mode p4-process-buffer 'sync
                               (string-to-number (match-string 1))))
              (t (p4-update-mode p4-process-buffer nil nil)))
        (kill-buffer (current-buffer))))))

(defun p4-update-status-sentinel-1 (process message)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (cond ((not (string-equal message "finished\n")))
              ((not (buffer-live-p p4-process-buffer)))
              ((looking-at "^info: //[^#\n]+#\\([0-9]+\\) - \\(add\\|branch\\|delete\\|edit\\) ")
               (p4-update-mode p4-process-buffer (intern (match-string 2))
                               (string-to-number (match-string 1))))
              ((looking-at "^error: .* - file(s) not opened on this client")
               (let ((args (append '("-s" "have") (cddr p4-process-args))))
                 (with-current-buffer
                     (p4-make-output-buffer (p4-process-buffer-name args))
                   (set-process-sentinel
                    (apply 'start-process "P4" (current-buffer)
                           (p4-executable) args)
                    'p4-update-status-sentinel-2)))))
        (kill-buffer (current-buffer))))))

(defun p4-update-status ()
  "Start an asynchronous update of the P4 status of the current buffer.
If the asynchronous update completes successfully, then
`p4-vc-revision' and `p4-vc-status' will be set in this buffer,
`p4-mode' will be set appropriately, and if `p4-mode' is turned
on, then `p4-mode-hook' will be run."
  (when (and p4-do-find-file buffer-file-name)
    (let ((buffer (current-buffer))
          (args (list "-s" "opened" (p4-buffer-file-name))))
      (with-current-buffer (p4-make-output-buffer (p4-process-buffer-name args))
        (set-process-sentinel
         (apply 'start-process "P4" (current-buffer)
                (p4-executable) args)
         'p4-update-status-sentinel-1)
        (setq p4-process-args args
              p4-process-buffer buffer)))))

(defun p4-refresh-buffer (&optional prompt)
  "Refresh the current buffer if it is under P4 control.
With optional argument `prompt', offer to revert it if modified."
  (let ((unmodified (not (buffer-modified-p))))
    (cond ((not buffer-file-name))
          ((and unmodified
                (not (verify-visited-file-modtime (current-buffer)))
                (file-readable-p buffer-file-name))
           (revert-buffer t))
          ((and (file-readable-p buffer-file-name)
                (or unmodified prompt))
           (revert-buffer t unmodified))
          (t (p4-update-status)))))

(defun p4-refresh-buffers ()
  "Refresh all buffers that are known to be under P4 control."
  (interactive)
  (when (or p4-auto-refresh p4-do-find-file)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when p4-vc-status
          (p4-refresh-buffer))))))

(defun p4-force-mode-line-update ()
  "Force the mode line update for different flavors of Emacs."
  (if (featurep 'xemacs)
      (redraw-modeline)
    (force-mode-line-update)))

(defun p4-toggle-vc-mode ()
  "In case, the P4 server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq p4-do-find-file (not p4-do-find-file))
  (message "P4 mode check %s" (if p4-do-find-file "enabled." "disabled.")))

(defalias 'p4-toggle-vc-mode-off 'p4-toggle-vc-mode)
(defalias 'p4-toggle-vc-mode-on 'p4-toggle-vc-mode)

;; Wrap C-x C-q to allow p4-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.
(defun p4-toggle-read-only (&optional arg)
  "If p4-mode is non-nil, \\[p4-toggle-read-only] toggles between `p4-edit'
and `p4-revert'. If ARG is non-nil, p4-offline-mode will be enabled for this
buffer before the toggling takes place. In p4-offline-mode, toggle between
making the file writable and write protected."
  (interactive "P")
  (if (and arg p4-mode)
      (setq p4-mode nil
	    p4-offline-mode t))
  (cond
   (p4-mode
    (if buffer-read-only
	(p4-edit)
      (p4-revert)))
   (p4-offline-mode
	(setq buffer-read-only (not buffer-read-only)) ;; this used to be  (toggle-read-only), but toggle-read-only shouldnt be called from elsip... lets hope this works.
    (if buffer-file-name
	(let ((mode (file-modes buffer-file-name)))
	  (if buffer-read-only
	      (setq mode (logand mode (lognot 128)))
	    (setq mode (logior mode 128)))
	  (set-file-modes buffer-file-name mode))))))

;; Break up a string into a list of words
;; (p4-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun p4-make-list-from-string (str)
  (let (lst)
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
	       (string-match "^ *\'\\([^\']*\\)\'" str)
	       (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (match-string 1 str))))
      (setq str (substring str (match-end 0))))
    lst))


;;; Defining Perforce command interfaces:

(eval-and-compile
  (defvar p4-include-help-to-command-docstring
    (let (val)
      (eval-when (compile) (setq val t))
      val))

  (defun p4-help-text (cmd text)
    (concat text
	    (with-temp-buffer
	      (if (and p4-include-help-to-command-docstring
                       (stringp p4-executable)
		       (file-executable-p p4-executable)
		       (zerop (call-process p4-executable nil t nil "help" cmd)))
		  (buffer-substring (point-min) (point-max))
		"")))))

(defmacro defp4cmd (name arglist help-cmd help-text &rest body)
  "Define a function, running p4 help `help-cmd' at compile time
to get its docstring."
  `(defun ,name ,arglist ,(p4-help-text help-cmd help-text) ,@body))

(defmacro defp4cmd* (name extra-arglist help-text args-default interactive-form &rest body)
  "Define an interactive P4 command.

`name' -- command name
`extra-arglist' -- extra optional arguments (in addition to `args')
`help-text' -- text to prepend to the P4 help
`args-default' -- form that evaluates to default list of P4 command arguments
`interactive-form' -- list of forms evaluating to extra interactive arguments
`body' -- body of command.

Inside `interactive-form': `args' is the result of evaluating
`args-default', and `args-string' consists of these arguments
joined by spaces.

Inside `body': `cmd' is `name' converted to a string, `args-orig'
is the list of P4 command arguments passed to the command, and
`args' is the actual list of P4 command arguments (either
`args-orig' if non-NIL, or the result of evaluating
`args-default' otherwise. Note that `args-default' thus appears
twice in the expansion."
  `(defp4cmd
     ,(intern (format "p4-%s" name))
     (&optional args-orig ,@extra-arglist)
     ,(format "%s" name)
     ,help-text
     (interactive
      (when current-prefix-arg
        (let* ((args ,args-default)
               (args-string (p4-join-list args)))
          (list (p4-read-args (format "p4 %s: " ',name) args-string)
                ,@interactive-form))))
     (let ((cmd (format "%s" ',name))
           (args (or args-orig ,args-default)))
       ,@body)))

(defun p4-buffer-file-name-args ()
  (let ((f (p4-buffer-file-name-2)))
    (if (not f) nil
      (list f))))

(defun p4-buffer-file-revision-args ()
  (let ((f (p4-buffer-file-name-2)))
    (if (not f) nil
      (let ((rev (get-char-property (point) 'rev)))
        (if rev (list (format "%s#%d" f rev))
          (let ((change (get-char-property (point) 'change)))
            (if change (list (format "%s@%d" f change))
              (list f))))))))


;;; Perforce command interfaces:

(defp4cmd* add (refresh-after)
  "To add the current file to the depot, type \\[p4-add]."
  (p4-buffer-file-name-args)
  (t)
  (p4-call-command cmd args nil (p4-refresh-callback nil refresh-after)))

(defp4cmd* annotate ()
  "Print a depot file with revision history to a buffer."
  (p4-buffer-file-revision-args)
  nil
  (p4-blame-int (car args)))

(defp4cmd p4-branch (args)
  "branch" "Edit a P4-BRANCH specification using \\[p4-branch]."
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 branch: " "" "branch"))))
  (if (or (null args) (equal args (list "")))
      (error "Branch must be specified!")
    (p4-form-command "branch" args "Description:\n\t")))

(defp4cmd* branches (&rest args)
  "To list all branches, type \\[p4-branches].\n"
  nil
  nil
  (p4-call-command cmd args nil
		   (lambda ()
		     (p4-regexp-create-links "^Branch \\([^ ]+\\).*\n" 'branch))))

(defp4cmd* change ()
  "To edit the change specification, type \\[p4-change].\n"
  nil
  nil
  (p4-form-command cmd args "Description:\n\t"))

(defp4cmd* changes ()
  "To list changes, type \\[p4-changes].\n"
  '("-m" "200" "...")
  nil
  (p4-file-change-log cmd args))

(defp4cmd p4-client (&rest args)
  "client" "To edit a client specification, type \\[p4-client].\n"
  (interactive (p4-read-args* "p4 client: " "" "client"))
  (p4-form-command "client" args "\\(Description\\|View\\):\n\t"))

(defp4cmd* clients ()
  "To list clients, type \\[p4-clients].\n"
  nil
  nil
  (p4-call-command cmd args nil
		   (lambda ()
		     (p4-regexp-create-links "^Client \\([^ ]+\\).*\n" 'client))))

(defp4cmd* delete ()
  "To delete the current file from the depot, type \\[p4-delete]."
  (p4-buffer-file-name-args)
  nil
  (when (yes-or-no-p "Really delete from depot? ")
    (p4-call-command cmd args nil (p4-refresh-callback))))

(defun p4-describe-internal (args)
  (p4-call-command "describe" args 'p4-diff-mode 'p4-activate-diff-buffer))

(defp4cmd p4-describe (&rest args)
  "describe"
  "To get a description for a change number, type \\[p4-describe].\n"
  (interactive (p4-read-args "p4 describe: " p4-default-diff-options))
  (p4-describe-internal args))

(defp4cmd* diff ()
  "To diff the current file and topmost depot version, type \\[p4-diff]."
  (cons p4-default-diff-options (p4-buffer-file-name-args))
  nil
  (p4-call-command cmd args 'p4-diff-mode 'p4-activate-diff-buffer))

(defun p4-get-file-rev (default-name rev)
  (if (string-match "^\\([0-9]+\\|none\\|head\\|have\\)$" rev)
      (setq rev (concat "#" rev)))
  (cond ((string-match "^[#@]" rev)
	 (concat default-name rev))
	((string= "" rev)
	 default-name)
	(t
	 rev)))

(defp4cmd p4-diff2 (prefix version1 version2)
  "diff2" "Display diff of two depot files.
When visiting a depot file, type \\[p4-diff2] and enter the versions."
  (interactive
   (let ((rev (or (get-char-property (point) 'rev) p4-vc-revision 0)))
     (list current-prefix-arg
           (p4-read-arg-string "First filespec/revision to diff: "
                               (when (> rev 1) (format "%d" (1- rev))))
	   (p4-read-arg-string "Second filespec/revision to diff: "
                               (when (> rev 1) (format "%d" rev))))))
  (let (diff-version1
	diff-version2
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if prefix
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (setq diff-version1 (p4-get-file-rev (p4-buffer-file-name-2) version1))
    (setq diff-version2 (p4-get-file-rev (p4-buffer-file-name-2) version2))

    (p4-call-command "diff2" (append diff-options
				     (list diff-version1
					   diff-version2))
		     'p4-diff-mode 'p4-activate-diff-buffer)))

(defun p4-activate-ediff-callback (&optional pop-count)
  "Return a callback function that runs ediff on the current
buffer and the P4 output buffer."
  (lexical-let ((orig-buffer (current-buffer))
                (pop-count (or pop-count 1)))
    (lambda ()
      (when (buffer-live-p orig-buffer)
        (p4-fontify-print-buffer t)
        (lexical-let ((depot-buffer (current-buffer)))
          (ediff-buffers
           orig-buffer depot-buffer
           (list (lambda ()
                   (make-local-variable 'ediff-cleanup-hook)
                   (add-hook 'ediff-cleanup-hook
                             (lambda ()
                               (p4-pop-window-config pop-count)))))))))))

(defun p4-ediff (prefix)
  "Use ediff to compare file with its original client version."
  (interactive "P")
  (if prefix
      (call-interactively 'p4-ediff2)
    (p4-call-command "print" (list (concat (p4-buffer-file-name) "#have"))
                     nil nil (p4-activate-ediff-callback))))

(defun p4-activate-ediff2-callback (other-file)
  "Return a callback function that runs ediff on the P4 output
buffer and other-file."
  (lexical-let ((other-file other-file))
    (lambda ()
      (p4-fontify-print-buffer t)
      (p4-call-command "print" (list other-file)
                       nil nil (p4-activate-ediff-callback 2)))))

(defun p4-ediff2 (version1 version2)
  "Use ediff to compare two versions of a depot file.
When visiting a depot file, type \\[p4-ediff2] and enter the versions."
  (interactive
   (let ((rev (or (get-char-property (point) 'rev) p4-vc-revision 0)))
     (list current-prefix-arg
           (p4-read-arg-string "First filespec/revision to diff: "
                               (when (> rev 1) (format "%d" (1- rev))))
	   (p4-read-arg-string "Second filespec/revision to diff: "
                               (when (> rev 1) (format "%d" rev))))))
  (let* ((file-name (p4-buffer-file-name-2))
         (basename (file-name-nondirectory file-name))
         (bufname1 (concat "*P4 ediff " basename "#" version1  "*"))
         (bufname2 (concat "*P4 ediff " basename "#" version2  "*"))
         (diff-version1 (p4-get-file-rev file-name version1))
         (diff-version2 (p4-get-file-rev file-name version2)))
    (p4-call-command "print" (list diff-version1)
                     nil nil (p4-activate-ediff2-callback diff-version2))))

(defp4cmd* edit (refresh-after)
  "To open the current depot file for edit, type \\[p4-edit]."
  (p4-buffer-file-name-args)
  (t)
  (p4-call-command cmd args nil (p4-refresh-callback t refresh-after)))

(defp4cmd* filelog ()
  "To view a history of the change made to the current file, type \\[p4-filelog]."
  (p4-buffer-file-name-args)
  nil
  (p4-file-change-log cmd args))

(defp4cmd* files ()
  "To list files in the depot, type \\[p4-files]."
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args 'p4-basic-list-mode))

(defp4cmd p4-fix (&rest args)
  "fix" "To mark jobs as being fixed by a changelist number, type \\[p4-fix].\n"
  (interactive (p4-read-args "p4 fix: " "" "job"))
  (p4-call-command "fix" args))

(defp4cmd* fixes ()
  "To list fixes, type \\[p4-fixes].\n"
  nil
  nil
  (p4-call-command cmd args))

(defp4cmd p4-group (&rest args)
  "group" "To create or edit a group specification, type \\[p4-group].\n"
  (interactive (p4-read-args* "p4 group: " "" "group"))
  (p4-form-command "group" args))

(defp4cmd p4-groups (&rest args)
  "groups" "To display list of known groups, type \\[p4-groups].\n"
  (interactive (p4-read-args* "p4 groups: " "" "group"))
  (p4-call-command "groups" args nil
		   (lambda ()
		     (p4-regexp-create-links "^\\(.*\\)\n" 'group))))

(defp4cmd* have ()
  "To list revisions last gotten, type \\[p4-have].\n"
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args 'p4-basic-list-mode))

(defp4cmd p4-help (&rest args)
  "help" "To print help message, type \\[p4-help].
Argument ARG command for which help is needed."
  (interactive (p4-read-args "p4 help: "))
  (p4-call-command "help" args))

(defp4cmd p4-info ()
  "info" "To print out server information, type \\[p4-info].\n"
  (interactive)
  (p4-call-command "info"))

(defp4cmd p4-integ (&rest args)
  "integ" "To schedule integrations between branches, type \\[p4-integ].\n"
  (interactive (p4-read-args "p4 integ: " "-b "))
  (p4-call-command "integ" args 'p4-basic-list-mode))

(defp4cmd p4-job (&rest args)
  "job" "To create or edit a job, type \\[p4-job].\n"
  (interactive (p4-read-args* "p4 job: " "" "job"))
  (p4-form-command "job" args "Description:\n\t"))

(defp4cmd* jobs ()
  "To display list of jobs, type \\[p4-jobs].\n"
  nil
  nil
  (p4-call-command cmd args))

(defp4cmd p4-jobspec ()
  "jobspec" "To edit the job template, type \\[p4-jobspec].\n"
  (interactive)
  (p4-form-command "jobspec"))

(defp4cmd p4-label (&rest args)
  "label" "Edit a P4-label specification using \\[p4-label].\n"
  (interactive (p4-read-args* "p4 label: " "" "label"))
  (if args
      (p4-form-command "label" args "Description:\n\t")
    (error "label must be specified!")))

(defp4cmd* labels ()
  "To list labels, type \\[p4-labels].\n"
  nil
  nil
  (p4-call-command cmd args nil
		   (lambda ()
		     (p4-regexp-create-links "^Label \\([^ ]+\\).*\n" 'label))))

(defp4cmd p4-labelsync (&rest args)
  "labelsync"
  "To synchronize a label with the current client contents, type \\[p4-labelsync].\n"
  (interactive (p4-read-args* "p4 labelsync: "))
  (p4-call-command "labelsync" args 'p4-basic-list-mode))

(defp4cmd* lock ()
  "To lock an opened file against changelist submission, type \\[p4-lock]."
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args nil (p4-refresh-callback t)))

(defp4cmd* login (&optional args)
  "To login by obtaining a session ticket, type \\[p4-login].\n"
  nil
  nil
  (let ((pw (if (member "-s" args) ""
              (read-passwd (format "Enter password for %s: "
                                   (p4-current-server-port))))))
    (with-temp-buffer
      (insert pw)
      (apply 'call-process-region (point-min) (point-max)
             (p4-executable) t t nil cmd args)
      (goto-char (point-min))
      (when (re-search-forward "Enter password:.*\n" nil t)
        (replace-match ""))
      (message "%s" (buffer-substring (point-min) (1- (point-max)))))))

(defp4cmd* logout ()
  "To logout by removing a session ticket, type \\[p4-logout].\n"
  nil
  nil
  (p4-call-command cmd args nil nil nil t))

(defp4cmd p4-move (from-file to-file)
  "move" "To move a file in the depot, type \\[p4-move].
If the \"move\" command is unavailable, use \"integrate\"
followed by \"delete\"."
  (interactive
   (list
    (p4-read-arg-string "move from: " (p4-buffer-file-name-2))
    (p4-read-arg-string "move to: " (p4-buffer-file-name-2))))
  (if (< (p4-server-version) 2009)
      (p4-call-command "integ" (list from-file to-file) nil
                       (lambda () (p4-call-command "delete" (list from-file))))
    (p4-call-command "move" (list from-file to-file))))

(defalias 'p4-rename 'p4-move)

(defp4cmd* opened ()
  "To list opened files, type \\[p4-opened].\n"
  nil
  nil
  (p4-call-command cmd args 'p4-basic-list-mode))

(defp4cmd* print ()
  "To print a depot file to a buffer, type \\[p4-print]."
  (p4-buffer-file-revision-args)
  nil
  (p4-call-command cmd args nil 'p4-activate-print-buffer))

(defp4cmd p4-passwd (old-pw new-pw new-pw2)
  "passwd" "To set the user's password on the server, type \\[p4-passwd].\n"
  (interactive
   (list (read-passwd "Enter old password: ")
         (read-passwd "Enter new password: ")
         (read-passwd "Re-enter new password: ")))
  (if (string= new-pw new-pw2)
      (p4-call-command "passwd" (list "-O" old-pw "-P" new-pw2))
    (error "Passwords don't match")))

(defp4cmd* reconcile ()
  "To reconcile the workspace with the depot, type \\[p4-reconcile].\n"
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args 'p4-basic-list-mode))

(defp4cmd* refresh ()
  "Refresh the contents of an unopened file. \\[p4-refresh].
Runs \"sync -f\"."
  (cons "-f" (p4-buffer-file-name-args))
  nil
  (p4-call-command "sync" args 'p4-basic-list-mode 'p4-refresh-buffers))

(defp4cmd* reopen ()
  "To change the type or changelist number of an opened file, type \\[p4-reopen]."
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args nil (p4-refresh-callback t)))

(defp4cmd* resolve ()
  "To merge open files with other revisions or files, type \\[p4-resolve].\n"
  nil
  nil
  (let (buffer (buf-name "*p4 resolve*"))
    (setq buffer (get-buffer buf-name))
    (if (and (buffer-live-p buffer)
             (not (comint-check-proc buffer)))
	(save-excursion
	  (let ((cur-dir default-directory))
	    (set-buffer buffer)
	    (cd cur-dir)
	    (goto-char (point-max))
	    (insert "\n--------\n\n"))))
    (setq args (cons cmd args))
    (setq buffer (apply 'make-comint "p4 resolve" (p4-executable) nil args))
    (set-buffer buffer)
    (comint-mode)
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))))

(defvar p4-empty-diff-regexp
  "\\(?:==== .* ====\\|--- .*\n\\+\\+\\+ .*\\)\n\\'"
  "Regular expression matching p4 diff output when there are no changes.")

(defp4cmd* revert (refresh-after)
  "To revert all change in the current file, type \\[p4-revert]."
  (p4-buffer-file-name-args)
  (t)
  (unless args-orig
    (let* ((diff-args (append (cons "diff" (p4-make-list-from-string p4-default-diff-options)) args)))
      (with-current-buffer
          (p4-make-output-buffer (p4-process-buffer-name diff-args))
        (p4-run diff-args)
        (cond ((looking-at ".* - file(s) not opened on this client")
               (p4-process-show-error))
              ((looking-at ".* - file(s) not opened for edit")
               (kill-buffer (current-buffer)))
              ((looking-at p4-empty-diff-regexp)
               (kill-buffer (current-buffer)))
              (t
               (p4-activate-diff-buffer)
               (p4-push-window-config)
               (display-buffer (current-buffer)))))))
  (when (yes-or-no-p "Really revert? ")
    (p4-call-command cmd args nil (p4-refresh-callback t))))

(defp4cmd p4-set ()
  "set" "To print out client information, type \\[p4-set].\n"
  (interactive)
  (p4-call-command "set"))

(defp4cmd* status ()
  "To identify differences between the workspace with the depot, type \\[p4-status].\n"
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args 'p4-status-list-mode))

(defun p4-empty-diff-buffer ()
  "If there exist any files opened for edit with an empty diff,
return a buffer listing those files. Otherwise, return NIL."
  (with-current-buffer (get-buffer-create "*P4 diff -sr*")
    (p4-run (list "diff" "-sr"))
    ;; The output of p4 diff -sr can be:
    ;; "File(s) not opened on this client." if no files opened at all.
    ;; "File(s) not opened for edit." if some files opened (but none for edit)
    ;; Nothing if files opened for edit (but all have changes).
    ;; List of filesnames (otherwise).
    (if (or (eobp) (looking-at "File(s) not opened"))
        (progn (kill-buffer (current-buffer)) nil)
      (current-buffer))))

(defp4cmd p4-submit (args)
  "submit" "To submit a pending change to the depot, type \\[p4-submit].\n"
  (interactive
   (if (integerp current-prefix-arg)
       (list (format "%d" current-prefix-arg))
     (p4-make-list-from-string (p4-read-arg-string "p4 change: "))))
  (p4-with-temp-buffer (list "-s" "opened")
    (unless (re-search-forward "^info: " nil t)
      (error "Files not opened on this client.")))
  (save-some-buffers nil (lambda () (or (not p4-do-find-file) p4-vc-status)))
  (let ((empty-buf (and p4-check-empty-diffs (p4-empty-diff-buffer))))
    (when (or (not empty-buf)
              (save-window-excursion
                (pop-to-buffer empty-buf)
                (ding t)
                (yes-or-no-p
                 "File with empty diff opened for edit. Submit anyway? ")))
      (p4-form-command "change" args "Description:\n\t" "submit"))))

(defp4cmd* sync ()
  "To synchronise the local view with the depot, type \\[p4-sync].\n"
  nil
  nil
  (p4-call-command cmd args 'p4-basic-list-mode 'p4-refresh-buffers))

(defalias 'p4-get 'p4-sync)

(defp4cmd* unlock ()
  "To release a locked file but leave open, type \\[p4-unlock]."
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args nil (p4-refresh-callback t)))

(defp4cmd p4-user (&rest args)
  "user" "To create or edit a user specification, type \\[p4-user].\n"
  (interactive (p4-read-args* "p4 user: " "" "user"))
  (p4-form-command "user" args))

(defp4cmd p4-users (&rest args)
  "users" "To display list of known users, type \\[p4-users].\n"
  (interactive (p4-read-args* "p4 users: " "" "user"))
  (p4-call-command "users" args nil
		   (lambda ()
		     (p4-regexp-create-links "^\\([^ ]+\\).*\n" 'user))))

(defp4cmd* where ()
  "To show how local file names map into depot names, type \\[p4-where].\n"
  (p4-buffer-file-name-args)
  nil
  (p4-call-command cmd args))


;;; Output decoration:

(defun p4-create-active-link (start end prop-list)
  (set-text-properties start end
                       (append '(face bold mouse-face highlight)
                       prop-list)))

(defun p4-move-buffer-point-to-top ()
  (let ((w (get-buffer-window (current-buffer))))
    (when w
      (save-selected-window
        (select-window w)
        (goto-char (point-min))))))

(defun p4-file-change-log (cmd file-list-spec)
  (p4-call-command cmd (cons "-l" file-list-spec) 'p4-filelog-mode
                   'p4-activate-file-change-log-buffer))

(defun p4-activate-file-change-log-buffer ()
  (p4-mark-print-buffer)
  (goto-char (point-min))
  (while (re-search-forward (concat
                             "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?[Cc]hange "
                             "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
                             "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
                             "\\(\\(?:\n\\|[ \t].*\n\\)*\\)") nil t)
    (let* ((rev-match 2)
           (rev (and (match-string rev-match)
                     (string-to-number (match-string rev-match))))
           (ch-match 3)
           (change (string-to-number (match-string ch-match)))
           (act-match 4)
           (action (match-string act-match))
           (user-match 5)
           (user (match-string user-match))
           (cl-match 6)
           (client (match-string cl-match))
           (desc-match 7))
      (when rev
        (p4-create-active-link (match-beginning rev-match)
                               (match-end rev-match)
                               `(rev ,rev)))
      (p4-create-active-link (match-beginning ch-match)
                             (match-end ch-match)
                             `(change ,change))
      (when action
        (p4-create-active-link (match-beginning act-match)
                               (match-end act-match)
                               `(action ,action rev ,rev)))
      (p4-create-active-link (match-beginning user-match)
                             (match-end user-match)
                             `(user ,user))
      (p4-create-active-link (match-beginning cl-match)
                             (match-end cl-match)
                             `(client ,client))
      (set-text-properties (match-beginning desc-match)
                           (match-end desc-match)
                           '(invisible t isearch-open-invisible t))))
  (p4-find-change-numbers (point-min) (point-max))
  (setq buffer-invisibility-spec (list))
  (p4-move-buffer-point-to-top))

(defvar p4-plaintext-change-regexp
  (concat "\\(?:[#@]\\|number\\|no\\.\\|\\)\\s-*"
          "\\([0-9]+\\)[-,]?\\s-*"
          "\\(?:and/or\\|and\\|&\\|or\\|\\)\\s-*")
  "Regexp matching a P4 change number in plain English text.")

(defun p4-find-change-numbers (start end)
  "Scan region between `start' and `end' for plain-text
references to change numbers, and make the change numbers
clickable."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(?:changes?\\|submit\\|p4\\)[:#]?[ \t\n]+" nil t)
        (save-excursion
          (while (looking-at p4-plaintext-change-regexp)
            (p4-create-active-link (match-beginning 1) (match-end 1)
                                   `(change ,(string-to-number (match-string 1))))
            (goto-char (match-end 0))))))))

(defun p4-mark-depot-list-buffer (&optional print-buffer)
  (save-excursion
    (let ((depot-regexp
	   (if print-buffer
	       "\\(^\\)\\(//[^/@# ][^/@#]*/[^@#]+\\)#[0-9]+ - "
	     "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[^/@# ][^/@#]*/[^#\n]*\\)")))
      (goto-char (point-min))
      (while (re-search-forward depot-regexp nil t)
	(let* ((p4-depot-file (match-string 2))
               (start (match-beginning 2))
               (end (match-end 2))
               (branching-op-p (and (match-string 1)
                                    (string-match "\\.\\.\\. \\.\\.\\..*"
                                                  (match-string 1))))
               (prop-list `(link-depot-name ,p4-depot-file)))
	  ;; some kind of operation related to branching/integration
	  (when branching-op-p
            (setq prop-list (append `(history-for ,p4-depot-file
                                      face p4-depot-branch-face)
				      prop-list)))
	  (p4-create-active-link start end prop-list))))))

(defun p4-fontify-print-buffer (&optional delete-filespec)
  "Fontify a p4-print buffer according to the filename in the
first line of outputput from \"p4 print\". If the optional
argument delete-filespec is non-NIL, remove the first line."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^//[^#@]+/\\([^/#@]+\\).*\n")
      (let ((buffer-file-name (match-string 1))
            (first-line (match-string-no-properties 0))
            (inhibit-read-only t))
        (replace-match "" t t)
        (set-auto-mode)
        (goto-char (point-min))
        (unless delete-filespec (insert first-line))))))

(defun p4-mark-print-buffer (&optional print-buffer)
  (p4-mark-depot-list-buffer print-buffer)
  (let ((depot-regexp
         (if print-buffer
             "^\\(//[^/@# ][^/@#]*/\\)[^@#]+#[0-9]+ - "
           "^\\(//[^/@# ][^/@#]*/\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward depot-regexp nil t)
        (let ((link-client-name (get-char-property (match-end 1)
                                                   'link-client-name))
              (link-depot-name (get-char-property (match-end 1)
                                                  'link-depot-name))
              (start (match-beginning 1))
              (end (point-max)))
          (save-excursion
            (when (re-search-forward depot-regexp nil t)
              (setq end (match-beginning 1))))
          (when link-client-name
            (set-text-properties start end
                                 `(block-client-name ,link-client-name)))
          (when link-depot-name
            (set-text-properties start end
                                 `(block-depot-name ,link-depot-name)))
          (p4-find-change-numbers start
                                  (save-excursion
                                    (goto-char start)
                                    (line-end-position))))))))

(defun p4-activate-print-buffer ()
  (p4-fontify-print-buffer)
  (p4-mark-print-buffer t)
  (use-local-map p4-basic-mode-map))


(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(set-text-properties start end `(face ,face-property))))))

(defun p4-activate-diff-buffer ()
  (save-excursion
    (p4-mark-depot-list-buffer)
    (if p4-colorized-diffs
	(progn
	  (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
	  (p4-buffer-set-face-property "^[@*].*" 'p4-diff-head-face)
	  (p4-buffer-set-face-property "^\\([+>].*\n\\)+" 'p4-diff-inserted-face)
	  (p4-buffer-set-face-property "^\\([-<].*\n\\)+" 'p4-diff-deleted-face)
	  (p4-buffer-set-face-property "^\\(!.*\n\\)+" 'p4-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n"
			      nil t)
      (let* ((link-depot-name (get-char-property (match-end 1) 'link-depot-name))
	     (start (match-beginning 0))
	     (end (save-excursion
		    (if (re-search-forward "^==== " nil t)
			(match-beginning 0)
		      (point-max)))))
	(when link-depot-name
          (set-text-properties start end `(block-depot-name ,link-depot-name)))))

    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
		    "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-number (match-string 2)))
	    (start (match-beginning 3))
	    (end (match-end 3)))
	(set-text-properties start end `(first-line ,first-line start ,start))))

    (goto-char (point-min))
    (let ((stop
	   (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
	       (match-beginning 0)
	     (point-max))))
      (p4-find-change-numbers (point-min) stop))

    (goto-char (point-min))
    (if (looking-at "^Change [0-9]+ by \\([^ @]+\\)@\\([^ \n]+\\)")
	(let ((user-match 1)
	      (cl-match 2)
	      cur-user cur-client)
	  (setq cur-user (match-string user-match))
	  (setq cur-client (match-string cl-match))
	  (p4-create-active-link (match-beginning user-match)
				 (match-end user-match)
				 `(user ,cur-user))
	  (p4-create-active-link (match-beginning cl-match)
				 (match-end cl-match)
				 `(client ,cur-client))))))

(defun p4-regexp-create-links (regexp property)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (p4-create-active-link (match-beginning 1) (match-end 1)
                               (list property (match-string 1)))))))


;;; Annotation:

(defconst p4-blame-change-regex
  (concat "^\\.\\.\\. #"     "\\([0-9]+\\)"   ;; revision
	  "\\s-+change\\s-+" "\\([0-9]+\\)"   ;; change
	  "\\s-+"            "\\([^ \t]+\\)"  ;; type
	  "\\s-+on\\s-+"     "\\([^ \t]+\\)"  ;; date
	  "\\s-+by\\s-+"     "\\([^ \t]+\\)"  ;; author
	  "@"))

(defconst p4-blame-revision-regex
  (concat "^\\([0-9]+\\),?"
	  "\\([0-9]*\\)"
	  "\\([acd]\\)"
	  "\\([0-9]+\\),?"
	  "\\([0-9]*\\)"))

(defalias 'p4-blame 'p4-annotate)
(defalias 'p4-print-with-rev-history 'p4-annotate)

(defun p4-annotate-line ()
  "Print a depot file with revision history to a buffer,
and jump to the current line in the revision buffer."
  (p4-blame-int (car (p4-buffer-file-revision-args))
                (line-number-at-pos (point))))

(defalias 'p4-blame-line 'p4-annotate-line)

(defstruct p4-file-revision filespec filename revision change date user annotation)

(defun p4-link (width fmt value properties)
  "Format `value' using `fmt' and insert into a field of `width'.
Make it into an active link with `properties'."
  (let* ((text (format fmt value))
         (length (length text))
         (text (if (< length width) text (substring text 0 (- width 1))))
         (p (point)))
    (insert text)
    (p4-create-active-link p (point) properties)
    (insert (make-string (- width (length text)) ? ))))

(defun p4-file-revision-links (rev)
  (let ((links (p4-file-revision-annotation rev)))
    (or links
        (with-temp-buffer
          (let* ((change (p4-file-revision-change rev))
                 (filename (p4-file-revision-filename rev))
                 (revision (p4-file-revision-revision rev))
                 (user (p4-file-revision-user rev)))
            (p4-link 7 "%6d" change `(change ,change))
            (p4-link 5 "%4d" revision
                     `(rev ,revision link-depot-name ,filename))
            (insert (format "%10s " (p4-file-revision-date rev)))
            (p4-link 9 "%7s: " user `(user ,user)))
          (setf (p4-file-revision-annotation rev) 
                (buffer-substring (point-min) (point-max)))))))

(defun p4-parse-filelog (filespec)
  "Parse the filelog for `filespec'.
Return an association list mapping revision number to a
`p4-file-revision' structure, in reverse order (earliest revision
first)."
  (let (head-seen       ; head revision not deleted?
        change-alist    ; alist mapping change to p4-file-revision structures
        current-file    ; current filename in filelog
        (args (list "filelog" "-i" filespec)))
    (message "Running p4 %s..." (p4-join-list args))
    (p4-with-temp-buffer args
      (while (not (eobp))
	(cond ((looking-at "^//.*$")
               (setq current-file (match-string 0)))
              ((looking-at p4-blame-change-regex)
               (let ((op (match-string 3))
                     (revision (string-to-number (match-string 1)))
                     (change (string-to-number (match-string 2))))
                 (if (string= op "delete")
                     (unless head-seen (goto-char (point-max)))
                   (push (cons change
                               (make-p4-file-revision
                                :filespec (format "%s#%d" current-file revision)
                                :filename current-file
                                :revision revision
                                :change change
                                :date (match-string 4)
                                :user (match-string 5)))
                         change-alist)
                   (setq head-seen t)))))
        (forward-line))
      change-alist)))

(defun p4-annotate-changes (filespec)
  "Return a list of change numbers, one for each line of `filespec'."
  (let ((args (list "annotate" "-c" "-q" filespec)))
    (message "Running p4 %s..." (p4-join-list args))
    (p4-with-temp-buffer args
      (loop while (re-search-forward "^\\([0-9]+\\):" nil t)
            collect (string-to-number (match-string 1))))))

(defun p4-annotate-changes-by-patching (filespec change-alist)
  "Return a list of change numbers, one for each line of `filespec'.
This builds the result by walking through the changes in
`change-alist' one at a time, fetching the diff, and applying the
patch. This is very slow for files with many revisions, so should
only be used when p4 annotate is unavailable."
  (let* ((base (cdar change-alist))
         (base-change (caar change-alist))
         (base-change-string (format "%d\n" base-change))
         (buffer (get-buffer-create "*P4 annotate tmp*")))
    (with-current-buffer buffer
      (erase-buffer)
      (message "Patching for change %d..." base-change)
      (p4-run (list "print" "-q" (p4-file-revision-filespec base)))
      (while (re-search-forward ".*\n" nil t)
        (replace-match base-change-string t t))
      (loop for (c1 . f1) in change-alist
            and (c2 . f2) in (cdr change-alist)
            for change-string = (format "%d\n" c2)
            do (p4-with-temp-buffer
                   (list "diff2"
                         (p4-file-revision-filespec f1)
                         (p4-file-revision-filespec f2))
                 (message "Patching for change %d..." c2)
                 (goto-char (point-max))
                 (while (re-search-backward p4-blame-revision-regex nil t)
                   (let ((la (string-to-number (match-string 1)))
                         (lb (string-to-number (match-string 2)))
                         (op (match-string 3))
                         (ra (string-to-number (match-string 4)))
                         (rb (string-to-number (match-string 5))))
                     (when (= lb 0) (setq lb la))
                     (when (= rb 0) (setq rb ra))
                     (cond ((string= op "a") (incf la))
                           ((string= op "d") (incf ra)))
                     (with-current-buffer buffer
                       (p4-goto-line la)
                       (delete-region (point)
                                      (progn (forward-line (1+ (- lb la)))
                                             (point)))
                       (while (<= ra rb)
                         (insert change-string)
                         (incf ra)))))))
      (goto-char (point-min))
      (loop while (re-search-forward "[0-9]+" nil t)
            collect (string-to-number (match-string 0))))))

(defun p4-blame-int (filespec &optional src-line)
  ;; make sure the filespec is unambiguous
  (p4-with-temp-buffer (list "files" filespec)
    (when (> (count-lines (point-min) (point-max)) 1)
      (error "File pattern maps to more than one file.")))
  (let ((file-change-alist (p4-parse-filelog filespec)))
    (unless file-change-alist (error "%s not available" filespec))
    (let* ((line-changes
            (if (< (p4-server-version) 2004)
                (p4-annotate-changes-by-patching filespec file-change-alist)
              (p4-annotate-changes filespec)))
           (lines (length line-changes))
           (inhibit-read-only t)
           (current-line 0)
           (current-percent -1)
           current-change)
      (with-current-buffer
          (p4-make-output-buffer (p4-process-buffer-name (list "annotate" filespec)))
        (p4-run (list "print" filespec))
        (p4-fontify-print-buffer)
        (forward-line 1)
        (insert "Change  Rev       Date  Author\n")
        (dolist (change line-changes)
          (incf current-line)
          (let ((percent (/ (* current-line 100) lines)))
            (when (> percent current-percent)
              (message "Formatting...%d%%" percent)
              (incf current-percent 10)))
          (if (eql change current-change)
              (insert (format "%29s : " ""))
            (insert (p4-file-revision-links
                     (cdr (assoc change file-change-alist)))))
          (setq current-change change)
          (forward-line))
        (goto-char (point-min))
        (p4-mark-print-buffer)
        (message "Formatting...done")
        (setq truncate-lines t)
        (use-local-map p4-annotate-mode-map)
        (display-buffer (current-buffer))
        (when src-line
          (switch-to-buffer-other-window (current-buffer))
          (p4-goto-line (+ 2 src-line)))))))


;;; Completion:

(defvar p4-depot-filespec-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a job and
cdr is the list of answers??")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a label and
cdr is the list of answers??")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a user and
cdr is the list of answers??")

(defvar p4-groups-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a group and
cdr is the list of answers??")

(defvar p4-arg-string-history nil
  "History for p4 command arguments")

(defun p4-depot-completion-search (filespec cmd)
  "Look into `p4-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
	    ((equal cmd "branches") p4-branches-completion-cache)
	    ((equal cmd "clients") p4-clients-completion-cache)
	    ((equal cmd "dirs") p4-depot-completion-cache)
	    ((equal cmd "jobs") p4-jobs-completion-cache)
	    ((equal cmd "labels") p4-labels-completion-cache)
	    ((equal cmd "users") p4-users-completion-cache)
	    ((equal cmd "groups") p4-groups-completion-cache)))
	dir list)

    (if (and p4-cleanup-cache (not p4-timer))
	(setq p4-timer
              (if (featurep 'xemacs)
                  (add-timeout p4-cleanup-time 'p4-cache-cleanup nil nil)
                (run-at-time p4-cleanup-time nil 'p4-cache-cleanup))))
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
	  (progn
	    ;; filespec is included in cache
	    (if (string= (car (car l)) filespec)
		(setq list (cdr (car l)))
	      (setq dir (cdr (car l)))
	      (while dir
		(if (string-match (concat "^" filespec) (car dir))
		    (setq list (cons (car dir) list)))
		(setq dir (cdr dir))))
	    (setq l nil
		  list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the p4 caches ...")
  (setq p4-branches-completion-cache nil)
  (setq p4-clients-completion-cache nil)
  (setq p4-depot-completion-cache nil)
  (setq p4-jobs-completion-cache nil)
  (setq p4-labels-completion-cache nil)
  (setq p4-users-completion-cache nil)
  (setq p4-groups-completion-cache nil)
  (if (featurep 'xemacs)
      (when p4-timer (disable-timeout p4-timer))
    (when (timerp p4-timer) (cancel-timer p4-timer)))
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches ... done."))

(defun p4-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
	 (setq p4-branches-completion-cache nil))
	((string= type "client")
	 (setq p4-clients-completion-cache nil))
	((or (string= type "submit") (string= type "change"))
	 (setq p4-depot-completion-cache nil))
	((string= type "job")
	 (setq p4-jobs-completion-cache nil))
	((string= type "label")
	 (setq p4-labels-completion-cache nil))
	((string= type "user")
	 (setq p4-users-completion-cache nil))
	((string= type "group")
	 (setq p4-groups-completion-cache nil))))

(defun p4-completion-helper (spec cmd var regexp)
  "Run p4 CMD, collect strings from output matching group 1 in REGEXP,
push SPEC onto the front of the list, assign the list to VAR, and
return the list."
  (let ((list (p4-output-matches (list cmd) regexp 1)))
    (set var (cons (cons spec list) (eval var)))
    list))

(defun p4-depot-completion-build (spec cmd)
  "Return list of depot objects beginning with SPEC that are
suitable for passing to CMD."
  (message "Making %s completion list..." cmd)
  (let (output-buffer line list)
    (cond
     ((equal cmd "branches")
      (setq list (p4-completion-helper
		  spec cmd 'p4-branches-completion-cache
		  "^Branch \\([^ \n]*\\) [0-9]+/")))
     ((equal cmd "clients")
      (setq list (p4-completion-helper
		  spec cmd 'p4-clients-completion-cache
		  "^Client \\([^ \n]*\\) [0-9]+/")))
     ((equal cmd "dirs")
      (setq list
            (append (p4-output-matches (list "dirs" (concat spec "*"))
                                       "^//[^ \n]+$")
                    (p4-output-matches (list "files" (concat spec "*"))
                                       "^\\(//[^#\n]+\\)#[0-9]+ - " 1)))
      (push (cons spec list) p4-depot-completion-cache))
     ((equal cmd "jobs")
      (setq list (p4-completion-helper
		  spec cmd 'p4-jobs-completion-cache
		  "\\([^ \n]*\\) on [0-9]+/")))
     ((equal cmd "labels")
      (setq list (p4-completion-helper
		  spec cmd 'p4-labels-completion-cache
		  "^Label \\([^ \n]*\\) [0-9]+/")))
     ((equal cmd "users")
      (setq list (p4-completion-helper
		  spec cmd 'p4-users-completion-cache
		  "^\\([^ \n]+\\)")))
     ((equal cmd "groups")
      (setq list (p4-completion-helper
		  spec cmd 'p4-groups-completion-cache
		  "^\\([^ \n]+\\)"))))
    (message nil)
    (cons spec list)))

(defun p4-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for Perforce " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")
     (let (list)
       ,(if (string= type "dirs")
	    ;; when testing for an exact match, remove trailing /
	    `(if (and (eq action 'lambda)
		      (eq (aref string (1- (length string))) ?/))
		 (setq string (substring string 0 (1- (length string))))))

       ;; First, look in cache
       (setq list (p4-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
	   (setq list (p4-depot-completion-build string ,type)))

       (cond
	;; try completion
	((null action)
	 (try-completion string (mapcar 'list (cdr list)) predicate))
	;; all completions
	((eq action t)
	 (let ((lst
		(all-completions string (mapcar 'list (cdr list)) predicate)))
	   ,(if (string= type "dirs")
		`(setq lst (mapcar (lambda (s)
				     (if (string-match ".*/\\(.+\\)" s)
					 (match-string 1 s)
				       s))
				   lst)))
	   lst))
	;; Test for an exact match
	(t
	 (and (>= (length list) 2)
	      (member (car list) (cdr list))))))))

(defalias 'p4-branches-completion (p4-completion-builder "branches"))
(defalias 'p4-clients-completion (p4-completion-builder "clients"))
(defalias 'p4-depot-completion (p4-completion-builder "dirs"))
(defalias 'p4-jobs-completion (p4-completion-builder "jobs"))
(defalias 'p4-labels-completion (p4-completion-builder "labels"))
(defalias 'p4-users-completion (p4-completion-builder "users"))
(defalias 'p4-groups-completion (p4-completion-builder "groups"))

(defun p4-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
		     (cond ((not type)
			    'p4-arg-string-completion)
			   ((string= type "branch")
			    'p4-branch-string-completion)
			   ((string= type "client")
			    'p4-client-string-completion)
			   ((string= type "label")
			    'p4-label-string-completion)
			   ((string= type "job")
			    'p4-job-string-completion)
			   ((string= type "user")
			    'p4-user-string-completion)
			   ((string= type "group")
			    'p4-group-string-completion))
		     nil nil
		     initial 'p4-arg-string-history)))

(defun p4-read-args (prompt &optional initial-input completion-type)
  (p4-make-list-from-string
   (p4-read-arg-string prompt initial-input completion-type)))

(defun p4-read-args* (prompt &optional initial-input completion-type)
  (p4-make-list-from-string
   (if current-prefix-arg
       (p4-read-arg-string prompt initial-input completion-type)
     initial-input)))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	(progn
	  (setq first-part (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
	   (setq completion (p4-branches-completion string predicate action)))
	  ((string-match "-t +$" first-part)
	   (setq completion (p4-list-completion
			     string (list "text " "xtext " "binary "
					  "xbinary " "symlink ")
			     predicate action)))
	  ((string-match "-j +$" first-part)
	   (setq completion (p4-jobs-completion string predicate action)))
	  ((string-match "-l +$" first-part)
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*status=\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "open " "closed " "suspended ")
			     predicate action)))
	  ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
	       (string-match "\\(.*@\\)\\(.*\\)" string))
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*#\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "none" "head" "have")
			     predicate action)))
	  ((string-match "^//" string)
	   (setq completion (p4-depot-completion string predicate action)))
	  ((string-match "\\(^-\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "a " "af " "am " "as " "at " "ay "
					  "b " "c " "d " "dc " "dn "
					  "ds " "du " "e " "f " "i " "j "
					  "l " "m " "n " "q " "r " "s " "sa "
					  "sd " "se " "sr " "t " "v ")
			     predicate action)))
	  (t
	   (setq completion (p4-file-name-completion string
						     predicate action))))
    (cond ((null action)                ; try-completion
	   (if (stringp completion)
	       (concat first-part completion)
	     completion))
	  ((eq action t)                ; all-completions
	   completion)
	  (t                            ; exact match
	   completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
	   (try-completion string collection predicate))
	  ((eq action t)
	   (all-completions string collection predicate))
	  (t
	   (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (p4-follow-link-name (expand-file-name string)))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
	(progn
	  (setq dir-path (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((not action)
	   (setq completion (file-name-completion string dir-path))
	   (if (stringp completion)
	       (concat dir-path completion)
	     completion))
	  ((eq action t)
	   (file-name-all-completions string dir-path))
	  (t
	   (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	   (progn
	     (setq first-part (match-string 1 string))
	     (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
	      (setq completion nil))
	     (t
	      (setq completion
		    (,completion-function string predicate action))))
       (cond ((null action)             ; try-completion
	      (if (stringp completion)
		  (concat first-part completion)
		completion))
	     ((eq action t)             ; all-completions
	      completion)
	     (t                         ; exact match
	      completion)))))

(defalias 'p4-branch-string-completion (p4-string-completion-builder
					'p4-branches-completion))

(defalias 'p4-client-string-completion (p4-string-completion-builder
					'p4-clients-completion))

(defalias 'p4-job-string-completion (p4-string-completion-builder
				     'p4-jobs-completion))

(defalias 'p4-label-string-completion (p4-string-completion-builder
				       'p4-labels-completion))

(defalias 'p4-user-string-completion (p4-string-completion-builder
				      'p4-users-completion))

(defalias 'p4-group-string-completion (p4-string-completion-builder
				      'p4-groups-completion))


;;; Basic mode:

;; Major mode used for most P4 output buffers, and as the parent mode
;; for specialized modes below.

(define-derived-mode p4-basic-mode nil "P4 Basic")

(defvar p4-basic-mode-map
  (let ((map (make-sparse-keymap)))
    (if (featurep 'xemacs)
        (progn
          (define-key map [button2] 'p4-buffer-mouse-clicked)
          (define-key map [button3] 'p4-buffer-mouse-clicked-3))
      (define-key map [mouse-2] 'p4-buffer-mouse-clicked)
      (define-key map [mouse-3] 'p4-buffer-mouse-clicked-3))
    (define-key map "\t" 'p4-forward-active-link)
    (define-key map "\e\t" 'p4-backward-active-link)
    (define-key map [(shift tab)] 'p4-backward-active-link)
    (define-key map "\C-m" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "k"	 'p4-scroll-down-1-line)
    (define-key map "j"	 'p4-scroll-up-1-line)
    (define-key map "b"	 'p4-scroll-down-1-window)
    (define-key map "n"	 'next-line)
    (define-key map "p"	 'previous-line)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "	 'p4-scroll-up-1-window)
    (define-key map "<"	 'p4-top-of-buffer)
    (define-key map ">"	 'p4-bottom-of-buffer)
    (define-key map "="	 'delete-other-windows)
    map))

(defun p4-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (if (featurep 'xemacs)
        (progn
          (setq win (event-window event))
          (setq pnt (event-point event)))
      (setq win (posn-window (event-end event)))
      (setq pnt (posn-point (event-start event))))
    (select-window win)
    (goto-char pnt)
    (p4-buffer-commands pnt)))

(defun p4-buffer-mouse-clicked-3 (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (if (featurep 'xemacs)
        (progn
          (setq win (event-window event))
          (setq pnt (event-point event)))
      (setq win (posn-window (event-end event)))
      (setq pnt (posn-point (event-start event))))
    (select-window win)
    (goto-char pnt)
    (let ((link-name (or (get-char-property pnt 'link-client-name)
			 (get-char-property pnt 'link-depot-name)))
	  (rev (get-char-property pnt 'rev)))
      (cond (link-name
	     (p4-diff))
	    (rev
	     (p4-diff2 nil (format "%d" rev) "#head"))
	    (t
	     (error "No file to diff!"))))))

(defun p4-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(change (get-char-property pnt 'change))
	(action (get-char-property pnt 'action))
	(user (get-char-property pnt 'user))
	(group (get-char-property pnt 'group))
	(client (get-char-property pnt 'client))
	(label (get-char-property pnt 'label))
	(branch (get-char-property pnt 'branch))
	(filename (p4-buffer-file-name-2)))
    (cond ((and (not action) rev)
           (p4-call-command "print" (list (format "%s#%d" filename rev))
                            nil 'p4-activate-print-buffer))
	  (action
           (if (<= rev 1)
               (error "There is no earlier revision to diff.")
             (p4-diff2 nil (format "%d" (1- rev)) (format "%d" rev))))
	  (change (p4-describe-internal
		   (append (p4-make-list-from-string p4-default-diff-options)
			   (list (format "%d" change)))))
	  (user (p4-user user))
	  (group (p4-group group))
	  (client (p4-client client))
	  (label (p4-label (list label)))
	  (branch (p4-branch (list branch)))

	  ;; Check if a "filename link" or an active "diff buffer area" was
	  ;; selected.
	  (t
	   (let ((link-client-name (get-char-property pnt 'link-client-name))
		 (link-depot-name (get-char-property pnt 'link-depot-name))
		 (block-client-name (get-char-property pnt 'block-client-name))
		 (block-depot-name (get-char-property pnt 'block-depot-name))
		 (p4-history-for (get-char-property pnt 'history-for))
		 (first-line (get-char-property pnt 'first-line))
		 (start (get-char-property pnt 'start)))
	     (cond
	      (p4-history-for
	       (p4-file-change-log "filelog" (list p4-history-for)))
	      ((or link-client-name link-depot-name)
	       (p4-find-file-or-print-other-window
		link-client-name link-depot-name))
	      ((or block-client-name block-depot-name)
	       (if first-line
		   (let ((c (max 0 (- pnt
				      (save-excursion
					(goto-char pnt)
					(beginning-of-line)
					(point))
				      1)))
			 (r first-line))
		     (save-excursion
		       (goto-char start)
		       (while (re-search-forward "^[ +>].*\n" pnt t)
			 (setq r (1+ r))))
		     (p4-find-file-or-print-other-window
		      block-client-name block-depot-name)
		     (p4-goto-line r)
		     (if (not block-client-name)
			 (forward-line 1))
		     (beginning-of-line)
		     (goto-char (+ (point) c)))
		 (p4-find-file-or-print-other-window
		  block-client-name block-depot-name)))
	      (t
	       (error "There is no file at that cursor location!"))))))))

(defun p4-forward-active-link ()
  (interactive)
  (while (and (not (eobp))
	      (goto-char (next-overlay-change (point)))
	      (not (get-char-property (point) 'face)))))

(defun p4-backward-active-link ()
  (interactive)
  (while (and (not (bobp))
	      (goto-char (previous-overlay-change (point)))
	      (not (get-char-property (point) 'face)))))

(defun p4-quit-current-buffer ()
  "Quit a buffer"
  (interactive)
  (kill-buffer (current-buffer))
  (p4-pop-window-config))

(defun p4-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))


;;; Basic List Mode:

;; This is for the output of files, sync, have, integ, opened,
;; labelsync, and reconcile, which consists of a list of lines
;; starting with a depot filespec.

(define-derived-mode p4-basic-list-mode p4-basic-mode "P4 Basic List"
  (setq font-lock-defaults '(p4-basic-list-font-lock-keywords t)))

(defvar p4-basic-list-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\C-m" 'p4-basic-list-activate)
    map)
  "The keymap to use in P4 Basic List Mode.")

(defvar p4-basic-list-font-lock-keywords
  '(("^\\(//.*\\)#[0-9]+ - add" 1 'p4-depot-add-face)
    ("^\\(//.*\\)#[0-9]+ - branch" 1 'p4-depot-branch-face)
    ("^\\(//.*\\)#[0-9]+ - delete" 1 'p4-depot-delete-face)
    ("^\\(//.*\\)#[0-9]+ - edit" 1 'p4-depot-edit-face)))

(defun p4-basic-list-get-filename ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(//.*\\)#[0-9]+ - ")
      (match-string 1))))

(defun p4-basic-list-activate ()
  (interactive)
  (let ((file (p4-basic-list-get-filename)))
    (when file
      (p4-depot-find-file file))))


;;; Status List Mode:

;; This is for the output of p4 status, where each line starts with a
;; client filename.

(define-derived-mode p4-status-list-mode p4-basic-list-mode "P4 Status List"
  (setq font-lock-defaults '(p4-status-list-font-lock-keywords t)))

(defvar p4-status-list-mode-map
  (let ((map (p4-make-derived-map p4-basic-list-mode-map)))
    (define-key map "\C-m" 'p4-status-list-activate)
    map)
  "The key map to use in P4 Status List Mode.")

(defvar p4-status-list-font-lock-keywords
  '(("^\\(.*\\) - reconcile to add" 1 'p4-depot-add-face)
    ("^\\(.*\\) - reconcile to delete" 1 'p4-depot-delete-face)
    ("^\\(.*\\) - reconcile to edit" 1 'p4-depot-edit-face)))

(defun p4-status-list-activate ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(.*\\) - reconcile to ")
      (find-file-other-window (match-string 1)))))


;;; Form mode:

(define-derived-mode p4-form-mode indented-text-mode "P4 Form"
  "Major mode for P4 form derived from `indented-text-mode'"
  (setq fill-column 80
	indent-tabs-mode t
	font-lock-defaults '(p4-form-font-lock-keywords t)))

(defvar p4-form-font-lock-keywords
  '(("^#.*$" . 'p4-form-comment-face)
    ("^[^ :]+:" . 'p4-form-keyword-face)))

(defvar p4-form-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'p4-form-commit)
    map)
  "Keymap for P4 form mode.")


;;; Filelog mode:

(define-derived-mode p4-filelog-mode p4-basic-mode "P4 File Log")

(defvar p4-filelog-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "d"	 'p4-diff2)
    (define-key map "f"	 'p4-find-file-other-window)
    (define-key map "s"	 'p4-filelog-short-format)
    (define-key map "l"	 'p4-filelog-long-format)
    (define-key map "k"	 'p4-scroll-down-1-line-other-w)
    (define-key map "j"	 'p4-scroll-up-1-line-other-w)
    (define-key map "b"	 'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "	 'p4-scroll-up-1-window-other-w)
    (define-key map "<"	 'p4-top-of-buffer-other-w)
    (define-key map ">"	 'p4-bottom-of-buffer-other-w)
    (define-key map "="	 'p4-delete-other-windows)
    (define-key map "n"	 'p4-goto-next-change)
    (define-key map "p"	 'p4-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defun p4-find-file-other-window ()
  "Open/print file"
  (interactive)
  (let ((link-client-name (get-char-property (point) 'link-client-name))
	(link-depot-name (get-char-property (point) 'link-depot-name))
	(block-client-name (get-char-property (point) 'block-client-name))
	(block-depot-name (get-char-property (point) 'block-depot-name)))
    (cond ((or link-client-name link-depot-name)
	   (p4-find-file-or-print-other-window
	    link-client-name link-depot-name)
	   (other-window 1))
	  ((or block-client-name block-depot-name)
	   (p4-find-file-or-print-other-window
	    block-client-name block-depot-name)
	   (other-window 1)))))

(defun p4-filelog-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-filelog-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window-other-w ()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


;;; Diff mode:

(define-derived-mode p4-diff-mode p4-basic-mode "P4 Diff")

(defvar p4-diff-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n"	 'p4-goto-next-diff)
    (define-key map "p"	 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"	 'p4-next-depot-diff)
    (define-key map "u"	 'p4-prev-depot-diff)
    map))

(defun p4-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^====" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^====" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))


;;; Annotate mode:

(define-derived-mode p4-annotate-mode p4-basic-mode "P4 Annotate")

(defvar p4-annotate-mode-map
  (let ((map (p4-make-derived-map p4-basic-mode-map)))
    (define-key map "n"	 'p4-next-change-rev-line)
    (define-key map "p"	 'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "l"	 'p4-toggle-line-wrap)
    map)
  "The key map to use for browsing annotate buffers.")

(defun p4-moveto-print-rev-column (old-column)
  (let ((colon (save-excursion
		 (move-to-column 0)
		 (if (looking-at "[^:\n]*:")
		     (progn
		       (goto-char (match-end 0))
		       (current-column))
		   0))))
    (move-to-column old-column)
    (if (and (< (current-column) colon)
	     (re-search-forward "[^ ][ :]" nil t))
	(goto-char (match-beginning 0)))))

(defun p4-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (current-column)))
    (move-to-column 1)
    (re-search-forward "^ *[0-9]+ +[0-9]+[^:]+:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (move-to-column 32)
    (re-search-backward "^ *[0-9]+ +[0-9]+[^:]*:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-toggle-line-wrap ()
  "Toggle line wrap mode"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (save-window-excursion
    (recenter)))


;;; Hooks:

(add-hook 'find-file-hooks 'p4-update-status)

(provide 'p4)

;;; p4.el ends here
