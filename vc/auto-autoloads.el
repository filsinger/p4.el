;;; DO NOT MODIFY THIS FILE
(if (featurep 'vc-autoloads) (error "Already loaded"))

;;;### (autoloads (vc-menu-filter vc-kill-buffer-hook vc-file-not-found-hook vc-find-file-hook vc-follow-link vc-status vc-mode-line vc-after-save vc-toggle-read-only vc-buffer-backend vc-find-cvs-master vc-registered vc-workfile-version vc-fetch-properties vc-master-workfile-version vc-your-latest-version vc-latest-version vc-locking-user vc-rcs-lock-from-diff vc-file-owner vc-user-login-name vc-lock-from-permissions vc-master-locking-user vc-master-locks vc-cvs-status vc-checkout-model vc-backend vc-name vc-backend-subdirectory-name vc-consult-rcs-headers vc-fetch-master-properties vc-simple-command vc-parse-locks vc-insert-file vc-parse-buffer vc-lock-file vc-match-substring vc-file-clearprops vc-file-getprop vc-file-setprop vc-error-occurred vc-mistrust-permissions) "vc-hooks" "vc/vc-hooks.el")

(defvar vc-default-back-end nil "\
*Back-end actually used by this interface; may be SCCS or RCS.
The value is only computed when needed to avoid an expensive search.")

(defvar vc-handle-cvs t "\
*If non-nil, use VC for files managed with CVS.
If it is nil, don't use VC for those files.")

(defvar vc-rcsdiff-knows-brief nil "\
*Indicates whether rcsdiff understands the --brief option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use --brief and sets this variable to remember whether it worked.")

(defvar vc-path (if (file-directory-p "/usr/sccs") '("/usr/sccs") nil) "\
*List of extra directories to search for version control commands.")

(defvar vc-master-templates '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS) ("%sSCCS/s.%s" . SCCS) ("%ss.%s" . SCCS) vc-find-cvs-master) "\
*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defvar vc-make-backup-files nil "\
*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups.")

(defvar vc-follow-symlinks 'ask "\
*Indicates what to do if you visit a symbolic link to a file
that is under version control.  Editing such a file through the
link bypasses the version control system, which is dangerous and
probably not what you want.  
  If this variable is t, VC follows the link and visits the real file,
telling you about it in the echo area.  If it is `ask', VC asks for
confirmation whether it should follow the link.  If nil, the link is
visited and a warning displayed.")

(defvar vc-display-status t "\
*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed.")

(defvar vc-consult-headers t "\
*If non-nil, identify work files by searching for version headers.")

(defvar vc-keep-workfiles t "\
*If non-nil, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag.")

(defvar vc-mistrust-permissions nil "\
*If non-nil, don't assume that permissions and ownership track 
version-control status.  If nil, do rely on the permissions.
See also variable `vc-consult-headers'.")

(autoload 'vc-mistrust-permissions "vc-hooks" nil nil nil)

(add-minor-mode 'vc-mode 'vc-mode)

(defvar vc-mode nil)

(make-variable-buffer-local 'vc-mode)

(put 'vc-mode 'permanent-local t)

(autoload 'vc-error-occurred "vc-hooks" nil nil 'macro)

(defvar vc-file-prop-obarray [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] "\
Obarray for per-file properties.")

(defvar vc-buffer-backend t)

(autoload 'vc-file-setprop "vc-hooks" nil nil nil)

(autoload 'vc-file-getprop "vc-hooks" nil nil nil)

(autoload 'vc-file-clearprops "vc-hooks" nil nil nil)

(autoload 'vc-match-substring "vc-hooks" nil nil nil)

(autoload 'vc-lock-file "vc-hooks" nil nil nil)

(autoload 'vc-parse-buffer "vc-hooks" nil nil nil)

(autoload 'vc-insert-file "vc-hooks" nil nil nil)

(autoload 'vc-parse-locks "vc-hooks" nil nil nil)

(autoload 'vc-simple-command "vc-hooks" nil nil nil)

(autoload 'vc-fetch-master-properties "vc-hooks" nil nil nil)

(autoload 'vc-consult-rcs-headers "vc-hooks" nil nil nil)

(autoload 'vc-backend-subdirectory-name "vc-hooks" nil nil nil)

(autoload 'vc-name "vc-hooks" "\
Return the master name of a file, nil if it is not registered.
For CVS, the full name of CVS/Entries is returned." nil nil)

(autoload 'vc-backend "vc-hooks" "\
Return the version-control type of a file, nil if it is not registered." nil nil)

(autoload 'vc-checkout-model "vc-hooks" nil nil nil)

(autoload 'vc-cvs-status "vc-hooks" nil nil nil)

(autoload 'vc-master-locks "vc-hooks" nil nil nil)

(autoload 'vc-master-locking-user "vc-hooks" nil nil nil)

(autoload 'vc-lock-from-permissions "vc-hooks" nil nil nil)

(autoload 'vc-user-login-name "vc-hooks" nil nil nil)

(autoload 'vc-file-owner "vc-hooks" nil nil nil)

(autoload 'vc-rcs-lock-from-diff "vc-hooks" nil nil nil)

(autoload 'vc-locking-user "vc-hooks" nil nil nil)

(autoload 'vc-latest-version "vc-hooks" nil nil nil)

(autoload 'vc-your-latest-version "vc-hooks" nil nil nil)

(autoload 'vc-master-workfile-version "vc-hooks" nil nil nil)

(autoload 'vc-fetch-properties "vc-hooks" nil nil nil)

(autoload 'vc-workfile-version "vc-hooks" nil nil nil)

(autoload 'vc-registered "vc-hooks" nil nil nil)

(autoload 'vc-find-cvs-master "vc-hooks" nil nil nil)

(autoload 'vc-buffer-backend "vc-hooks" "\
Return the version-control type of the visited file, or nil if none." nil nil)

(autoload 'vc-toggle-read-only "vc-hooks" "\
Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.  With prefix argument, ask for version number." t nil)

(define-key global-map "" 'vc-toggle-read-only)

(autoload 'vc-after-save "vc-hooks" nil nil nil)

(autoload 'vc-mode-line "vc-hooks" "\
Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name." t nil)

(autoload 'vc-status "vc-hooks" nil nil nil)

(autoload 'vc-follow-link "vc-hooks" nil nil nil)

(autoload 'vc-find-file-hook "vc-hooks" nil nil nil)

(add-hook 'find-file-hooks 'vc-find-file-hook)

(autoload 'vc-file-not-found-hook "vc-hooks" "\
When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise." nil nil)

(add-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

(autoload 'vc-kill-buffer-hook "vc-hooks" nil nil nil)

(setq vc-prefix-map (lookup-key global-map "v"))

(if (not (keymapp vc-prefix-map)) (progn (setq vc-prefix-map (make-sparse-keymap)) (define-key global-map "v" vc-prefix-map) (define-key vc-prefix-map "a" 'vc-update-change-log) (define-key vc-prefix-map "c" 'vc-cancel-version) (define-key vc-prefix-map "d" 'vc-directory) (define-key vc-prefix-map "h" 'vc-insert-headers) (define-key vc-prefix-map "i" 'vc-register) (define-key vc-prefix-map "l" 'vc-print-log) (define-key vc-prefix-map "r" 'vc-retrieve-snapshot) (define-key vc-prefix-map "s" 'vc-create-snapshot) (define-key vc-prefix-map "u" 'vc-revert-buffer) (define-key vc-prefix-map "v" 'vc-next-action) (define-key vc-prefix-map "=" 'vc-diff) (define-key vc-prefix-map "~" 'vc-version-other-window)))

(defconst vc-menu '("VC" :filter vc-menu-filter ["" vc-next-action buffer-file-name nil] "----" ["Revert to Last Revision" vc-revert-buffer vc-mode nil] ["Cancel Last Checkin" vc-cancel-version vc-mode] ["Rename File" vc-rename-this-file vc-mode nil] "----" ["Diff Against Last Version" vc-diff vc-mode] ["Diff Between Revisions..." vc-version-diff vc-mode] ["Visit Other Version..." vc-version-other-window vc-mode] ["Show Edit History" vc-print-log vc-mode] "----" ["List Locked Files Any User" vc-directory t] "----" ["Create Snapshot" vc-create-snapshot t] ["Retrieve Snapshot" vc-retrieve-snapshot t] "----" ["CVS Update Directory" cvs-update t]) "\
Menubar entry for using the revision control system.")

(autoload 'vc-menu-filter "vc-hooks" nil nil nil)

(and (featurep 'menubar) current-menubar (car (find-menu-item current-menubar '("Tools"))) (add-submenu '("Tools") vc-menu "Compare") (add-menu-button '("Tools") "---" "Compare"))

;;;***

;;;### (autoloads (vc-update-change-log vc-rename-file vc-cancel-version vc-revert-buffer vc-print-log vc-retrieve-snapshot vc-create-snapshot vc-directory vc-insert-headers vc-version-other-window vc-version-diff vc-diff vc-checkout vc-register vc-next-action vc-find-binary) "vc" "vc/vc.el")

(defvar vc-before-checkin-hook nil "\
*Normal hook (list of functions) run before a file gets checked in.  
See `run-hooks'.")

(defvar vc-checkin-hook nil "\
*Normal hook (List of functions) run after a checkin is done.
See `run-hooks'.")

(autoload 'vc-find-binary "vc" "\
Look for a command anywhere on the subprocess-command search path." nil nil)

(autoload 'vc-next-action "vc" "\
Do the next logical checkin or checkout operation on the current file.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.
   A prefix argument lets you specify the version number to use.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control and then retrieves a writable, locked copy for editing.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy." t nil)

(autoload 'vc-register "vc" "\
Register the current file into your version-control system.
The default initial version number, taken to be `vc-default-init-version',
can be overridden by giving a prefix arg." t nil)

(autoload 'vc-checkout "vc" "\
Retrieve a copy of the latest version of the given file." nil nil)

(autoload 'vc-diff "vc" "\
Display diffs between file versions.
Normally this compares the current file and buffer with the most recent 
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare." t nil)

(autoload 'vc-version-diff "vc" "\
For FILE, report diffs between two stored versions REL1 and REL2 of it.
If FILE is a directory, generate diffs between versions for all registered
files in or below it." t nil)

(autoload 'vc-version-other-window "vc" "\
Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created." t nil)

(autoload 'vc-insert-headers "vc" "\
Insert headers in a file for use with your version-control system.
Headers desired are inserted at the start of the buffer, and are pulled from
the variable `vc-header-alist'." t nil)

(autoload 'vc-directory "vc" "\
Show version-control status of the current directory and subdirectories.
Normally it creates a Dired buffer that lists only the locked files
in all these directories.  With a prefix argument, it lists all files." t nil)

(autoload 'vc-create-snapshot "vc" "\
Make a snapshot called NAME.
The snapshot is made from all registered files at or below the current
directory.  For each file, the version level of its latest
version becomes part of the named configuration." t nil)

(autoload 'vc-retrieve-snapshot "vc" "\
Retrieve the snapshot called NAME.
This function fails if any files are locked at or below the current directory
Otherwise, all registered files are checked out (unlocked) at their version
levels in the snapshot." t nil)

(autoload 'vc-print-log "vc" "\
List the change log of the current buffer in a window." t nil)

(autoload 'vc-revert-buffer "vc" "\
Revert the current buffer's file back to the latest checked-in version.
This asks for confirmation if the buffer contents are not identical
to that version.
If the back-end is CVS, this will give you the most recent revision of
the file on the branch you are editing." t nil)

(autoload 'vc-cancel-version "vc" "\
Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards." t nil)

(autoload 'vc-rename-file "vc" "\
Rename file OLD to NEW, and rename its master file likewise." t nil)

(autoload 'vc-update-change-log "vc" "\
Find change log file and add entries from recent RCS/CVS logs.
Normally, find log entries for all registered files in the default
directory using `rcs2log', which finds CVS logs preferentially.
The mark is left at the end of the text prepended to the change log.

With prefix arg of C-u, only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any arguments are assumed to be filenames and are
passed to the `rcs2log' script after massaging to be relative to the
default directory." t nil)

;;;***

(provide 'vc-autoloads)
