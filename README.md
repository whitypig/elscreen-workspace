usage
=====

This makes elscreen persistent.

To use this, use customize to turn on `elscreen-persist-mode`
or add the following line somewhere in your init file:

    (elscreen-persist-mode 1)

Or manually, use `elscreen-persist-store` to store,
and use `elscreen-persist-restore` to restore.

work with desktop
=================

You can use `desktop` to restore frames.

When `desktop` restored frames, `elscreen-persist` doesn't restore any frame.
The behavior occurs when `desktop` is enabled and `desktop-restore-frames` is `t`(default).

`elscreen-persist` restores all buffers, so `desktop` doesn't have to save the buffers.

    (setq desktop-files-not-to-save "")

work with desktop using "desktop-globals-to-save"
=================================================

You can use `desktop` like explained above (it is more simple and quick to setup). Or you
can use it like explained here (a little bit more setup) to use it in conjunction with
e.g. [bookmark](http://www.emacswiki.org/emacs/BookmarkPlus#toc7) or
[desktop+](https://github.com/ffevotte/desktop-plus). So you dont want to get two files on
disk (the desktop-file and the elscreen-file) and you want to save files anywhere on disc.

```elisp
(defcustom desktop-data-elscreen nil nil
  :type 'list
  :group 'desktop)

(defun desktop-prepare-data-elscreen! ()
  (setq desktop-data-elscreen
        (elscreen-persist-get-data)))

(defun desktop-evaluate-data-elscreen! ()
  (when desktop-data-elscreen
    (elscreen-persist-set-data desktop-data-elscreen)))

(add-hook 'desktop-after-read-hook 'desktop-evaluate-data-elscreen!)
(add-hook 'desktop-save-hook 'desktop-prepare-data-elscreen!)
(add-to-list 'desktop-globals-to-save 'desktop-data-elscreen)

(setq desktop-files-not-to-save "")
(setq desktop-restore-frames nil)
```

# Workspace capability
## Elscreen
Elscreen is great. But the number of screens you can create is limited
to 10 or so, and even if it weren't, I couldn't handle all the tabs in
one frame with limited width. With this forked version of
elscreen-persist, you can create as many workspaces as you want, and
switch between them with real ease.

## Workspace
Workspace here means a group of screens. Each workspace has an index
starting from 0. Also, you can give each of them a name. These
information is saved to file (`elscreen-persist-file`), so the next
time you start emacs, all the workspaces are restored.

Imagine. Create a workspace and name it project1, open lots of
project1 buffers there, then create another workspace, name it
project2, open lots of project2 buffers, and so on. All these
information is saved to file, so you won't need to open all these
buffers the next time you start emacs... unless something not good
happens :)

Restoring buffers is limited to some basic buffers, so w3m buffers,
for example, won't be restored. Dired buffers ARE resotred with the
modified versioin of `revive.el` in my repository. If you are
insterested, give it a try.

## Some drawbacks
If you try to switch for the first time to a workspace in which there
are lots of buffers, especially lots of large org-mode buffers,
switching to that workspace takes a bit long time. Just close your
eyes, breathe deeply 3 times and you'll be happy.

If `elscreen-display-screen-number` defined in `elscreen.el` is set to
`t` (default), then workspace number, or name if available, is also
displayed in mode line.

## Commands
### `#'elscreen-persist-open-workspace`
Create and move to a new workspace. Default name for new workspaces is
"" (empty).

### `#'elscreen-persist-goto-next-workspace`
Move to the next workspace.

### `#'elscreen-persist-goto-previous-workspace`
Move to the previous workspace.

### `#'elscreen-persist-kill-workspace`
Kill current workspace. Buffers in there won't be killed.

### `#'elscreen-persist-switch-workspace-through-helm`
Helm interface to move around workspaces. If helm isn't installed in
your system, give it a shot. It's worth a try.

### `#'elscreen-persist-name-workspace`
Name the current workspace.

### Recommended settings
```elisp
(define-key elscreen-map (kbd "C-o") #'elscreen-persist-open-workspace)
(define-key elscreen-map (kbd "C-f") #'elscreen-persist-goto-next-workspace)
(define-key elscreen-map (kbd "C-b") #'elscreen-persist-goto-previous-workspace)
(define-key elscreen-map (kbd "C-d") #'elscreen-persist-kill-workspace)
(define-key elscreen-map (kbd "C-z") #'elscreen-persist-switch-workspace-through-helm))

;; Save state every 30 mins
(setq my-30min-elscreen-persist-timer (run-with-timer 1800 1800 #'elscreen-persist-store))

```

Any ideas and issue reports will be appreciated.
