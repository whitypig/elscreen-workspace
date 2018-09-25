# Usage
This elisp makes configurations of elscreen tabs persistent across
emacs sessions by saving window configurations to a file and reading
in those configurations from the file when emacs starts up.

It also provides workspace capability, with which you can manage as
many groups of elscreen tabs as you want and naming them and switching
between them easily. This would be easier with the help of helm
extension, so I recommend you use helm.

To enable this extension, load the file in the way whatever you like
and turn on elscreen-workspace-mode by `M-x elscreen-workspace-mode`, or
add the following line somewhere in your init file.
```elisp
(require 'elscreen-workspace)
(elscreen-workspace-mode 1)
```

## Save and restore workspaces manually
To save workspace configurations, do `M-x elscreen-workspace-save`.
This command writes all of workspace configurations in current emacs
session into file. To restore workspace configurations from file, do
`M-x elscreen-workspace-restore`.

## Using `elscreen-workspace.el` with `desktop.el` (to be edited)
`elscreen-workspace` can restore frames by itself, but you can also let
`desktop.el` restore frames. When `desktop.el` restores frames,
`elscreen-workspace.el` doesn't restore any frame. This case occurs when
`desktop.el` is enabled and variable `desktop-restore-frames` is set
`t`(default), but this will make little difference, so choose
whichever you like.

## Working with desktop using "desktop-globals-to-save" (to be edited)
You can use `desktop` like explained above (it is simpler and quicker
to setup). Or you can use it like explained here (a little bit more
settings needed.) to use it in conjunction with
e.g. [bookmark](http://www.emacswiki.org/emacs/BookmarkPlus#toc7) or
[desktop+](https://github.com/ffevotte/desktop-plus). So you don't
want to get two files on disk (the desktop-file and the elscreen-file)
and you want to save files anywhere on disc.

```elisp
(defcustom desktop-data-elscreen nil nil
  :type 'list
  :group 'desktop)

(defun desktop-prepare-data-elscreen! ()
  (setq desktop-data-elscreen
        (elscreen-workspace-get-data)))

(defun desktop-evaluate-data-elscreen! ()
  (when desktop-data-elscreen
    (elscreen-workspace-set-data desktop-data-elscreen)))

(add-hook 'desktop-after-read-hook 'desktop-evaluate-data-elscreen!)
(add-hook 'desktop-save-hook 'desktop-prepare-data-elscreen!)
(add-to-list 'desktop-globals-to-save 'desktop-data-elscreen)

(setq desktop-files-not-to-save "")
(setq desktop-restore-frames nil)
```

# Workspace capability
## Elscreen
Elscreen is great. But the number of screens you can create is limited
to 10 or so, and even if it weren't, I bet you couldn't handle all the
tabs in one frame with limited frame width. That's why we need
workspace capability.  With elscreen-workspace, you can create as many
workspaces as you want, and switch between them with real ease.

## Workspace
Workspace here means a group of screens. Each workspace has an index
starting from 0. Also, you can give each of them a name. These
information is saved to file (specified by `elscreen-workspace-file`),
so the next time you start emacs, all the workspaces are restored.

Imagine. Create a workspace and name it project1, open lots of
project1-related buffers there, then create another workspace, name it
project2, open lots of project2-related buffers, and so on.

Then suddenly emacs crashes. We would lose a lot. But, with
elscreen-workspace.el, this is not the case. All information about
your workspaces is saved to file, and if you are using some timer to
save workspaces every 30 minutes or so, you lose nothing, yes! happy
emacs life!

Also, the next time you start up our beloved emacs, workspaces are
restored. This means that you don't need to do find-file or even
recentf to open buffers because when you switch workspaces, the
buffers that were there will be restored.

## Some drawbacks
First, buffers to be restored are limited to some basic ones, so eww
buffers, for example, won't be restored. Dired buffers ARE resotred
with the modified versioin of `revive.el` in my repository. If you are
insterested, give it a try.

Secondly, if you try to switch for the first time to a workspace in
which there are lots of buffers, switching to that workspace takes a
bit long time. This is because emacs is trying to open all of buffers
in that workspace. Just close your eyes, breathe deeply 3 times and
you'll be happy.

If `elscreen-display-screen-number` defined in `elscreen.el` is set to
`t` (default), then workspace number, or name if available, is also
displayed in mode line.

## Commands
### `M-x elscreen-workspace-open-workspace`
Create and move to a new workspace. Default name for new workspaces is
"" (empty string).

### `M-x elscreen-workspace-goto-next-workspace`
Move to the next workspace.

### `M-x elscreen-workspace-goto-previous-workspace`
Move to the previous workspace.

### `M-x elscreen-workspace-kill-workspace`
Kill current workspace. Buffers in this workspace won't be killed.

### `M-x elscreen-workspace-switch-workspace-through-helm`
Helm interface to move around workspaces. If helm isn't installed in
your system, give it a shot. It's worth a try.

### `M-x elscreen-workspace-name-workspace`
Name the current workspace.

## Recommended settings
```elisp
(define-key elscreen-map (kbd "C-o") #'elscreen-workspace-open-workspace)
(define-key elscreen-map (kbd "C-f") #'elscreen-workspace-goto-next-workspace)
(define-key elscreen-map (kbd "C-b") #'elscreen-workspace-goto-previous-workspace)
(define-key elscreen-map (kbd "C-d") #'elscreen-workspace-kill-workspace)
(define-key elscreen-map (kbd "C-z") #'elscreen-workspace-switch-workspace-through-helm))

;; Save workspace state every 30 mins
(setq my-30min-elscreen-workspace-timer (run-with-timer 1800 1800 #'elscreen-workspace-store))

;; To cancel the timer above, evaluate the following.
;; (cancel-timer my-30min-elscreen-workspace-timer)

```

Any ideas and issue reports will be appreciated.
