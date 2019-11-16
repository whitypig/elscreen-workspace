;;; elscreen-workspace.el --- Switching back and forth a group of elscreen tabs and retaining them across emacs sessions.
;; Copyright (C) 2014 Hironori Yoshida
;;               2018, 2019 whitypig <whitypig@gmail.com>
;; Authors: Hironori Yoshida <webmaster@robario.com>
;;          whitypig <whitypig@gmail.com>
;; Keywords: elscreen frames
;; Version: 0.1
;; Package-Requires: ((elscreen "1.4.6") (revive "2.19") (eieio) (helm))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'elscreen)
(require 'revive)
(require 'eieio)

(defcustom elscreen-workspace-file (locate-user-emacs-file "elscreen-workspace")
  "The file where the elscreen configuration is stored."
  :type 'file
  :group 'elscreen)

;; should we change the group of the following customs to 'elscreen-workspace?
(defcustom elscreen-workspace-default-buffer "*scratch*"
  "Default buffer to be visited when a new workspace is created."
  :type 'string
  :group 'elscreen)

(defcustom elscreen-workspace-workspace-name-limit 10
  "The upper limit of workspace name. If the length of a
  workspace name is more than this value, only the first
  `elscreen-workspace-workspace-name-limit' number of characters
  are displayed."
  :type 'integer
  :group 'elscreen)

(defcustom elscreen-workspace-helm-buffer-separator " + "
  "Used as a separator of buffers in one screen when displaying
  helm candidates"
  :type 'string
  :group 'elscreen)

(defcustom elscreen-workspace-helm-screen-separator " | "
  "Used as a seperator of screens in workspace when displaying
  helm candidates."
  :type 'string
  :group 'elscreen)

(defcustom elscreen-workspace-number-of-backup-generations 5
  "The number of generations for backup files."
  :type 'integer
  :group 'elscreen)

;;; Variables:
(defvar elscreen-workspace--workspaces nil
  "A list of workspaces. Each workspace contains one or more screens.")

(defvar elscreen-workspace--workspace-names '("")
  "A list of workspace names")

(defvar elscreen-workspace--current-index 0
  "Index in `elscreen-workspace--workspaces'")

(defvar elscreen-workspace-helm-buffer-name "*helm elscreen workspaces*")

(defclass elscreen-workspace-ws ()
  ((name
    :initarg :name :initform ""
    :type string
    :documentation
    "The name of this workspace")

   (frame-parameters
    :initarg :frame-parameters :initform (elscreen-workspace-get-frame-parameters)
    :type list
    :documentation
    "Frame parameters")

   (current-screen-number
    :initarg :current-screen-number :initform (elscreen-get-current-screen)
    :type number
    :documentation
    "Active screen number in this workspace.")

   (screen-configurations
    :initarg :screen-configurations :initform (elscreen-workspace-get-screens)
    :type list
    :documentation
    ;; screen-number height width list-of-window-positions list-of-buffers-in-this-screen
    ;; (
    ;;   (3 95 51 ((0 0 98 50)) ((nil "*info*<2>" 2644 1300)))
    ;; ...
    ;; )
    "List of screen and window configurations.

This list is sorted by increasing order of screen number.Each element
in this list is also a list that represents one screen and its window
configuration in the form of (screen-number window-configuration).")

   (nicknames
    :initarg :nicknames :initform (elscreen-workspace-get-nicknames)
    :type list
    :documentation
    "List of nicknames for screens. The first element is a name for screen 0, the second one is for screen 1, and so on.")

   (theme
    :initarg :theme :initform nil
    :type list
    :documentation
    "Theme which is applied to this workspace. Not implemented though."))
  "A class which represents elscreen workspace.")

(defmethod elscreen-workspace--update ((ws elscreen-workspace-ws))
  "Update workspace by actually collecting information on the current configuration."
  (setf (slot-value ws :name) (elscreen-workspace-get-workspace-name))
  (setf (slot-value ws :frame-parameters) (elscreen-workspace-get-frame-parameters))
  (setf (slot-value ws :current-screen-number) (elscreen-get-current-screen))
  (setf (slot-value ws :screen-configurations) (elscreen-workspace-get-screens))
  (setf (slot-value ws :nicknames) (elscreen-workspace-get-nicknames))
  (setf (slot-value ws :theme) (elscreen-workspace-get-theme))
  ws)

(defmethod elscreen-workspace--get-name ((ws elscreen-workspace-ws))
  (slot-value ws :name))

(defmethod elscreen-workspace--get-frame-parameters ((ws elscreen-workspace-ws))
  (slot-value ws :frame-parameters))

(defmethod elscreen-workspace--get-screen-configurations ((ws elscreen-workspace-ws))
  (slot-value ws :screen-configurations))

(defmethod elscreen-workspace--get-current-screen ((ws elscreen-workspace-ws))
  (slot-value ws :current-screen-number))

(defmethod elscreen-workspace--get-nicknames ((ws elscreen-workspace-ws))
  (slot-value ws :nicknames))

(defmethod elscreen-workspace--set-name ((ws elscreen-workspace-ws) name)
  (setf (slot-value ws :name) name))

(defmethod elscreen-workspace--get-buffers ((ws elscreen-workspace-ws))
  "Return a list of buffers that are in workspace WS."
  (mapcar (lambda (elt)
            ;; buffer info is 4th element in screen-configurations slot.
            (nth 4 elt))
          (elscreen-workspace--get-screen-configurations ws)))

(defmethod elscreen-workspace--get-theme ((ws elscreen-workspace-ws))
  (slot-value ws 'theme))

;;;###autoload
(defun elscreen-workspace-get-frame-parameters ()
  "Get and return serializable frame parameters."
  (let ((frame-parameters (frame-parameters)))
    ;; Delete some unserializable frame parameter.
    (dolist (key '(buffer-list buried-buffer-list minibuffer))
      (delq (assq key frame-parameters) frame-parameters))
    frame-parameters))

;;;###autoload
(defun elscreen-workspace-get-theme ()
  nil)

(defun elscreen-workspace-get-screens ()
  "Return a list of window configurations for all screens in current workspace."
  (let ((current-screen (elscreen-get-current-screen)))
    (prog1 (mapcar (lambda (screen-number)
                       (elscreen-goto screen-number)
                       (cons screen-number
                             (current-window-configuration-printable)))
                   (sort (elscreen-get-screen-list) #'<))
      ;; Go back to current screen.
      (elscreen-goto current-screen))))

;;;###autoload
(defun elscreen-workspace-get-nicknames ()
  "Return a list of nicknames for screens in current workspace."
  (mapcar (lambda (screen-number)
            (elscreen-get-screen-nickname screen-number))
          (sort (elscreen-get-screen-list) #'<)))

(defun elscreen-workspace-update-current-workspace ()
  "Update and return current workspace."
  (if (null elscreen-workspace--workspaces)
      ;; If there is no workspace, create a new one.
      (setq elscreen-workspace--current-index 0
            elscreen-workspace--workspaces (list (elscreen-workspace-ws)))
    (elscreen-workspace--update
     (nth elscreen-workspace--current-index elscreen-workspace--workspaces))))

(defun elscreen-workspace-get-current-workspace ()
  (cond
   ((null elscreen-workspace--workspaces)
    ;; There is no saved workspace in elscreen-workspace--workspaces,
    ;; so create a new one.
    (setq elscreen-workspace--workspaces (list (elscreen-workspace-ws))
          elscreen-workspace--current-index 0)
    (car elscreen-workspace--workspaces))
   (t
    (nth elscreen-workspace--current-index elscreen-workspace--workspaces))))

(defun elscreen-workspace-get-current-workspace-name ()
  "Return the name of current workspace."
  (elscreen-workspace--get-name (elscreen-workspace-get-current-workspace)))

(defun elscreen-workspace-save (&optional no-message)
  "Save workspaces into file specified by `elscreen-workspace-file'."
  (interactive)
  (when (file-exists-p elscreen-workspace-file)
      ;; Shift backup file
    (elscreen-workspace--shift-backup-files))
  ;; First, update current workspace data.
  (elscreen-workspace-update-current-workspace)
  ;; Then, store the configurations into the file.
  (with-temp-file elscreen-workspace-file
    (let ((print-length nil)
          (print-level nil))
      ;; save workspace list
      ;; for now, save only workspace list
      (insert ";; Auto-generated file. Do not edit this file unless you know what you are doing.\n")
      (insert (format-time-string ";; Created at: %Y-%m-%d %H:%M:%S\n" (current-time)))
      (pp elscreen-workspace--workspaces (current-buffer))))
  (unless no-message
    (message (format "Saved current workspaces to %s" elscreen-workspace-file))))

(defun elscreen-workspace--shift-backup-files ()
  "Copy backup files.

Let's say we have file backup files, elws.bak1, elws.bak2, elws.bak3,
elws.bak4, elws.bak5.  First, copy elws.bak4 to elws.bak5, then
elws.bak3 to elws.bak4, and so on.  Finally, copy the newest one, i.e,
`elscreen-workspace-file' to elws.bak1.
Note that the number of backup-file generations can be configured by
`elscreen-workspace-number-of-backup-generations'."
  (cl-loop for i from (1- elscreen-workspace-number-of-backup-generations) downto 1
           for from-name = (format "%s.bak%d" elscreen-workspace-file i)
           for to-name = (format "%s.bak%d" elscreen-workspace-file (1+ i))
           when (file-exists-p from-name)
           do (copy-file from-name to-name t)
           finally (copy-file elscreen-workspace-file
                              (format "%s.bak1" elscreen-workspace-file)
                              t)))

;;;###autoload
(defun elscreen-workspace-restore-frame-parameters (fparams)
  "Restore frame parameters FPARAMS if necessary."
  (unless (and (boundp 'desktop-restore-frames)
               desktop-restore-frames
               (fboundp 'desktop-full-lock-name)
               (file-exists-p (desktop-full-lock-name)))
    (modify-frame-parameters nil fparams)
    (message "The frame has been restored by `elscreen-workspace'. Using `desktop' is recommended.")))

;;;###autoload
(defun elscreen-workspace-restore-screens (workspace)
  "Restore screens and window configurations saved in WORKSPACE."
  ;; Note:
  ;; #'elscreen-kill restores window-configuration with its own
  ;; #'elscreen-apply-window-configuration, so let them first play a
  ;; role. After that, we do our own job. Otherwise, restored window
  ;; configuration will be modified again by elscreen.
  ;; First, create enough number of screens
  ;; (message "DEBUG: creating screens")
  (let* ((screen-numbers (mapcar #'car (slot-value workspace :screen-configurations)))
         (largest (apply #'max screen-numbers)))
    ;; First, create enough number of screens.
    (cl-loop for i from 0 to largest
             do (unless (elscreen-screen-live-p i)
                  (elscreen-create)))
    ;; Then, kill unnecessary screens if any. This case happens when
    ;; stored screen numbers are something like '(0 2 3 4), in which
    ;; case we have to kill screen 1, or when existing screens are '(0
    ;; 1 2 3 4 5), but screens in WORKSPACE are '(0 1 2 3), in which
    ;; case we have to kill screen 4 and 5.
    (cl-loop for i in (elscreen-get-screen-list)
             unless (member i screen-numbers)
             do (elscreen-kill-internal i))
    ;; Finally, restore window configurtion
    (cl-loop for conf in (slot-value workspace :screen-configurations)
             for screen-ix = (car conf)
             for win-conf = (cdr conf)
             do (unless (window-minibuffer-p)
                  (elscreen-goto screen-ix)
                  (restore-window-configuration win-conf)))
    ;; Go to the screen in which we had been when we left this worksspace.
    (elscreen-goto (slot-value workspace :current-screen-number))
    (elscreen-notify-screen-modification 'force-immediately)))

;;;###autoload
(defun elscreen-workspace-restore-nicknames (workspace)
  "Restore nicknames of screens."
  (cl-loop for ix in (sort (elscreen-get-screen-list) #'<)
           for name in (elscreen-workspace--get-nicknames workspace)
           when name
           do (elscreen-set-screen-nickname ix name)))

;; ;;;###autoload
;; (defun elscreen-workspace-restore-workspace (workspace)
;;   "Set the frame parameters, screens, window configurations and nicknames."
;;   (elscreen-workspace-set-frame-parameters (elscreen-workspace--get-frame-parameters workspace))
;;   (elscreen-workspace-set-screens (elscreen-workspace--get-screen-configurations workspace))
;;   (elscreen-workspace-set-nicknames (elscreen-workspace--get-nicknames workspace)))

;;;###autoload
(defun elscreen-workspace-restore-workspace (workspace)
  "Set the frame parameters, screens, window configurations and nicknames."
  (elscreen-workspace-restore-frame-parameters
   (elscreen-workspace--get-frame-parameters workspace))
  (elscreen-workspace-restore-screens workspace)
  (elscreen-workspace-restore-nicknames workspace))

;;;###autoload
(defun elscreen-workspace-restore ()
  "Read saved information from `elscreen-workspace-file' and
restore workspaces."
  (interactive)
  (when (file-exists-p elscreen-workspace-file)
    ;; Should we check if the size of the file is greater than 0?
    (let* ((s (with-temp-buffer
                (insert-file-contents elscreen-workspace-file)
                (buffer-string)))
           (version (cond
                     ((string-match-p "^(\\[eieio-class-tag--elscreen-workspace-ws.*" s)
                        ;; file saved on emacs whose version is <= 25
                      25)
                     ((string-match-p "^(#s(elscreen-workspace-ws.*" s)
                      26)
                     (t
                      nil)))
           (lst nil))
      (cond
       ((or (null version) (not (numberp version)))
        ;; Invalid format
        nil)
       ((and (= emacs-major-version 26) (numberp version) (= version 25))
        ;; Trying to read version 25 file on emacs 26.
        ;; Replace eieio part with #s sth.
        (setq lst
              (read
               (replace-regexp-in-string
                "\\[eieio-class-tag--elscreen-workspace-ws\\([^]]+\\)\\]"
                "#s(elscreen-workspace-ws\\1)"
                s))))
       ((and (= emacs-major-version 25) (= version 26))
        ;; Trying to read version 26 file on emacs 25, not likely though.
        (setq lst
              (read
               (replace-regexp-in-string
                "#s(elscreen-workspace-ws\\(.*\\))"
                "[eieio-class-tag--elscreen-workspace-ws\\1]"
                s))))
       (t
        (setq lst (read s))))
      (cond
       ((and (listp lst)
             (not (null lst))
             (cl-every #'elscreen-workspace-ws-p lst))
        ;; (message "DEBUG: reading in workspace file done")
        ;; If read obj is a non-nil list and each elemnt of it is of
        ;; type elscreen-workspace-ws, then wo go.
        (setq elscreen-workspace--workspaces lst)
        ;; for now, use 0 as default index
        (setq elscreen-workspace--current-index 0)
        ;; then, restore workspace
        (elscreen-workspace-restore-workspace (car elscreen-workspace--workspaces)))
       (t
        ;; Invalid object having been read.
        (message "%s is Invalid format" elscreen-workspace-file))))))

(defun elscreen-workspace--restore-from-file (path)
  ;; for debug
  (interactive "fFile: ")
  (let ((elscreen-workspace-file path))
    (elscreen-workspace-restore)))

(defun elscreen-workspace-workspace-single-p ()
  (= 1 (length elscreen-workspace--workspaces)))

(defun elscreen-workspace-update-workspace-index (delta)
  (setq elscreen-workspace--current-index
        (% (+ (+ delta elscreen-workspace--current-index)
              (length elscreen-workspace--workspaces))
           (length elscreen-workspace--workspaces))))

(defun elscreen-workspace-switch-workspace-by-delta (delta)
  "Switch workspace to another one which is DELTA distance away."
  (cl-assert (not (zerop delta)) t "switching workspace, delta cannot be zero")
  ;; (message "DEBUG: goto %s workspace, current-buffer=%s"
  ;;          (if (> delta 0) "next" "previsou") (buffer-name))
  ;; save current workspace info into memory
  (elscreen-workspace-update-current-workspace)
  ;; update index
  (elscreen-workspace-update-workspace-index delta)
  (elscreen-workspace-restore-workspace (elscreen-workspace-get-current-workspace))
  (elscreen-workspace-show-workspace-info)
  (redisplay)
  (elscreen-notify-screen-modification 'force-immediately))

(defun elscreen-workspace-goto-next-workspace ()
  "Switch to the next workspace."
  (interactive);
  ;; (message "DEBUG: goto next ws from index=%d" elscreen-workspace--current-index)
  (if (elscreen-workspace-workspace-single-p)
      (elscreen-message "You should have at least two workspaces to move around!")
    (elscreen-workspace-switch-workspace-by-delta 1))
  ;; (message "DEBUG: current workspace, index=%d" elscreen-workspace--current-index)
  )

(defun elscreen-workspace-goto-previous-workspace ()
  "Switch to the previous workspace"
  (interactive)
  ;; (message "DEBUG: goto previous ws from index=%d" elscreen-workspace--current-index)
  (if (elscreen-workspace-workspace-single-p)
      (elscreen-message "You should have at least two workspaces to move around!")
    (elscreen-workspace-switch-workspace-by-delta -1))
  ;; (message "DEBUG: current workspace, index=%d" elscreen-workspace--current-index)
  )

(defun elscreen-workspace-kill-all-tabs ()
  "Kill all the screens"
  (cl-loop repeat (1- (elscreen-get-number-of-screens))
           ;; actually, we cannot kill all the screens
           do (elscreen-kill))
  (cl-assert (and (= 1 (elscreen-get-number-of-screens)))))

(defun elscreen-workspace-show-workspace-info ()
  (elscreen-message (format "workspace %d/%d"
                            elscreen-workspace--current-index
                            (1- (length elscreen-workspace--workspaces)))))

(defun elscreen-workspace-open-workspace ()
  "Create a new workspace"
  (interactive)
  ;; (message "DEBUG: opening a new workspace")
  ;; save current workspace
  (elscreen-workspace-update-current-workspace)
  ;; safety first, let us save workspaces to file
  (elscreen-workspace-save t)
  ;; then, kill all the screens in the current workspace
  (elscreen-workspace-kill-all-tabs)
  ;; move to default buffer as if we are starting a new session
  (switch-to-buffer (get-buffer-create elscreen-workspace-default-buffer))
  (setq elscreen-workspace--current-index (length elscreen-workspace--workspaces))
  ;; append new workspace object to elscreen-workspace--workspaces
  (setcdr (last elscreen-workspace--workspaces) (list (elscreen-workspace-ws)))
  (elscreen-notify-screen-modification 'force-immediately)
  (message "New workspace is %d/%d"
           elscreen-workspace--current-index
           (1- (length elscreen-workspace--workspaces)))
  (when (>= elscreen-workspace--current-index (length elscreen-workspace--workspaces))
    (error "elscreen-workspace-create-workspace(), index is invalid")))

(defun elscreen-workspace-remove-nth (n lst)
  "Remove Nth element in list LST and return a new list."
  (append (cl-subseq lst 0 n) (nthcdr (1+ n) lst)))

(defun elscreen-workspace-helm-mm-migemo-string-match-p (input str)
  "Do `string-match-p' with the help of `helm-mm-migemo-get-pattern'."
  ;; (message "DEBUG: input=%s, str=%s" input str)
  (cl-loop with pattern = (helm-mm-3-get-patterns-internal input)
           for re in (mapcar (lambda (elt)
                               (helm-mm-migemo-get-pattern (cdr elt)))
                             pattern)
           always (string-match-p re str)))

(defun elscreen-workspace-switch-to-nth-workspace (n &optional input)
  (cond
   ((window-minibuffer-p)
    (error "elscreen-workspace, current window is minbuffer!"))
   ((and (not (= n elscreen-workspace--current-index))
         (>= n 0)
         (< n (length elscreen-workspace--workspaces)))
    (elscreen-workspace--update (elscreen-workspace-get-current-workspace))
    (elscreen-workspace-restore-workspace (nth n elscreen-workspace--workspaces))
    (setq elscreen-workspace--current-index n)
    (when (and (stringp input) (> (length input) 1))
      (elscreen-workspace--move-to-matched-screen-maybe input))
    (elscreen-workspace-show-workspace-info)
    (redisplay)
    (elscreen-notify-screen-modification 'force-immediately))
   (t
    nil)))

(defun elscreen-workspace--move-to-matched-screen-maybe (input)
  (cond
   ((string= input (elscreen-workspace-get-current-workspace-name))
    ;; If INPUT exactly matches with workspace name, we assume that a
    ;; user do not want any buffer switching.
    nil)
   (t
    ;; Go to screen which has a buffer whose name matches INPUT.
    (let* ((screen (sort
                    (cl-remove-if-not
                     (lambda (screen-to-name)
                       (cl-find-if
                        (lambda (s)
                          (or (string-match-p input s)
                              (and (featurep 'helm)
                                   helm-migemo-mode
                                   (fboundp 'helm-mm-migemo-get-pattern)
                                   (elscreen-workspace-helm-mm-migemo-string-match-p
                                    input s))))
                        ;; if there is more than one buffer in one
                        ;; screen, those names are concatenated with
                        ;; separator being ":".
                        (split-string (cdr screen-to-name) ":" t)))
                     ;; each elt is like (screen-number . buffer-name)
                     (elscreen-get-screen-to-name-alist))
                    (lambda (scr1 scr2)
                      (< (car scr1) (car scr2))))))
      ;; when there is more than one screen matching INPUT, let the
      ;; smallest screen number be our destination.
      ;; (message "DEBUG: screen=%s" screen)
      (and screen (elscreen-goto (caar screen)))))))

;; from elscreen.el
;; (defun elscreen-mode-line-update ()
;;   (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
;;     (setq elscreen-mode-line-string
;;           (format "[%d]" (elscreen-get-current-screen)))
;;     (force-mode-line-update)))
(defun elscreen-workspace-get-workspace-string ()
  "Return a string to display in mode-line."
  (cond
   (elscreen-workspace-mode
    (format "[%s:%d]"
            (elscreen-workspace-format-workspace-number-or-name
             elscreen-workspace--current-index
             (elscreen-workspace-get-workspace-name))
            (elscreen-get-current-screen)))
   (t
    (format "[%d:%d]" elscreen-workspace--current-index (elscreen-get-current-screen)))))

(defun elscreen-workspace-elscreen-mode-line-update-override-ad-func ()
  "Advising function to override the behavior of the original `elscreen-mode-line-update'.

Just add the index of the current workspace to the original string."
  (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
    (setq elscreen-mode-line-string
          (if elscreen-workspace-mode
              ;; format is [workspace-index:screen-index]
              (elscreen-workspace-get-workspace-string)
            (format "[%d]" (elscreen-get-current-screen))))
    (force-mode-line-update)))

(advice-add 'elscreen-mode-line-update :override #'elscreen-workspace-elscreen-mode-line-update-override-ad-func)

;; to remove advice
;; (advice-remove 'elscreen-mode-line-update #'elscreen-workspace-elscreen-mode-line-update-override-ad-func)

(defun elscreen-workspace-kill-workspace ()
  "Delete current workspace. If there is only one workspace, do nothing."
  (interactive)
  (cond
   ((elscreen-workspace-workspace-single-p)
    (elscreen-message "You cannot kill only one workspace!"))
   (t
    ;; either of the two situations below occurs.
    ;; ws1 ws2 ws3
    ;;          ^
    ;;          |
    ;;        delete this, then make ws2 the current ws
    ;; in this case, new index will be (1- index)
    ;;
    ;; ws1 ws2 ws3
    ;;  ^   ^
    ;;  |   |
    ;;  delete one of these, then make ws3 the current ws when ws2 is
    ;;  removed, and ws1 when ws2 is removed.
    ;; in this case, index will not change.

    (let ((target-index elscreen-workspace--current-index)
          (temp-index (if (= elscreen-workspace--current-index
                             (1- (length elscreen-workspace--workspaces)))
                          (1- elscreen-workspace--current-index)
                        (1+ elscreen-workspace--current-index))))
      ;; first, switch to the adjacent workspace before actually deleting workspace.
      ;; (message "DEBUG: len(workspaces)=%d" (length elscreen-workspace--workspaces))
      ;; (message "DEBUG: current workspace is %d" elscreen-workspace--cursrent-index)
      ;; (message "DEBUG: switching to workspace, index=%d" temp-index)
      (elscreen-workspace-switch-to-nth-workspace temp-index)
      ;; then, delete target workspace from elscreen-workspace--workspaces
      ;; (message "DEBUG: deleting workspace index=%d" target-index)
      (setq elscreen-workspace--workspaces (elscreen-workspace-remove-nth
                                          target-index
                                          elscreen-workspace--workspaces))
      ;; update current index
      (when (= elscreen-workspace--current-index (length elscreen-workspace--workspaces))
        ;; when the last one in workspaces was deleted
        (cl-decf elscreen-workspace--current-index))
      ;; (message "DEBUG: len(workspaces)=%d" (length elscreen-workspace--workspaces))
      ))))

(defun elscreen-workspace-get-workspace-name (&optional ws)
  "Return the name of the current workspace or workspace WS if ws is
non nil. When both of them are nil, return empty string."
  (elscreen-workspace--get-name (or ws
                                    (elscreen-workspace-get-current-workspace))))

(defun elscreen-workspace-rename-workspace (name)
  "Set the name of current workspace to NAME."
  (setf (slot-value (elscreen-workspace-get-current-workspace) 'name) name))

(defun elscreen-workspace--get-workspace-names ()
  "Return a list of workspace names."
  (mapcar (lambda (ws) (slot-value ws 'name))
          elscreen-workspace--workspaces))

(defun elscreen-workspace-name-workspace (name)
  "Name or rename current workspace. For resetting purpose, NAME
may be empty."
  (interactive (list (read-from-minibuffer
                      (if (elscreen-workspace-get-workspace-name)
                          ;; rename
                          "Rename to : "
                        (format "Name for workspace %d: "
                                elscreen-workspace--current-index)))))
  ;; (message "DEBUG: name=|%s|" name)
  (elscreen-workspace-rename-workspace name)
  ;; update workspace
  (elscreen-workspace-update-current-workspace)
  (message (format "New name is %s" (if (zerop (length name)) "empty" name))))

(defun elscreen-workspace--get-buffer-names-for-workspace (workspace)
  "Return a list of lists of buffers names in workspace WORKSPACE."
  (mapcar (lambda (scr)
            (cl-remove-duplicates (mapcar (lambda (e) (nth 1 e))
                                          ;; buffer name is 1st element
                                          scr)
                                  :test #'string=))
          (elscreen-workspace--get-buffers workspace)))

(defun elscreen-workspace-format-workspace-number-or-name (ix name)
  (format "[%d:%s]"
          ix
          name
          ;; (substring-no-properties name
          ;;                          0
          ;;                          (min (length name)
          ;;                               elscreen-workspace-workspace-name-limit))
          ))

(defun elscreen-workspace-format-current-workspace-number-or-name ()
  (elscreen-workspace-format-workspace-number-or-name
   elscreen-workspace--current-index
   (elscreen-workspace-get-workspace-name)))

(defun elscreen-workspace-get-helm-candidates ()
  ;; update has to be done here, neither in helm :init slot nor in
  ;; cl-loop initially clause.
  (elscreen-workspace-update-current-workspace)
  (cl-loop for ws in elscreen-workspace--workspaces
           for buf-names = (elscreen-workspace--get-buffer-names-for-workspace ws)
           for ix from 0
           for ws-name = (elscreen-workspace-get-workspace-name ws)
           collect (cons
                    (concat
                     (elscreen-workspace-format-workspace-number-or-name ix ws-name)
                     ": "
                     (mapconcat #'identity
                                (mapcar
                                 (lambda (lst)
                                   (mapconcat
                                    #'identity lst
                                    elscreen-workspace-helm-buffer-separator))
                                 buf-names)
                                elscreen-workspace-helm-screen-separator))
                    ix)))

(defun elscreen-workspace-switch-workspace-through-helm ()
  "Switch workspace through helm interface."
  (interactive)
  (let* ((choice
          (save-selected-window
            (helm
             :buffer elscreen-workspace-helm-buffer-name
             :sources (helm-build-sync-source "helm-elscreen-workspace--workspaces"
                        :candidates (elscreen-workspace-get-helm-candidates)
                        :migemo t
                        :volatile t)
             :preselect
             (format "^%s:"
                     (regexp-quote
                      (elscreen-workspace-format-current-workspace-number-or-name)))))))
    (when (numberp choice)
      (elscreen-workspace-switch-to-nth-workspace choice helm-input))))

(defun elscreen-workspace-clear ()
  "Function for debugging purpose."
  (interactive)
  (setq elscreen-workspace--workspaces nil
        elscreen-workspace--current-index 0
        elscreen-workspace--workspace-names nil))

;;;###autoload
(define-minor-mode elscreen-workspace-mode
  "Toggle elscreen-workspace-mode.
With a prefix argument ARG, enable elscreen-workspace-mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil."
  :group 'elscreen
  :global t
  (if elscreen-workspace-mode
      (progn
        (add-hook 'kill-emacs-hook #'elscreen-workspace-save t)
        (add-hook 'window-setup-hook #'elscreen-workspace-restore t))
    (remove-hook 'kill-emacs-hook #'elscreen-workspace-save)
    (remove-hook 'window-setup-hook #'elscreen-workspace-restore)))

(provide 'elscreen-workspace)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; elscreen-workspace.el ends here
