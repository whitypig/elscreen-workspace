;;; elscreen-persist.el --- persist the elscreen across sessions
;; Copyright (C) 2014 Hironori Yoshida

;; Author: Hironori Yoshida <webmaster@robario.com>
;; Keywords: elscreen frames
;; Version: 0.3.0
;; Package-Requires: ((elscreen "1.4.6") (revive "2.19"))

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

;; This makes elscreen persistent.
;;
;; To use this, use customize to turn on `elscreen-persist-mode`
;; or add the following line somewhere in your init file:
;;
;;     (elscreen-persist-mode 1)
;;
;; Or manually, use `elscreen-persist-store` to store,
;; and use `elscreen-persist-restore` to restore.
;;
;; Or manually, use `elscreen-persist-get-data` to get data to store,
;; and use `elscreen-persist-set-data` to set data to restore.
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'cl-lib)
(require 'elscreen)
(require 'revive)

(defcustom elscreen-persist-file (locate-user-emacs-file "elscreen")
  "The file where the elscreen configuration is stored."
  :type 'file
  :group 'elscreen)

;; should we change the group of the following customs to 'elscreen-persist?
(defcustom elscreen-persist-default-buffer "*scratch*"
  "Default buffer to be visited when a new workspace is created."
  :type 'string
  :group 'elscreen)

(defcustom elscreen-persist-workspace-name-limit 10
  "The upper limit of workspace name. If the length of a
  workspace name is more than this value, only the first
  `elscreen-persist-workspace-name-limit' number of characters
  are displayed."
  :type 'integer
  :group 'elscreen)

(defcustom elscreen-persist-helm-buffer-separator " + "
  "Used as a separator of buffers in one screen when displaying
  helm candidates"
  :type 'string
  :group 'elscreen)

(defcustom elscreen-persist-helm-screen-separator " | "
  "Used as a seperator of screens in workspace when displaying
  helm candidates."
  :type 'string
  :group 'elscreen)

;;; Variables:
(defvar elscreen-persist--workspaces nil
  "A list of screens. Each screen contains one or more tabs.")

(defvar elscreen-persist--workspace-names '("")
  "A list of workspace names")

(defvar elscreen-persist--current-index 0
  "Index in `elscreen-persist--workspaces'")

(defvar elscreen-persist-helm-buffer-name "*helm elscreen workspaces*")

;;;###autoload
(defun elscreen-persist-get-frame-params ()
  "Determine the frame parameters."
  (let ((frame-parameters (frame-parameters)))
    ;; Delete some unserializable frame parameter.
    (dolist (key '(buffer-list buried-buffer-list minibuffer))
      (delq (assq key frame-parameters) frame-parameters))
    frame-parameters))

;;;###autoload
(defun elscreen-persist-get-screens ()
  "Determine the screens, window configurations."
  (let ((current-screen (elscreen-get-current-screen))
        screen-to-window-configuration-alist)
    ;; Collect all the screen and window configurations.
    ;; - The first element is a last (max screen number) screen configuration.
    ;; - The last element is a current screen configuration.
    (dolist (screen (sort (elscreen-get-screen-list) '<))
      (elscreen-goto screen)
      (let ((screen-to-window-configuration
             (list (cons
                    screen
                    (current-window-configuration-printable)))))
        (setq screen-to-window-configuration-alist
              (if (eq screen current-screen)
                  (append screen-to-window-configuration-alist screen-to-window-configuration)
                (append screen-to-window-configuration screen-to-window-configuration-alist)))))
    (elscreen-goto current-screen)
    screen-to-window-configuration-alist))

;;;###autoload
(defun elscreen-persist-get-nicknames ()
  "Determine the nicknames."
  (let (screen-to-nickname-alist)
    ;; Collect all the nicknames.
    (dolist (screen (sort (elscreen-get-screen-list) '<))
      (setq screen-to-nickname-alist
            (append screen-to-nickname-alist
                    (list (elscreen-get-screen-nickname screen)))))
    screen-to-nickname-alist))

;;;###autoload
(defun elscreen-persist-get-data ()
  "Determine the frame parameters, screens, window configurations and nicknames."
  (list (list 'frame-parameters (elscreen-persist-get-frame-params))
        (list 'screen-to-window-configuration-alist (elscreen-persist-get-screens))
        (list 'screen-to-nickname-alist (elscreen-persist-get-nicknames))
        (list 'workspace-name (elscreen-persist-get-workspace-name))))

(defun elscreen-persist-update-current-workspace ()
  "Update info about current workspace in `elscreen-persist--workspaces'."
  (let ((ws (elscreen-persist-get-data)))
    (cond
     ((null elscreen-persist--workspaces)
      ;; no workspce in workspace list, so start a new workspace
      (setq elscreen-persist--workspaces (list ws))
      (setq elscreen-persist--current-index 0)
      (setq elscreen-persist--workspace-names '("")))
     ((>= elscreen-persist--current-index (length elscreen-persist--workspaces))
      ;; another workspace has been just created, so appened new one.
      (setcdr (last elscreen-persist--workspaces) (list ws))
      ;; use empty string as default name
      (setcdr (last elscreen-persist--workspace-names) (list "")))
     (t
      ;; update current workspace data
      (setf (nth elscreen-persist--current-index elscreen-persist--workspaces) ws)))))

(defun elscreen-persist-store ()
  "Store the screens, window configurations, nicknames and frame parameters."
  (interactive)
  ;; update current workspace info
  (elscreen-persist-update-current-workspace)
  ;; Store the configurations.
  (with-temp-file elscreen-persist-file
    (let ((print-length nil)
          (print-level nil))
      ;; save workspace list
      ;; for now, save only workspace list
      (insert (prin1-to-string elscreen-persist--workspaces)))))

;;;###autoload
(defun elscreen-persist-set-frame-params (data)
  "Set the frame parameters if necessary."
  (unless (and (boundp 'desktop-restore-frames) desktop-restore-frames
               (fboundp 'desktop-full-lock-name) (file-exists-p (desktop-full-lock-name)))
    (modify-frame-parameters nil data)
    (message "The frame was restored by `elscreen-persist'. Using `desktop' is recommended.")))

;;;###autoload
(defun elscreen-persist-set-screens (data)
  "Set the screens, window configurations."
  ;; Note:
  ;; #'elscreen-kill restores window-configuration with its own
  ;; #'elscreen-apply-window-configuration, so let them first play a
  ;; role. After that, we do our own job. Otherwise, restored window
  ;; configuration will be modified again by elscreen.
  ;; First, create enough number of screens
  ;; (message "DEBUG: creating screens")
  (dolist (screen-to-window-configuration data)
    (while (not (elscreen-screen-live-p (car screen-to-window-configuration)))
      (elscreen-create)))
  ;; then kill uncecessary screens
  ;; (message "DEBUG: killing screens")
  (dolist (screen (elscreen-get-screen-list))
    (unless (assq screen data)
      (elscreen-kill-internal screen)))
  ;; finally, restore window configurtion
  ;; (message "DEBUG: restoring screens")
  (dolist (screen-to-window-configuration data)
    ;; (message "DEBUG: goto %s" (prin1-to-string screen-to-window-configuration))
    (unless (window-minibuffer-p)
      ;; Note: For some reason or other, sometimes we somehow get to
      ;; minibuffer window, and try to delete other window to restore
      ;; window configuration. This raises unpleasant error. So, make
      ;; sure we are NOT in minibuffer widow.
      (elscreen-goto (car screen-to-window-configuration))
      ;; (message "DEBUG: then, restoring, window=%s, buffer=%s" (selected-window) (buffer-name (window-buffer)))
      ;; (message "DEBUG: restoring conf=%s" (cdr screen-to-window-configuration))
      (restore-window-configuration (cdr screen-to-window-configuration)))))

;;;###autoload
(defun elscreen-persist-set-nicknames (data)
  "Set the nicknames."
  (dolist (screen (sort (elscreen-get-screen-list) '<))
    (let ((nickname (nth screen data)))
      (when nickname
        (elscreen-set-screen-nickname screen nickname)))))

;;;###autoload
(defun elscreen-persist-set-data (data)
  "Set the frame parameters, screens, window configurations and nicknames."
  (elscreen-persist-set-frame-params (car (assoc-default 'frame-parameters data)))
  ;; (message "DEBUG: calling elsper-set-screens")
  (elscreen-persist-set-screens (car (assoc-default 'screen-to-window-configuration-alist data)))
  ;; (message "DEBUG: got back from elsper-set-screens")
  (elscreen-persist-set-nicknames (car (assoc-default 'screen-to-nickname-alist data))))

;;;###autoload
(defun elscreen-persist-restore ()
  "Restore the screens, window configurations, nicknames, and also the frame parameters if necessary."
  (interactive)
  (when (file-exists-p elscreen-persist-file)
    (setq elscreen-persist--workspaces
          (read (with-temp-buffer
                  (insert-file-contents elscreen-persist-file)
                  (buffer-string))))
    ;; for now, use 0 as default index
    (setq elscreen-persist--current-index 0)
    (setq elscreen-persist--workspace-names (elscreen-persist--get-workspace-names))
    ;; then, restore
    (elscreen-persist-set-data (car elscreen-persist--workspaces))))

(defun elscreen-persist-workspace-single-p ()
  (= 1 (length elscreen-persist--workspaces)))

(defun elscreen-persist-update-workspace-index (delta)
  (setq elscreen-persist--current-index
        (% (+ (+ delta elscreen-persist--current-index)
              (length elscreen-persist--workspaces))
           (length elscreen-persist--workspaces))))

(defun elscreen-persist-switch-workspace-by-delta (delta)
  "Switch workspace to another one which is DELTA distance away."
  (assert (not (zerop delta)) t "switching workspace, delta cannot be zero")
  ;; (message "DEBUG: goto %s workspace, current-buffer=%s"
  ;;          (if (> delta 0) "next" "previsou") (buffer-name))
  ;; save current workspace info into memory
  (elscreen-persist-update-current-workspace)
  ;; killing unnecessary tabs is executed in elscreen-persist-set-data.
  ;; (elscreen-persist-kill-all-tabs)

  ;; debug
  ;; (cl-loop for winconf in (assoc 'screen-to-window-configuration-alist
  ;;                                (nth elscreen-persist--current-index
  ;;                                     elscreen-persist--workspaces))
  ;;          initially do (message "DEBUG: before switching")
  ;;          do (cl-loop for screen in winconf
  ;;                      do (message "DEBUG: screen=%s" (prin1-to-string screen))))
  ;; update index
  (elscreen-persist-update-workspace-index delta)
  (elscreen-persist-set-data
   (nth elscreen-persist--current-index elscreen-persist--workspaces))
  (elscreen-notify-screen-modification 'force-immediately))

(defun elscreen-persist-goto-next-workspace ()
  "Switch to the next workspace."
  (interactive);
  ;; (message "DEBUG: goto next ws from index=%d" elscreen-persist--current-index)
  (if (elscreen-persist-workspace-single-p)
      (elscreen-message "You should have at least two groups of screens to move around!")
    (elscreen-persist-switch-workspace-by-delta 1))
  ;; (message "DEBUG: current workspace, index=%d" elscreen-persist--current-index)
  )

(defun elscreen-persist-goto-previous-workspace ()
  "Switch to the previous workspace"
  (interactive)
  ;; (message "DEBUG: goto previous ws from index=%d" elscreen-persist--current-index)
  (if (elscreen-persist-workspace-single-p)
      (elscreen-message "You should have at least two groups of screens to move around!")
    (elscreen-persist-switch-workspace-by-delta -1))
  ;; (message "DEBUG: current workspace, index=%d" elscreen-persist--current-index)
  )

(defun elscreen-persist-kill-all-tabs ()
  "Kill all the screens"
  (cl-loop repeat (1- (elscreen-get-number-of-screens))
           ;; actually, we cannot kill all the screens
           do (elscreen-kill))
  (assert (and (= 1 (elscreen-get-number-of-screens)))))

(defun elscreen-persist-show-workspace-info ()
  (elscreen-message (format "workspace %d/%d"
                            elscreen-persist--current-index
                            (1- (length elscreen-persist--workspaces)))))

(defun elscreen-persist-open-workspace ()
  "Create a new workspace"
  (interactive)
  ;; (message "DEBUG: opening a new workspace")
  ;; save current workspace
  (elscreen-persist-update-current-workspace)
  ;; safety first, let us save workspaces to file
  (elscreen-persist-store)
  ;; then, kill all the screens in the current workspace
  (elscreen-persist-kill-all-tabs)
  ;; move to scratch buffer as if we are starting a new session
  (switch-to-buffer (get-buffer-create elscreen-persist-default-buffer))
  (setq elscreen-persist--current-index (length elscreen-persist--workspaces))
  (elscreen-persist-update-current-workspace)
  (elscreen-notify-screen-modification 'force-immediately)
  (when (>= elscreen-persist--current-index (length elscreen-persist--workspaces))
    (error "elscreen-persist-create-workspace(), index is invalid"))
  )

(defun elscreen-persist-remove-nth (n lst)
  "Remove Nth element in list LST and return a new list."
  (append (cl-subseq lst 0 n) (nthcdr (1+ n) lst)))

(defun elscreen-persist-helm-mm-migemo-string-match-p (input str)
  "Do `string-match-p' with the help of `helm-mm-migemo-get-pattern'."
  (cl-loop with pattern = (helm-mm-3-get-patterns-internal input)
           for re in (mapcar (lambda (elt)
                               (helm-mm-migemo-get-pattern (cdr elt)))
                             pattern)
           always (string-match-p re str)))

(defun elscreen-persist-switch-to-nth-workspace (n &optional input)
  (cond
   ((window-minibuffer-p)
    (error "elscreen-persist, current window is minbuffer!"))
   ((and (not (= n elscreen-persist--current-index))
         (>= n 0)
         (< n (length elscreen-persist--workspaces)))
    (elscreen-persist-set-data (nth n elscreen-persist--workspaces))
    (setq elscreen-persist--current-index n)
    (when (and (stringp input) (> (length input) 1))
      ;; when switching from helm-buffer and input is not empty, goto
      ;; screen which has a buffer whose name matches that input
      (let* ((screen (sort
                      (cl-remove-if-not
                       (lambda (screen-to-name)
                         (cl-find-if
                          (lambda (s)
                            (or (string-match-p input s)
                                (and (featurep 'helm)
                                     helm-migemo-mode
                                     (fboundp 'helm-mm-migemo-get-pattern)
                                     (elscreen-persist-helm-mm-migemo-string-match-p
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
        (and screen (elscreen-goto (caar screen)))))
    (elscreen-notify-screen-modification 'force-immediately))
   (t
    nil)))

;; from elscreen.el
;; (defun elscreen-mode-line-update ()
;;   (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
;;     (setq elscreen-mode-line-string
;;           (format "[%d]" (elscreen-get-current-screen)))
;;     (force-mode-line-update)))

(defun elscreen-persist-get-workspace-string ()
  "Return a string to display in mode-line."
  (cond
   (elscreen-persist-mode
    (format "[%s:%d]"
            (elscreen-persist-format-workspace-number-or-name
             elscreen-persist--current-index (elscreen-persist-get-workspace-name))
            (elscreen-get-current-screen)))

   (t
    (format "[%d:%d]" elscreen-persist--current-index (elscreen-get-current-screen)))))

(defun elscreen-persist-elscreen-mode-line-update-override-ad-func ()
  "Advising function to override the behavior of the original `elscreen-mode-line-update'.

Just add the index of the current workspace to the original string."
  (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
    (setq elscreen-mode-line-string
          (if elscreen-persist-mode
              ;; format is [workspace-index:screen-index]
              (elscreen-persist-get-workspace-string)
            (format "[%d]" (elscreen-get-current-screen))))
    (force-mode-line-update)))

(advice-add 'elscreen-mode-line-update
            :override
            #'elscreen-persist-elscreen-mode-line-update-override-ad-func)

;; to remove advice
;; (advice-remove 'elscreen-mode-line-update #'elscreen-persist-elscreen-mode-line-update-override-ad-func)

(defun elscreen-persist-kill-workspace ()
  "Delete current workspace. If there is only one workspace, do nothing."
  (interactive)
  (cond
   ((elscreen-persist-workspace-single-p)
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

    (let ((target-index elscreen-persist--current-index)
          (temp-index (if (= elscreen-persist--current-index
                             (1- (length elscreen-persist--workspaces)))
                          (1- elscreen-persist--current-index)
                       (1+ elscreen-persist--current-index))))
      ;; first, switch to the adjacent workspace before actually deleting workspace.
      ;; (message "DEBUG: len(workspaces)=%d" (length elscreen-persist--workspaces))
      ;; (message "DEBUG: current workspace is %d" elscreen-persist--cursrent-index)
      ;; (message "DEBUG: switching to workspace, index=%d" temp-index)
      (elscreen-persist-switch-to-nth-workspace temp-index)
      ;; then, delete target workspace
      ;; (message "DEBUG: deleting workspace index=%d" target-index)
      (setq elscreen-persist--workspaces (elscreen-persist-remove-nth
                                          target-index
                                          elscreen-persist--workspaces))
      (setq elscreen-persist--workspace-names (elscreen-persist-remove-nth
                                               target-index
                                               elscreen-persist--workspace-names))
      ;; update current index
      (when (= elscreen-persist--current-index (length elscreen-persist--workspaces))
        ;; when the last one in workspaces was deleted
        (cl-decf elscreen-persist--current-index))
      ;; (message "DEBUG: len(workspaces)=%d" (length elscreen-persist--workspaces))
      ))))

(defun elscreen-persist-get-workspace-name ()
  "Return the name of current workspace or empty string if not set."
  (or (nth elscreen-persist--current-index elscreen-persist--workspace-names)
      ""))

(defun elscreen-persist-set-workspace-name (name)
  "Set the name of current workspace to NAME."
  (unless elscreen-persist--workspace-names
    ;; init
    (setq elscreen-persist--workspace-names
          (make-list (length elscreen-persist--workspaces) "")))
  (setf (nth elscreen-persist--current-index elscreen-persist--workspace-names)
        name))

(defun elscreen-persist--get-workspace-names ()
  "Return a list of workspace names."
  (cl-loop for ws in elscreen-persist--workspaces
           collect (elscreen-persist--get-workspace-name-for-workspace ws)))

(defun elscreen-persist--get-workspace-name-for-workspace (workspace)
  "Return workspace name for workspace WORKSPACE."
  (car (assoc-default 'workspace-name workspace)))

(defun elscreen-persist-name-workspace (name)
  "Name or rename current workspace. For resetting purpose, NAME
may be empty."
  (interactive (list (read-from-minibuffer
                      (if (elscreen-persist-get-workspace-name)
                          ;; rename
                          "Rename to : "
                        (format "Name for workspace %d: "
                                elscreen-persist--current-index)))))
  ;; (message "DEBUG: name=|%s|" name)
  (elscreen-persist-set-workspace-name name)
  ;; update workspace
  (elscreen-persist-update-current-workspace)
  (message (format "New name is %s" (if (zerop (length name)) "empty" name))))

(defun elscreen-persist--get-buffer-info-for-worksapce (workspace)
  "Return a list of buffer info in workspace WORKSPACE. Buffer
info is also a list corresponding to one screen."
  (mapcar (lambda (elt) (nth 4 elt))
          ;; buffer info is 4th element in window configuration
          (sort (copy-sequence
                 (elscreen-persist--get-workspace-window-configuration workspace))
                ;; sort by screen number
                (lambda (x y) (< (car x) (car y))))))

(defun elscreen-persist--get-buffer-names-for-workspace (workspace)
  "Return a list of lists of buffers names in workspace WORKSPACE."
  (mapcar (lambda (scr)
            (cl-remove-duplicates (mapcar (lambda (e) (nth 1 e))
                                          ;; buffer name is 1st element
                                          scr)
                                  :test #'string=))
          (elscreen-persist--get-buffer-info-for-worksapce ws)))

(defun elscreen-persist--get-workspace-window-configuration (workspace)
  (car (assoc-default 'screen-to-window-configuration-alist ws)))

(defun elscreen-persist-format-workspace-number-or-name (ws-index name)
  (cond
   ((zerop (length name))
    (format "%d" ws-index))
   ((<= (length name) elscreen-persist-workspace-name-limit)
    (format "%s" name))
   (t
    (format "%s"
            (substring-no-properties name
                                     0
                                     elscreen-persist-workspace-name-limit)))))

(defun elscreen-persist-get-helm-candidates ()
  ;; update has to be done here, neither in helm :init slot nor in
  ;; cl-loop initially clause.
  (elscreen-persist-update-current-workspace)
  (cl-loop for ws in elscreen-persist--workspaces
           for buf-names = (elscreen-persist--get-buffer-names-for-workspace ws)
           for ix from 0
           for ws-name = (elscreen-persist--get-workspace-name-for-workspace ws)
           collect (cons
                    (concat
                     (elscreen-persist-format-workspace-number-or-name ix ws-name)
                     ": "
                     (mapconcat #'identity
                                (mapcar
                                 (lambda (lst)
                                   (mapconcat
                                    #'identity lst
                                    elscreen-persist-helm-buffer-separator))
                                 buf-names)
                                elscreen-persist-helm-screen-separator))
                    ix)))

(defun elscreen-persist-switch-workspace-through-helm ()
  "Switch workspace through helm interface."
  (interactive)
  (let* ((choice (save-selected-window
                   (helm
                    :buffer elscreen-persist-helm-buffer-name
                    :sources (helm-build-sync-source "helm-elscreen-persist--workspaces"
                               :candidates (elscreen-persist-get-helm-candidates)
                               :migemo t
                               :volatile t)
                    :preselect (format "^%d:" elscreen-persist--current-index)))))
    ;; (when helm-alive-p
    ;;   (message "DEBUG: helm is alive")
    ;;   (helm-keyboard-quit))
    ;; (when (window-minibuffer-p (selected-window))
    ;;   (delete-window (selected-window)))
    ;; (when (stringp helm-input)
    ;;   (message "DEBUG: helm-input=%s" helm-input))
    (when (numberp choice)
      (elscreen-persist-switch-to-nth-workspace choice helm-input))))

(defun elscreen-persist-clear ()
  "Function for debugging purpose."
  (interactive)
  (setq elscreen-persist--workspaces nil
        elscreen-persist--current-index 0
        elscreen-persist--workspace-names nil))

;;;###autoload
(define-minor-mode elscreen-persist-mode
  "Toggle persistent elscreen (ElScreen Persist mode).
With a prefix argument ARG, enable ElScreen Persist mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :group 'elscreen
  :global t
  (if elscreen-persist-mode
      (progn
        (add-hook 'kill-emacs-hook #'elscreen-persist-store t)
        (add-hook 'window-setup-hook #'elscreen-persist-restore t))
    (remove-hook 'kill-emacs-hook #'elscreen-persist-store)
    (remove-hook 'window-setup-hook #'elscreen-persist-restore)))

(provide 'elscreen-persist)
;;; elscreen-persist.el ends here
