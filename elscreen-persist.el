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

(require 'elscreen)
(require 'revive)

(defcustom elscreen-persist-file (locate-user-emacs-file "elscreen")
  "The file where the elscreen configuration is stored."
  :type 'file
  :group 'elscreen)

(defvar elscreen-persist-workspaces nil
  "A list of screens. Each screen contains one or more tabs.")

;; unnecessary?
(defvar elscreen-persist-current-workspace nil
  "Current workspace")

(defvar elscreen-persist-current-index 0
  "Index in `elscreen-persist-workspaces'")

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
      (let ((screen-to-window-configuration (list (cons screen (current-window-configuration-printable)))))
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
        (list 'screen-to-nickname-alist (elscreen-persist-get-nicknames))))

(defun elscreen-persist-update-current-workspace ()
  "Update info about current workspace in `elscreen-persist-workspaces'."
  (let ((ws (elscreen-persist-get-data)))
    (cond
     ((not elscreen-persist-workspaces)
      ;; no workspce in workspace list, so start a new workspace
      (setcdr (last elscreen-persist-workspaces) (list ws))
      (setq elscreen-persist-current-index 0))
     ((>= elscreen-persist-current-index (length elscreen-persist-workspaces))
      ;; another workspace has been just created
      (setcdr (last elscreen-persist-workspaces) (list ws)))
     (t
      ;; update current workspace data
      (setf (nth elscreen-persist-current-index elscreen-persist-workspaces) ws)))))

(defun elscreen-persist-store ()
  "Store the screens, window configurations, nicknames and frame parameters."
  (interactive)
  ;; Store the configurations.
  (with-temp-file elscreen-persist-file
    (let ((print-length nil)
          (print-level nil))
      ;; save workspace list
      ;; for now, save only workspace list
      (insert (prin1-to-string elscreen-persist-workspaces)))))

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
  (dolist (screen-to-window-configuration data)
    (while (not (elscreen-screen-live-p (car screen-to-window-configuration)))
      (elscreen-create))
    (elscreen-goto (car screen-to-window-configuration))
    (restore-window-configuration (cdr screen-to-window-configuration)))
  ;; Kill unnecessary screens.
  (dolist (screen (elscreen-get-screen-list))
    (unless (assq screen data)
      (elscreen-kill screen))))

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
  (elscreen-persist-set-screens (car (assoc-default 'screen-to-window-configuration-alist data)))
  (elscreen-persist-set-nicknames (car (assoc-default 'screen-to-nickname-alist data))))

;;;###autoload
(defun elscreen-persist-restore ()
  "Restore the screens, window configurations, nicknames, and also the frame parameters if necessary."
  (interactive)
  (when (file-exists-p elscreen-persist-file)
    (setq elscreen-persist-workspaces
          (read (with-temp-buffer
                  (insert-file-contents elscreen-persist-file)
                  (buffer-string))))
    ;; for now, use 0 as default index
    (setq elscreen-persist-current-index 0)
    ;; then, restore
    (elscreen-persist-set-data (car elscreen-persist-workspaces))))

(defun elscreen-persist-goto-next-workspace ()
  "Switch to the next workspace."
  (interactive)
  (message "DEBUG: got-next, current-buffer=%s" (buffer-name))
  (elscreen-persist-update-current-workspace)
  (elscreen-persist-increment-current-index)
  (message "DEBUG: goto-next-workspace, index=%d" elscreen-persist-current-index)
  (elscreen-persist-kill-all-tabs)
  (elscreen-persist-set-data
   (nth elscreen-persist-current-index elscreen-persist-workspaces)))

(defun elscreen-persist-goto-previous-workspace ()
  "Switch to the previous workspace"
  (interactive)
  (elscreen-persist-update-current-workspace)
  (elscreen-persist-decrement-current-index)
  (message "DEBUG: goto-previous-workspace, index=%d" elscreen-persist-current-index)
  (elscreen-persist-kill-all-tabs)
  (elscreen-persist-set-data
     (nth elscreen-persist-current-index elscreen-persist-workspaces)))

(defun elscreen-persist-kill-all-tabs ()
  (cl-loop repeat (1- (elscreen-get-number-of-screens))
           do (elscreen-kill)
           ;; actually, we cannot kill all the screens,
           ;; so let's start up with our old friend.
           finally (switch-to-buffer (get-buffer-create "*screatch*")))
  (message "DEBUG: kill-all-tabs, must have switched to scratch buffer, buffer=%s" (buffer-name)))

(defun elscreen-persist-increment-current-index ()
  (setq elscreen-persist-current-index
        (% (1+ elscreen-persist-current-index) (length elscreen-persist-workspaces))))

(defun elscreen-persist-decrement-current-index ()
  (setq elscreen-persist-current-index
        (% (+ (1- elscreen-persist-current-index) (length elscreen-persist-workspaces))
           (length elscreen-persist-workspaces))))

(defun elscreen-persist-create-workspace ()
  "Create a new workspace"
  (interactive)
  (message "DEBUG: creating new workspace")
  ;; save current workspace
  (elscreen-persist-update-current-workspace)
  ;; for safety first, also save to file
  (elscreen-persist-store)
  ;; then, kill all the screens in the current workspace
  (elscreen-persist-kill-all-tabs)
  (setq elscreen-persist-current-index (length elscreen-persist-workspaces))
  (elscreen-persist-update-current-workspace)
  (when (>= elscreen-persist-current-index (length elscreen-persist-workspaces))
    (error "elscreen-persist-create-workspace(), index is invalid"))
  )

(defun elscreen-persist-remove-nth (n lst)
  "Remove Nth element in list LST and return a new list."
  (append (cl-subseq lst 0 n) (nthcdr (1+ n) lst)))

(defun elscreen-persist-switch-to-nth-workspace (n)
  (elscreen-persist-set-data (nth n elscreen-persist-workspaces)))

(defun elscreen-persist-delete-workspace ()
  "Delete current workspace. If there is only one workspace, do nothing."
  (interactive)
  (cond
   ((= 1 (length elscreen-persist-workspaces))
    (messge "You cannot delete only one workspace!")
    (sit-for 1))
   (t
    ;; either of the two situations below occurs.
    ;; ws1 ws2 ws3
    ;;          ^
    ;;          |
    ;;        delete this, then make ws2 the current ws
    ;; in this case, new index will be (1- index)
    ;;
    ;; ws1 ws2 ws3
    ;;      ^
    ;;      |
    ;;    delete this, then make ws3 the current ws
    ;; in this case, index will not change
    (let ((new-index (if (= elscreen-persist-current-index
                            (1- (length elscreen-persist-workspaces)))
                         (1- elscreen-persist-current-index)
                       elscreen-persist-current-index)))
      (setq elscreen-persist-workspaces (elscreen-persist-remove-nth
                                         elscreen-persist-current-index
                                         elscreen-persist-workspaces))
      (setq elscreen-persist-current-index new-index)
      ;; switch to workspace
      (elscreen-persist-switch-to-nth-workspace elscreen-persist-current-index)))))

(defun elscreen-persist-clear ()
  "Function for debugging purpose."
  (interactive)
  (setq elscreen-persist-workspaces nil
        elscreen-persist-current-workspace
        elscreen-persist-current-index 0))

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
