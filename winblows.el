;;; winblows.el --- I got 98 Windows but Word ain't one.   -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Winblows blows your windows. Where the wind blows is where your windows
;; go. Set the wind direction and any window spawned will follow.

;;; Code:
(require 'cl-lib)

(defgroup winblows nil
  "Winblows blows your windows."
  :group 'convenience
  :prefix "winblows-")

(defun deftoggle-var-doc (name)
  (concat "Non-nil if " name " is enabled.\n\n"
          "See " name
          " command for a description of this toggle."))
(defun deftoggle-fun-doc (name doc)
  (concat "Toggle " name " on or off.\n\n" doc
          "\n\nIf called interactively, enable Global Gumshoe-Buf mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is toggle; disable the mode otherwise.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, toggle toggles the state."))
(defmacro deftoggle (name doc enabler disabler)
  `(progn
     (defvar ,name nil ,(deftoggle-var-doc (symbol-name name)))
     (defun ,name (&optional enable)
       ,(deftoggle-fun-doc (symbol-name name) doc)
       (interactive)
       (if (called-interactively-p 'interactive)
           (progn
             (if ,name
                 ,disabler
               ,enabler)
             (setq ,name (not ,name)))
         (progn
           (if enable
               ,enabler
             ,disabler)
           (setq ,name enable))))))

(defvar winblows--dba-stash)
(defcustom winblows-auto-display-buffer-alist nil
  "`display-buffer-alist’ used in `winblows-auto’."
  :type 'alist)

(defcustom winblows--side-width 80
  "Width of side windows in Winblows."
  :type 'integer)
(defcustom winblows--side-height 10
  "Height of side windows in Winblows."
  :type 'integer)

(defvar winblows--display-buffers-no-select '("*Completions*"))

(defun winblows--display-buffer-advice (f &rest args)
  (let ((w (apply f args)))
    (when (and (windowp w)
               (not (cl-find-if
                     #'get-buffer-window winblows-display-buffers-no-select)))
      (select-window w))
    w))

;; https://www.reddit.com/r/emacs/comments/aepvwq/how_to_automatically_switch_focus_to_newly/edsvalc?utm_source=share&utm_medium=web2x&context=3
(deftoggle winblows-follow
  "Move point to any newly opened windows."
  (advice-add 'display-buffer :around 'winblows--display-buffer-advice)
  (advice-remove 'display-buffer 'winblows--display-buffer-advice))
;; (defun winblows-unfollow
;;   (interactive)
;;   (advice-remove 'display-buffer 'winblows--display-buffer-advice))

;; (defun winblows-auto
;;   (interactive)
;;   (setq display-buffer-alist winblows-auto-display-buffer-alist))

(defun winblows--swap-in-dba (dba)
  (setq winblows--dba-stash display-buffer-alist)
  (setq display-buffer-alist dba))

(defun winblows-stop ()
  (interactive)
  (setf display-buffer-alist winblows--user-display-buffer-alist))
(defun winblows--cardinal (direction width height)
  (winblows--swap-in-dba
   `((""
      ;; (display-buffer-in-side-window)
      (display-buffer-in-side-window)
      (side . ,direction)
      (slot . 0)
      (window-width . ,width)
      (window-height . ,height)
      (reusable-frames . visible)))))
(defvar winblows--current nil)
(defvar winblowingp nil)
(defun winblows--blowing-p (dir)
  (equal winblows--current dir))
(defvar winblows--user-display-buffer-alist display-buffer-alist)
(defmacro define-winblows-cmd (name doc blower)
  `(progn
     (defun ,name ()
       ,doc
       (interactive)
       (cond ((and winblowingp (equal winblows--current this-command))
              (winblows-stop)
              (message "%s disabled." ',name))
             (t ,blower
                (message "%s enabled." ',name)
                (setf winblowingp t)))
       (setq winblows--current this-command))))
(define-winblows-cmd winblows-east
  "Blow new windows East."
  (winblows--cardinal 'right winblows--side-width nil))
(define-winblows-cmd winblows-west
  "Blow new windows West."
  (winblows--cardinal 'left winblows--side-width nil))
(define-winblows-cmd winblows-north
  "Blow new windows North."
  (winblows--cardinal 'top nil winblows--side-height))
(define-winblows-cmd winblows-south
  "Blow new windows South."
  (winblows--cardinal 'bottom nil winblows--side-height))

(define-winblows-cmd winblows-here
  "Blow new windows to current window."
  (winblows--swap-in-dba
   `((""
      (display-buffer-same-window)))))
(defun winblows-display-buffer-other-window (buffer alist)
  "Display buffer BUFFER in another window."
  (unless (or (cdr (assq 'inhibit-same-window alist))
	      (window-minibuffer-p)
	      (window-dedicated-p))
    (other-window 1)
    (window--display-buffer buffer (selected-window) 'reuse)))
(define-winblows-cmd winblows-there
  "Blow new windows to other window."
  (winblows--swap-in-dba
   `((""
      (winblows-display-buffer-other-window display-buffer-pop-up-window)
      (inhibit-same-window)))))
(define-winblows-cmd winblows-full
  "Blow new windows up fullscreen."
  (winblows--swap-in-dba
   `((""
      (display-buffer-same-window)
      (fullscreen . fullheight)))))

(define-winblows-cmd winblows-right
  "Blow new windows right."
  (winblows--swap-in-dba
   `((""
      (display-buffer-in-direction display-buffer-pop-up-window)
      (inhibit-same-window)
      (direction . right)))))
(define-winblows-cmd winblows-left
  "Blow new windows left."
  (winblows--swap-in-dba
   `((""
      (display-buffer-in-direction)
      (inhibit-same-window)
      (direction . left)))))
(define-winblows-cmd winblows-up
  "Blow new windows up."
  (winblows--swap-in-dba
   `((""
      (display-buffer-in-direction)
      (inhibit-same-window)
      (direction . up)))))
(define-winblows-cmd winblows-down
  "Blow new windows down."
  (winblows--swap-in-dba
   `((""
      (display-buffer-in-direction)
      (inhibit-same-window)
      (direction . down)))))

(define-winblows-cmd winblows-pop-up
  "Spawn new windows circularly, but keep them at least 80 characters."
  ;; (fset #'split-window-sensibly #'winblows--split-window-sensibly)
  (winblows--swap-in-dba
   `((""
      (display-buffer-pop-up-window)))))

(define-winblows-cmd winblows-cycle
  "Blow new windows to adjascent slot."
  (winblows--swap-in-dba
   `((""
      (display-buffer-use-some-window display-buffer-pop-up-window)
      (inhibit-same-window)))))

(define-minor-mode global-winblows-mode
  "Winblows blows your windows.

Where the wind blows is where your windows go. Set the wind direction and any
window spawned will follow.
"
  :init-value nil
  :lighter " Winblows"
  ;; :group 'winblows
  :global t
  (if global-winblows-mode
      (setq winblows--user-display-buffer-alist display-buffer-alist)
    (setq display-buffer-alist winblows--user-display-buffer-alist)))

(provide 'winblows)
;;; winblows.el ends here
