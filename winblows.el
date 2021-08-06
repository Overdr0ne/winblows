;;; winblows.el --- I got 98 Windows but Word ain't one.   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam <scmorris.dev@gmail.com>
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
;; (require 'window)

(defcustom winblows--split-window-below nil
  "If non-nil, vertical splits produce new windows below."
  :group 'windows
  :type 'boolean)
(defcustom winblows--split-window-right nil
  "If non-nil, horizontal splits produce new windows to the right."
  :group 'windows
  :type 'boolean)

(defcustom winblows--side-width 80
  "Width of side windows in Winblows.")
(defcustom winblows--side-height 10
  "Height of side windows in Winblows.")

;; https://www.reddit.com/r/emacs/comments/aepvwq/how_to_automatically_switch_focus_to_newly/edsvalc?utm_source=share&utm_medium=web2x&context=3
(defvar winblows--display-buffers-no-select '("*Completions*"))

(defun winblows--display-buffer-advice (f &rest args)
  (let ((w (apply f args)))
    (when (and (windowp w)
               (not (cl-find-if
                     #'get-buffer-window winblows-display-buffers-no-select)))
      (select-window w))
    w))

(defun winblows-follow ()
  (interactive)
  (advice-add 'display-buffer :around 'winblows--display-buffer-advice))
(defun winblows-unfollow ()
  (interactive)
  (advice-remove 'display-buffer 'winblows--display-buffer-advice))

(defun winblows-auto ()
  (interactive)
  (setq
   ;; Default rules
   display-buffer-alist
   `(;; Display *Help* buffer at the bottom-most slot
     ("\\(*\\(info\\|grep*\\|.*systemctl.*\\|Help\\|help\\|helpful\\|trace-\\|Backtrace\\|RefTeX.*\\)\\)"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 80)
      (reusable-frames . visible))
     ("\\(magit-diff\\|shelldon\\)"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-window)
      (pop-up-frame-parameters
       (width . 80)
       (left . 1.0)
       (fullscreen . fullheight)))
     ("\\(\\*draft\\*\\|Draft/\\)"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-frame)
      (pop-up-frame-parameters
       (width . 80)
       (left . 1.0)
       (fullscreen . fullheight)))
     ;; Display *BBDB* buffer on the bottom frame
     ("\\*BBDB"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-height . 10)
      (reusable-frames . visible))
     ;; Split shells at the bottom
     ("^\\*e?shell"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
      (window-min-height . 20)
      (reusable-frames . visible))
     ("magit"
      (display-buffer-reuse-window display-buffer-same-window)))))

(defun winblows--cardinal (direction &optional width height)
  (unless width (setq width winblows--side-width))
  (unless height (setq height winblows--side-height))
  (setq
   ;; Default rules
   display-buffer-alist
   `(;; Display *Help* buffer at the bottom-most slot
     ("\\*"
      ;; (display-buffer-in-side-window)
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . ,direction)
      (slot . 0)
      (window-width . ,width)
      (window-height . ,height)
      (reusable-frames . visible)))))
(defun winblows-east ()
  "Blow window East."
  (interactive)
  (winblows--cardinal 'right))
(defun winblows-west ()
  "Blow window West."
  (interactive)
  (winblows--cardinal 'left))
(defun winblows-north ()
  "Blow window North."
  (interactive)
  (winblows--cardinal 'top))
(defun winblows-south ()
  "Blow window South."
  (interactive)
  (winblows--cardinal 'bottom nil winblows--side-height))

(defun winblows-here ()
  (interactive)
  (setq
   ;; Default rules
   display-buffer-alist
   `(;; Display *Help* buffer at the bottom-most slot
     ("\\*"
      (display-buffer-same-window)))))
(defun winblows-display-buffer-other-window (buffer alist)
  "Display buffer BUF in another window."
  (unless (or (cdr (assq 'inhibit-same-window alist))
	      (window-minibuffer-p)
	      (window-dedicated-p))
    (other-window 1)
    (window--display-buffer buffer (selected-window) 'reuse)))
(defun winblows-there ()
  (interactive)
  (setq
   ;; Default rules
   display-buffer-alist
   `(;; Display *Help* buffer at the bottom-most slot
     ("\\*"
      (winblows-display-buffer-other-window)))))
(defun winblows-full ()
  (interactive)
  (setq
   ;; Default rules
   display-buffer-alist
   `(;; Display *Help* buffer at the bottom-most slot
     ("\\*"
      (display-buffer-same-window)
      (fullscreen . fullheight)))))

(define-minor-mode global-winblows-mode
  "Winblows blows your windows.

Where the wind blows is where your windows go. Set the wind direction and any
window spawned will follow.
"
  :init-value nil
  :lighter " Winblows"
  ;; :group 'winblows
  :global t)

(provide 'winblows)
;;; winblows.el ends here
