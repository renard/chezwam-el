;;; chezwam-buffer.el --- Buffer behaviour setup.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration, buffer
;; Created: 2010-10-13
;; Last changed: 2011-05-19 00:44:29
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

;; window move definitions
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Frame move
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-|") 'other-frame)

(winner-mode 1)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq
 x-select-enable-clipboard t
 x-select-enable-primary t)

(defun cw:buffer:diff-with-file()
  "Allway call `diff-buffer-with-file' in non interactive mode."
  (interactive)
  (with-current-buffer (get-buffer (current-buffer))
    (if (and buffer-file-name
             (file-exists-p buffer-file-name))
        (let ((tempfile (make-temp-file "buffer-content-")))
          (unwind-protect
              (progn
                (write-region nil nil tempfile nil 'nomessage)
                (diff buffer-file-name tempfile "-Nu" t)
                (sit-for 0))
            (when (file-exists-p tempfile)
              (delete-file tempfile))))
      (message "Buffer %s has no associated file on disc" (buffer-name))
      ;; Display that message for 1 second so that user can read it
      ;; in the minibuffer.
      (sit-for 1)))
  ;; return always nil, so that save-buffers-kill-emacs will not move
  ;; over to the next unsaved buffer when calling `d'.
  nil)

(global-set-key (kbd "C-c C-d") 'cw:buffer:diff-with-file)

(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward))

(when (require 'ibuffer nil t)
  (global-set-key "\C-x\C-b" 'ibuffer)
  (iswitchb-mode))


(defun cw:make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))
(add-hook 'after-save-hook 'cw:make-buffer-file-executable-if-script-p)


(setq
 time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
 time-stamp-start "Last changed:\\\\? "  ; start of pattern
 time-stamp-end "\\\\?\n"                ; end of pattern
 time-stamp-active t                     ; do enable time-stamps
 time-stamp-line-limit 0)
(make-variable-buffer-local 'time-stamp-start)

(add-hook 'before-save-hook 'time-stamp)


(defun cw:buffer:delete-trailing-spaces ()
  "Clear lines with only spaces when moving around."
  (unless buffer-read-only
    (save-excursion
      (end-of-line)
      ;; This is an empty line, delete its spaces
      (when (looking-back "[ \t]+$")
        (delete-horizontal-space)))))

(defadvice newline
  (before cw:newline activate protect)
  "Delete spaces before going to next line."
  (cw:buffer:delete-trailing-spaces))

(provide 'chezwam-buffer)
