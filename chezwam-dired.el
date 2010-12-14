;;; chezwam-dired.el --- Dired configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired
;; Created: 2010-11-19
;; Last changed: 2010-12-10 00:16:01
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(defadvice dired-find-file (around cw:dired-find-file activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
	 (filename (dired-get-filename t t))
	 (bye-p (file-directory-p filename)))
    ad-do-it
    (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
      (kill-buffer orig))))

(defadvice dired-up-directory (around cw:dired-up-directory activate)
  "Replace current buffer with parent dir."
  (let* ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))

(require 'gnus-dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(define-key dired-mode-map (kbd "<C-return>")
  'gnus-dired-find-file-mailcap)

(provide 'chezwam-dired)
