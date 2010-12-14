;;; chezwam-doc-mode.el --- doc-mode configuration file

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-10-13
;; Last changed: 2010-11-30 05:30:12
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:

(require 'man nil t)

(defcustom cw:doc-mode:makefile "~/.emacs.d/Makefile/asciidoc.mak"
  "Where to find the asciidoc.mak Makefile")

(defun cw:doc-mode:compile ()
    "Compile manpage and display it in another buffer."
    (interactive)
    (unless (string-match "\\.[1-9]\\.txt$" (buffer-file-name))
      (error "%s does not look like an asciidoc manpage source." (buffer-file-name)))
    (save-buffer)
    (let* ((default-directory (file-name-directory (buffer-file-name)))
           (man (file-name-nondirectory
                 (file-name-sans-extension (buffer-file-name))))
           (target (concat (file-name-directory default-directory) man))
           (process
            (start-process (format "*asciidoc to man %s*" target)
                           target
                           "make" "-k" "-f" cw:doc-mode:makefile man)))
      (process-put process :target target)
      (set-process-sentinel process 'cw:doc-mode:sentinel)))

(defun cw:doc-mode:sentinel (proc change)
  "a simple process sentinel so that we don't display the man page early"
  (when (eq (process-status proc) 'exit)
    (cw:doc-mode:display-man-page (process-get proc :target))))

(defun cw:doc-mode:display-man-page (target)
  (when (get-buffer (concat "*Man " target "*"))
    (kill-buffer (concat "*Man " target "*")))
  (Man-getpage-in-background target))

(when (require 'doc-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . doc-mode))
  (add-hook 'doc-mode-hook 'flyspell-mode)
  (add-hook 'doc-mode-hook 'flyspell-buffer)
  (define-key doc-mode-map (kbd "C-c m") 'cw:doc-mode:compile))


(provide 'chezwam-doc-mode)
