;;; chezwam-adoc.el --- Asciidoc compiler

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2011-03-22
;; Last changed: 2011-03-22 13:46:48
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Make sure to have asciidoc.mak available.


;;; Code:

(require 'man nil t)

(defcustom cw:adoc-mode:makefile 
  "~/.emacs.d/Makefiles/asciidoc.mak"
  "Where to find the asciidoc.mak Makefile")

(defun cw:adoc-mode:compile ()
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
                           "make" "-k" "-f" cw:adoc-mode:makefile man)))
      (process-put process :target target)
      (set-process-sentinel process 'cw:adoc-mode:sentinel)))

(defun cw:adoc-mode:sentinel (proc change)
  "a simple process sentinel so that we don't display the man page early"
  (when (eq (process-status proc) 'exit)
    (cw:adoc-mode:display-man-page (process-get proc :target))))

(defun cw:adoc-mode:display-man-page (target)
  "Show compiled man page"
  (when (get-buffer (concat "*Man " target "*"))
    (kill-buffer (concat "*Man " target "*")))
  (Man-getpage-in-background target))


(provide 'chezwam-adoc)
