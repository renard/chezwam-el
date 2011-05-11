;;; chezwam-dired.el --- Dired configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired
;; Created: 2010-11-19
;; Last changed: 2011-05-11 18:12:03
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
(require 'mailcap)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(define-key dired-mode-map (kbd "<C-return>")
  'gnus-dired-find-file-mailcap)
(add-to-list 'mailcap-mime-extensions '(".mkv" . "video/x-matroska"))
(mailcap-parse-mailcaps nil t)

(defun dired-mplayer (&optional file)
  ""
  (interactive)
  (let* ((file (or file (dired-file-name-at-point)))
	 (file-vec (or (ignore-errors (tramp-dissect-file-name file))
		       (tramp-dissect-file-name (concat "/:" file) 1)))
	 (host (tramp-file-name-real-host file-vec))
	 (user (tramp-file-name-user file-vec))
	 (path (tramp-file-name-localname file-vec))
	 (method (tramp-file-name-method file-vec))
	 (default-directory "~"))
    (if (not host)
	(shell-command (format "mplayer %s" path))
      (shell-command
       (concat "ssh -o StrictHostKeyChecking=no "
	       "-o UserKnownHostsFile=/dev/null "
	       host " cat '" (shell-quote-argument path)
	       "' | mplayer -idx -quiet - &")))))



(defun dired-toogle-sudo ()
  "Open current dired buffer with sudo"
  (interactive)
  (let* ((orig (current-buffer))
	 (file-vec (or (ignore-errors (tramp-dissect-file-name
				       default-directory))
		       (tramp-dissect-file-name
			(concat "/:" default-directory) 1)))
	 (method  (tramp-file-name-method file-vec))
	 (user (tramp-file-name-user file-vec)) 
	 (host  (tramp-file-name-host file-vec))
	 (localname (expand-file-name (tramp-file-name-localname file-vec)))
	 uri)
    (when (string= system-name host)
      (setq host nil))
    (cond
     ;; remote directory -> sudo
     ((and host (string= method "scp"))
      (setq method "sudo" user nil))
     ;; remote directory -> normal
     ((and host (string= method "sudo"))
      (setq method "scp" user nil))
     ;; Local directory -> normal
     ((and (not host) (string= method "scp"))
      (setq method "sudo"))
     ;; Local directory -> sudo
     ((and (not host) (string= method "sudo"))
      (setq method nil user nil))
     ;; Local directory -> normal
     (t
      (setq method "sudo")))
    (setq uri
	  (replace-regexp-in-string  
	   "^/:/" "/"
	   (tramp-make-tramp-file-name method user host localname)))
    (when (eq major-mode 'dired-mode)
      (kill-buffer orig))
    (dired uri)))
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toogle-sudo)


(provide 'chezwam-dired)
