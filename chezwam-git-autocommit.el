;;; chezwam-git-autocommit.el --- Git auto commit directory.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2011-06-28
;; Last changed: 2011-06-28 21:47:10
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


(defvar cw:ac-dir-set '()
  "Set of git repositories to auto-commit using
  `cw:ac-commit-file'.")

(defun cw:ac-match-filep (f)
  "Test if file F is in a subdirectory og `cw:ac-dir-set'."
  (car
   (remove 'nil
	   (mapcar
	    (lambda(x)
	      (setq x (abbreviate-file-name
		       (file-name-as-directory x)))
	      (when
		  (and
		   (file-directory-p (concat x ".git"))
		   (string-match x (substring f 0 (length x))))
		x))
	    cw:ac-dir-set))))

(defun cw:ac-schedule-push (dn)
  "Schedule a push if one is not already scheduled for the given dir."
  (if (null (member dn cw:ac-dir-set))
      (progn
	(run-with-idle-timer
	 10 nil
	 (lambda (dn)
	   (message (concat "Pushing git repository from  " dn))
	   (let ((default-directory dn))
	     (shell-command ("git push & /usr/bin/true"))))
	 dn))))

(defun cw:ac-commit-file ()
  "Commit file visited in current buffer."
  (interactive)
  (let* ((fn (abbreviate-file-name (buffer-file-name)))
	 (dn (cw:ac-match-filep fn))
	 rn)
    (when dn
      (message "git adding %s" fn)
      (setq  rn (file-relative-name fn dn))
      (let ((default-directory dn))
	(shell-command (concat "git add " rn))
	(shell-command (format 
			"git commit -m 'Auto commit %s.'" rn)))
      (cw:ac-schedule-push dn))))

(add-hook 'after-save-hook 'cw:ac-commit-file t nil)
;;(remove-hook 'after-save-hook 'cw:ac-commit-file)

(provide 'chezwam-git-autocommit)
