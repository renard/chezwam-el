;;; chezwam-tramp.el --- tramp customisation

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-10-12
;; Last changed: 2011-05-19 01:15:25
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'tramp)
(require 'tramp-sh)

(defun cw:tramp:reload-tramp-compute-multi-hops()
  "Reload `tramp-compute-multi-hops' to make `cw:tramp-error'
advice work. WHY ????"
  (find-library "tramp-sh")
  (find-function 'tramp-compute-multi-hops)
  (forward-sexp)
  (eval-last-sexp nil)
  (kill-buffer "tramp-sh.el.gz")) 

;; Need to install emacs-snapshot-el
(eval-after-load "tramp"
  (progn
    (cw:tramp:reload-tramp-compute-multi-hops)
    (message "tramp-compute-multi-hops reloaded")))
 

(defadvice tramp-error
  (around cw:tramp-error activate)
  "Allow to use sudo on a remote host:
/sudo:x@y:z ==> /multi:sshx:y:sudo:z@y:z

Based on TWB hack (http://paste.lisp.org/display/90780)."
  ;;(message (format "TRAMP-ERROR(%s %s)" vec-or-proc signal))
  (if (and (eq 'file-error signal)
	   (string= "sudo" (tramp-file-name-method vec-or-proc)))
      (progn
	;;(message (format "target-alist: %s" target-alist))
	(setq target-alist
	      (cons (vector "sshx" ""
			    (tramp-file-name-host vec-or-proc)
			    "")
		    (list (vector (tramp-file-name-method vec-or-proc)
				  (unless (string= "root" (tramp-file-name-user vec-or-proc))
				    (tramp-file-name-user vec-or-proc))
				  (tramp-file-name-host vec-or-proc)
				  (tramp-file-name-localname vec-or-proc))))))
    ad-do-it))

(setq
 tramp-default-method "scp"
 tramp-terminal-type "screen"
 tramp-backup-directory-alist backup-directory-alist)



(provide 'chezwam-tramp)
