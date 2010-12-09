;;; chezwam-macros.el --- Some macros shamelessly stolen from dim ;)

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2010-12-09 11:08:33
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(defun lsb-release (&optional property)
  "Parse lsb-release output and return an alist, or the value for the given property"
  (when (file-executable-p "/usr/bin/lsb_release")
    (let* ((lsbr (shell-command-to-string "/usr/bin/lsb_release -a 2>/dev/null"))
           (props (split-string lsbr "[:\n]" t))
           (kv))
      (while (>= (length props) 2)
        ;; Don't keep extra spaces. This way seems like the easy one in elisp.
        (let ((key (mapconcat 'identity (split-string (car props)) " "))
              (val (mapconcat 'identity (split-string (cadr props)) " ")))
          (setq kv (add-to-list 'kv (cons key val)))
          (setq props (cddr props))))
      (if property
          (cdr (assoc property (lsb-release)))
        kv))))

;; thanks to ams on #emacs on irc.freenode.net
(defmacro with-window-system (&rest body)
  "eval body only when running an windowed version of Emacs"
  `(when window-system ,@body))

(defmacro without-window-system (&rest body)
  "eval body only when running a console Emacs"
  `(unless window-system ,@body))

;; variations on the theme
(defmacro when-running-debian (&rest body)
  "eval body only when running under debian"
  ;; FIXME: check "lsb_release -a" output on debian/kFreeBSD
  `(when (equal (lsb-release "Distributor ID") "Debian") ,@body))


(provide 'chezwam-macros)
