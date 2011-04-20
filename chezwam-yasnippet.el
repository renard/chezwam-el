;;; chezwam-yasnippet.el --- YASnippet customisation.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, yasnippet, configuration
;; Created: 2010-12-09
;; Last changed: 2011-04-20 12:30:55
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'yasnippet)

(defcustom cw:yasnippet:new-file-template "new-file-tpl"
  "Yasnippet template to use when creating a new file using
`cw:yasnippet:insert-snippet-new-file'"
  :type 'string
  :group 'chezwam)

(defun cw:yasnippet:insert-snippet-new-file ()
  "Insert a default file skeleton when a new file is created as defined
by `cw:yasnippet:new-file-template'.

To make this tip work, a \"NEWFILE-TEMPLATE.yasnippet\" file
should contain the default file skeleton."
  (interactive)
  (when (and (buffer-file-name)
	     (not (file-exists-p (buffer-file-name)))
	     (= (point-max) 1))
    (insert cw:yasnippet:new-file-template)
    (yas/expand)))

;; workarround for yasnippet to act as a skeleton creator.
(setq-default yas/dont-activate
	      #'(lambda ()
		  (and yas/snippet-dirs
		       (null (yas/get-snippet-tables)))))
(setq yas/trigger-key "M-TAB")
(setq yas/snippet-dirs "~/.emacs.d/templates")
(setq yas/prompt-functions '(yas/completing-prompt))
(yas/initialize)
(yas/load-directory yas/root-directory)
(add-hook 'yas/minor-mode-hook 'cw:yasnippet:insert-snippet-new-file)

(provide 'chezwam-yasnippet)
