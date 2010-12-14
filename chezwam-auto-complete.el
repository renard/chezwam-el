;;; chezwam-auto-complete.el --- autocomplete configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: Auto complete configuration
;; Created: 2010-10-13
;; Last changed: 2010-12-10 12:47:28
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:

(when (require 'auto-complete-config nil t)
  (setq ac-comphist-file "~/.emacs.d/.tmp/ac-comphist.dat")
  (let ((dict-path  "~/.emacs.d/el-get/auto-complete/dict"))
    (when (file-exists-p dict-path)
      (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
      (ac-config-default)))
  ;; Remove autocomplete for CSS due to a bug
  (remove-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'css-mode-hook '(lambda() auto-complete-mode -1)))

(provide 'chezwam-auto-complete)
