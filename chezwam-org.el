;;; chezwam-org.el --- Org-mode configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, org, configuration
;; Created: 2010-12-21
;; Last changed: 2011-01-03 10:02:13
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'org-install)

(defun cw:org:org-mode-setup ()
  "Setup buffer for `org-mode' files."
  (unless (and
	   (boundp 'cw:org:publishing-project)
	   cw:org:publishing-project)
    ;; yasnippet
    ;; (make-variable-buffer-local 'yas/trigger-key)
    ;; (setq yas/trigger-key [tab])
    ;; (define-key yas/keymap [tab] 'yas/next-field-group)
    (setq time-stamp-start "^#\\+DATE: ")
    ;; flyspell mode to spell check everywhere
    (flyspell-mode 1)
    ;;(ispell-change-dictionary "francais")
    ))

(defadvice org-publish-projects
  (around cw:org:publish-projects (projects) activate)
  "Set `cw:publishing-project' to `t' when publishing a project."
  (let ((cw:org:publishing-project t))
    ad-do-it))

(when (require 'org nil t)
  (require 'cw-org-publish nil t)
  (add-hook 'org-mode-hook 'cw:org:org-mode-setup))


;; redefine some key mappings in order to use windmove with Org.
(org-defkey org-mode-map [(shift up)]          'windmove-up)
(org-defkey org-mode-map [(shift down)]        'windmove-down)
(org-defkey org-mode-map [(shift left)]        'windmove-left)
(org-defkey org-mode-map [(shift right)]       'windmove-right)
(define-key org-mode-map (kbd "C-x <up>")    'org-shiftup)
(define-key org-mode-map (kbd "C-x <down>")  'org-shiftdown)
(define-key org-mode-map (kbd "C-x <left>")  'org-shiftleft)
(define-key org-mode-map (kbd "C-x <right>") 'org-shiftright)


(provide 'chezwam-org)
