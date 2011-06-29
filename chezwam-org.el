;;; chezwam-org.el --- Org-mode configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, org, configuration
;; Created: 2010-12-21
;; Last changed: 2011-06-29 23:52:51
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:

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

    ;; redefine some key mappings in order to use windmove with Org.
    (org-defkey org-mode-map [(shift up)]          'windmove-up)
    (org-defkey org-mode-map [(shift down)]        'windmove-down)
    (org-defkey org-mode-map [(shift left)]        'windmove-left)
    (org-defkey org-mode-map [(shift right)]       'windmove-right)
    (define-key org-mode-map (kbd "C-x <up>")    'org-shiftup)
    (define-key org-mode-map (kbd "C-x <down>")  'org-shiftdown)
    (define-key org-mode-map (kbd "C-x <left>")  'org-shiftleft)
    (define-key org-mode-map (kbd "C-x <right>") 'org-shiftright)

    ;; redefine some key mappings in order to use windmove with Org-calendar.
    (org-defkey org-agenda-mode-map [(shift up)]          'windmove-up)
    (org-defkey org-agenda-mode-map [(shift down)]        'windmove-down)
    (org-defkey org-agenda-mode-map [(shift left)]        'windmove-left)
    (org-defkey org-agenda-mode-map [(shift right)]       'windmove-right)
    (define-key org-agenda-mode-map (kbd "C-x <up>")    'org-agenda-priority-up)
    (define-key org-agenda-mode-map (kbd "C-x <down>")  'org-agenda-priority-down)
    (define-key org-agenda-mode-map (kbd "C-x <left>")  'org-agenda-do-date-earlier)
    (define-key org-agenda-mode-map (kbd "C-x <right>") 'org-agenda-do-date-later)

    ))

(defadvice org-publish-projects
  (around cw:org:publish-projects (projects) activate)
  "Set `cw:publishing-project' to `t' when publishing a project."
  (let ((cw:org:publishing-project t))
    ad-do-it))

(require 'cw-org-publish nil t)
(add-hook 'org-mode-hook 'cw:org:org-mode-setup)


(defun cw:org:toggle-encryption()
  "Toggle encryption in for current entry."
  (interactive)
  (org-crypt-use-before-save-magic)
  (save-excursion
    (org-back-to-heading t)
    (next-line)
    (if (looking-at "-----BEGIN PGP MESSAGE-----")
	(org-decrypt-entry)
      (org-encrypt-entry))))
(define-key org-mode-map (kbd "C-c C-x C-e") 'cw:org:toggle-encryption)

(defadvice org-encrypt-entry
  (around cw:org-encrypt-entry activate)
  (org-back-to-heading t)
  (show-subtree)
  ad-do-it
  (org-back-to-heading t)
  (org-set-tags-to (delete "CLEAR" (org-get-tags)))
  (hide-entry))

(defadvice org-decrypt-entry
  (around cw:org-decrypt-entry activate)
  (org-back-to-heading t)
  (show-subtree)
  ad-do-it
  (org-back-to-heading t)
  (hide-subtree)
  (let ((tags-list (org-get-tags)))
    (when (member org-crypt-tag-matcher tags-list)
      (org-set-tags-to (append '("CLEAR") tags-list))
      (hide-entry)
      (show-children 3))))

(setq org-agenda-files '("~/.emacs.d/org/agenda.org"
			 "~/.emacs.d/org/todo.org"
			 "~/.emacs.d/org/diary.org"
			 "~/.emacs.d/org/rentabiliweb.org"
			 "~/.emacs.d/org/refile.org"))


(add-to-list 'org-link-escape-chars '(?\311 . "%C9")) ; É

(provide 'chezwam-org)

