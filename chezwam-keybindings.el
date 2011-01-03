;;; chezwam-keybindings.el --- Special key bindings

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-10-13
;; Last changed: 2010-10-13 14:24:45
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(define-key global-map (kbd "C-x C-f") 'find-file-at-point)
(define-key global-map (kbd "C-z") 'undo)

;; scroll without moving point
(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (scroll-down 1) (forward-line -1)))
(global-set-key (kbd "M-S-<up>")
                (lambda () (interactive) (scroll-down 10) (forward-line -10)))

(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (scroll-up 1) (forward-line 1)))
(global-set-key (kbd "M-S-<down>")
                (lambda () (interactive) (scroll-up 10) (forward-line 10)))

;; http://www.emacswiki.org/emacs/ParenthesisMatching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)



(provide 'chezwam-keybindings)
