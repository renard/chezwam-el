;;; cw-visual.el --- Set visual effects for Emacs

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2010-12-10 00:10:15
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

(require 'chezwam-macros)

;; disable init message
(setq inhibit-startup-message t)

;; remove menus
(menu-bar-mode -1)
(tool-bar-mode -1)

;; and scroll bars
(scroll-bar-mode nil)
(set-scroll-bar-mode nil)

;; mark curent line
(with-window-system
 (global-hl-line-mode 1))

(transient-mark-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; parentheses mode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; line / columns numbers
(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 76)

;; browser
(setq
 browse-url-generic-program "conkeror"
 browse-url-browser-function 'browse-url-generic)



(provide 'chezwam-emacs)
