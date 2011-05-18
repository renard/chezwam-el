;;; chezwam-escreen.el --- Escreen configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2011-03-22
;; Last changed: 2011-05-11 23:32:53
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Make sure to have asciidoc.mak available.


;;; Code:

(require 'escreen)
(escreen-install)


;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (emphased ""))

    (dolist (s escreens)
      (setq emphased
            (concat emphased (if (= escreen-current-screen-number s)
                                 (propertize (number-to-string s)
                                             ;;'face 'custom-variable-tag) " ")
                                             ;; 'face 'info-title-3)
                                             'face 'font-lock-warning-face)
                               (number-to-string s))
                    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)


;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)


;;
;; We want the last/prev/next escreen function to show the list with
;; emphasis
;;
(defadvice escreen-goto-last-screen
  (after cw:escreen:goto-last-screen activate)
  "Show the escreen list each time we go to last screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-prev-screen
  (after cw:escreen:goto-prev-screen activate)
  "Show the escreen list each time we go to previous screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-next-screen
  (after cw:escreen:goto-next-screen activate)
  "Show the escreen list each time we go to next screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-create-screen
  (after cw:escreen:create-screen activate)
  "Show the escreen list each time we create a new screen."
  (escreen-get-active-screen-numbers-with-emphasis))


(global-set-key (kbd "M-[") 'escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'escreen-goto-next-screen)




(provide 'chezwam-escreen)
