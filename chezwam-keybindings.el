;;; chezwam-keybindings.el --- Special key bindings

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-10-13
;; Last changed: 2011-06-28 18:25:52
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

;;(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

(ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-use-url-at-point t)
(setq ido-auto-merge-delay-time 2.00)

(define-key global-map (kbd "C-x C-b") 'ido-switch-buffer)
(define-key global-map (kbd "C-x b") 'ido-switch-buffer)
(define-key global-map (kbd "C-x B") 'ibuffer)


(defun cw:get-buffer-visiting-file-or-directory-list()
  ""
  (mapcar 'abbreviate-file-name
	  (remove 'nil (mapcar
			(lambda(x)
			  (set-buffer x)
			  (if (eq major-mode 'dired-mode)
			      default-directory
			    (buffer-file-name)))
			(buffer-list)))))


(defun cw:get-buffer-visiting-directory-list()
  ""
  (mapcar 'abbreviate-file-name
	  (remove 'nil (mapcar
			(lambda(x)
			  (set-buffer x)
			  (when (eq major-mode 'dired-mode)
			      default-directory))
			(buffer-list)))))

(defun cw:get-buffer-visiting-file-list()
  ""
  (mapcar 'abbreviate-file-name
	  (delete-duplicates
	   (remove 'nil (mapcar
			 (lambda(x)
			   (set-buffer x)
			   (unless (eq major-mode 'dired-mode)
			     (buffer-file-name)))
			 (buffer-list))))))



(setq cw:switch-to-buffer-visiting-file-or-directory-list nil)
(defun cw:switch-to-buffer-visiting-file-or-directory (&optional filename)
  "Switch to buffer visiting FILENAME."
  (interactive)
  (save-current-buffer
    (let ((filename (or filename 
			(completing-read
			 "Switch to buffer visiting: "
			 (cw:get-buffer-visiting-file-or-directory-list)
			 t t nil 'cw:switch-to-buffer-visiting-file-or-directorylist nil t))))
      (when filename
	(find-file filename)))))



(setq cw:switch-to-buffer-visiting-directory-list nil)
(defun cw:switch-to-buffer-visiting-directory (&optional filename)
  "Switch to buffer visiting FILENAME."
  (interactive)
  (save-current-buffer
    (let ((filename (or filename 
			(completing-read
			 "Switch to buffer visiting (dir): "
			 (cw:get-buffer-visiting-directory-list)
			 t t nil 'cw:switch-to-buffer-visiting-directory-list nil t))))
      (when filename
	(find-file filename)))))



(setq cw:switch-to-buffer-visiting-file-list nil)
(defun cw:switch-to-buffer-visiting-file (&optional filename)
  "Switch to buffer visiting FILENAME."
  (interactive)
  (save-current-buffer
    (let ((filename (or filename 
			(completing-read
			 "Switch to buffer visiting (file): "
			 (cw:get-buffer-visiting-file-list)
			 t t nil 'cw:switch-to-buffer-visiting-file-list nil t))))
      (when filename
	(find-file filename)))))


(define-prefix-command 'cw:switch-to-buffer-map)
(global-set-key (kbd "C-x C-c") 'cw:switch-to-buffer-map)
(define-key cw:switch-to-buffer-map (kbd "C-c") 'cw:switch-to-buffer-visiting-file-or-directory)
(define-key cw:switch-to-buffer-map (kbd "C-d") 'cw:switch-to-buffer-visiting-directory)
(define-key cw:switch-to-buffer-map (kbd "C-f") 'cw:switch-to-buffer-visiting-file)



;;; ; Do not quit Emacs. Use M-x kill-emacs instead
;; (define-key global-map (kbd "C-x C-c C-c") 'cw:switch-to-buffer-visiting-file-or-directory)
;; (define-key global-map (kbd "C-x C-c C-d") 'cw:switch-to-buffer-visiting-directory)
;; (define-key global-map (kbd "C-x C-c C-f") 'cw:switch-to-buffer-visiting-file)

(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

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


(defun cw:gnus ()
  "Switch to gnus buffer or run `gnus'."
  (interactive)
  (if (buffer-live-p "*Group*")
      (switch-to-buffer"*Group*")
    (gnus)))
(global-set-key (kbd "<C-f1>") 'cw:gnus)


;; user defined completing-read-function entered in emacs24
(when (boundp 'completing-read-function)
   (defun ido-completing-read* (prompt choices &optional predicate require-match
                                       initial-input hist def inherit-input-method)
     "Adjust arguments when it's necessary"
     (if (and (listp choices) (not (functionp choices)))
         (ido-completing-read
          prompt
          (mapcar (lambda (c) (if (listp c) (car c) c)) choices)
          predicate require-match initial-input hist def inherit-input-method)
       (completing-read-default prompt choices predicate require-match
                                initial-input hist def inherit-input-method)))
   (setq completing-read-function 'ido-completing-read*))


(provide 'chezwam-keybindings)
