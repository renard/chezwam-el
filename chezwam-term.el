;;; chezwam-term.el --- terminal customisation

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration, term
;; Created: 2010-12-16
;; Last changed: 2011-07-04 14:49:53
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:

(defun cw:term:handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     (lambda (proc change)
       (when (eq (process-status proc) 'exit)
         (kill-buffer (process-buffer proc)))))))

(defun cw:term:set-font()
  "Set default font for terminal."
  (set
   (make-local-variable 'buffer-face-mode-face)
   '(:family "Terminus" :height 120))
  (buffer-face-mode))
;;(remove-hook 'term-mode-hook 'use-hard-newlines)

(when (require 'term nil t)
  (setq term-default-bg-color 'unspecified)
  (setq term-default-fg-color "#eeeeec")
  (setq  ansi-term-color-vector
	 [unspecified "black" "#f57900" "#8ae234" "#edd400" "#729fcf"
		      "#ad7fa8" "cyan3" "#eeeeec"])
  
  (add-hook 'term-mode-hook 'cw:term:handle-close )
  (add-hook 'term-mode-hook (lambda () (hl-line-mode -1)))
  (add-hook 'term-mode-hook 'cw:term:set-font)
  (add-hook 'term-mode-hook 'use-hard-newlines))

(require 'cssh)

(defun cw:term:run()
  "Run an ansi-term in the directory of current buffer or current
directory."
  (interactive)

  (let* ((current-buffer-dir
	  ;; get directory name from either dired or buffer file name and
	  ;; fall back to nil
	  (or (ignore-errors (dired-current-directory))
	      (ignore-errors (file-name-directory (buffer-file-name)))
	      nil))
	 (file-vector
	  ;; get a tramp usable file URI from directory.
	  (or (ignore-errors (tramp-dissect-file-name current-buffer-dir))
	      (tramp-dissect-file-name (concat "/:" current-buffer-dir) 1)))
	 ;; split file URI into its components
	 (localname (tramp-file-name-localname file-vector))
	 (method (tramp-file-name-method file-vector))
	 (host (tramp-file-name-host file-vector))
	 ;; get user shell (taken from term.el)
	 (shell  (or explicit-shell-file-name
		     (getenv "ESHELL")
		     (getenv "SHELL")
		     "/bin/sh"))
	 (shell-nd (file-name-nondirectory shell)))
    (cond
     ;; Log into remote host, enable remote directory tracking and change to
     ;; visited directory.
     ((and (stringp host) (not (string= host (system-name))))
      (cssh-term-create host)

	;; go to visited directory
	(insert (concat
		 "cd \"" localname "\";"
		 "history -d $((HISTCMD - 1));"))
	(term-send-input))
     ;; go to visited directory for localhost
     ((stringp localname)
      (ansi-term shell)
      (insert 
       (concat 
	(format "cd \"%s\"; " localname)
	(when (string= shell-nd "bash") "history -d $((HISTCMD - 1));")))
      (term-send-input))
     ;; by default only open a shell
     (t
      (ansi-term shell)))))


(defadvice shell
  (after cw:shell activate)
  (when (ignore-errors (get-buffer-process ad-return-value))
    (set-process-sentinel
     (get-buffer-process ad-return-value)
     (lambda (proc change)
       (when (eq (process-status proc) 'exit)
         (kill-buffer (process-buffer proc)))))))


(defmacro cw:term:with-parse-directory (sudo &rest body)
  "Execute BODY with a declaration of following variables after
setting `default-directory' to the directory of file file visited
in current buffer.

If SUDO is not nil `method' is set to \"sudo\" and `user' to
\"root\".
"
  `(let* ((sudo ,sudo)
	  (current-buffer-dir
	   ;; get directory name from either dired or buffer file name and
	   ;; fall back to nil
	   (or (ignore-errors (dired-current-directory))
	       (ignore-errors (file-name-directory (buffer-file-name)))
	       "~"))
	  (file-vector
	   ;; get a tramp usable file URI from directory.
	   (or (ignore-errors (tramp-dissect-file-name current-buffer-dir))
	       (tramp-dissect-file-name (concat "/:" current-buffer-dir) 1)))
	  ;; split file URI into its components
	  (method (if ,sudo "sudo" (tramp-file-name-method file-vector)))
	  (user (if ,sudo "root" (tramp-file-name-user file-vector)))
	  (localname (tramp-file-name-localname file-vector))
	  (host (tramp-file-name-host file-vector))
	  (default-directory
	    ;; If no method is defined then the file is local
	    ;; then don't use tramp.
	    (if method
		(tramp-make-tramp-file-name method user host localname)
	      localname)))
     ,@body))

(defun cw:term:shell (&optional sudo)
  "Run terminal in current buffer directory."
  (interactive "P")
  (cw:term:with-parse-directory sudo (progn (shell))))

(defun cw:term:toggle-line-mode()
  "Toogle between line and char mode in term-mode."
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (end-of-buffer)
    (term-char-mode)))

(defadvice global-hl-line-highlight
  (around cw:global-hl-line-highlight activate)
  "Disable hl-line-highlight in `term-char-mode'."
  (unless (and (eq major-mode 'term-mode) (term-in-char-mode))
    ad-do-it))

(global-set-key (kbd "C-x <C-return>") 'cw:term:shell)
(global-set-key (kbd "C-x <S-return>") 'cw:term:run)

(defun cw:term:backward-word ()
  "Move backward work in term-mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun cw:term:forward-word ()
  "Move forward work in term-mode."
  (interactive)
  (term-send-raw-string "\ef"))

(define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)
(define-key term-raw-map (kbd "M-x") 'execute-extended-command)
(define-key term-raw-map (kbd "C-c C-b")  'ibuffer)
(define-key term-raw-map (kbd "C-y")  'term-paste)
(define-key term-raw-map (kbd "<C-right>")  'cw:term:forward-word)
(define-key term-raw-map (kbd "<C-left>")  'cw:term:backward-word)
(define-key term-raw-map (kbd "C-c C-'")  'cw:term:toggle-line-mode)
(define-key term-mode-map (kbd "C-c C-'")  'cw:term:toggle-line-mode)
(define-key term-raw-map [mouse-2] 'term-mouse-paste)


;;(info "(emacs) Mouse Avoidance")

(provide 'chezwam-term)
