;;; gnus-init.el --- 

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-11-26
;; Last changed: 2011-03-21 10:17:04
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:
(require 'gnus-start)
(require 'gnus-util)
(require 'message)
(require 'gnus-msg)
(require 'gnus-identities)


(defcustom cw:gnus:host-configuration
  '((localhost . (:archive "nnimap+localhost:INBOX" :identity "default")))

  "List of default setting for different hosts."

  :group 'chezwam
  :type 'list)

;; Configure mail sources
(require 'chezwam-gnus-private)


(defvar cw:gnus:configure-group-alist
  '((".*" "american"))
  "Setup for `cw:gnus:configure-group'.

List format:  (REGEXP dictionary)

REGEXP:

   Regular expression to match against the `gnus-newsgroup-name'.

dictionary:

   Language for `ispell'.")

(defun cw:gnus:configure-group ()
  "Configure behaviour for some groups groups."
  (mapc
   (lambda (x)
     (when (string-match (car x) gnus-newsgroup-name)
       (ispell-change-dictionary (cadr x))))
   cw:gnus:configure-group-alist))


(when (require 'sendmail)
  (defadvice mail-envelope-from
    (around cw:gnus:mail-envelope-from activate)
    "Get envelope sender from Sender, Return-path, From fields
or `mail-envelope-from'."
    (setq ad-return-value
	  (or
	   (nth 1 (mail-extract-address-components
		   (or 
		    (message-fetch-field "Sender")
		    (message-fetch-field "Return-path")
		    (message-fetch-field "From"))))
	   mail-envelope-from))))
;; (ad-unadvise 'mail-envelope-from)


(defun cw:gnus:archive-message (current-folder)
  "Post a copy of sent message in current or default folder as
given by `cw:gnus:host-configuration'."
  (cond
   ((string-equal "" current-folder)
    (plist-get
     (cdr (assoc (intern
		  ;; retrieve the hostname
		  (car (split-string (system-name) "[.]" t)))
		 cw:gnus:host-configuration))
     :archive))
   (t current-folder)))



(require 'gnus-art)

(setq message-signature-insert-empty-line t
      gnus-user-date-format-alist
      '(
	((gnus-seconds-today) .           "Today      %H:%M")
	((+ 86400 (gnus-seconds-today)) . "%Y-%m-%d %H:%M")
	(604800 .                         "%Y-%m-%d %H:%M") ;;that's one week
	((gnus-seconds-month) .           "%Y-%m-%d %H:%M")
	((gnus-seconds-year) .            "%Y-%m-%d %H:%M")
	(t .                              "%Y-%m-%d %H:%M"))
      gnus-summary-line-format (concat "%U%R %5,5k %&user-date; %-20,20n %B%s\n")
      ;;gnus-group-line-format "%M%S%p%P%5uy:%B%(%g%)%O\n"
      ;;gnus-group-line-format "%M%S%p%P:%B%(%g%)%O\n"
      gnus-visible-headers (concat gnus-visible-headers
				   "\\|^User-Agent:\\|^X-Mailer:")
      gnus-sum-thread-tree-false-root " ♽ "
      gnus-sum-thread-tree-single-indent "⚙ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "⚈ "
      gnus-sum-thread-tree-leaf-with-other "├─►"  ; "┣━► "  "▶"
      gnus-sum-thread-tree-single-leaf     "└─►"  ; "┗━► "
      gnus-sum-thread-tree-vertical        "│"   ; "┆" "┋")  "│" "┆"
      )

(gnus-add-configuration
 '(article
   (horizontal 8
               (group 50)
               (vertical 1.0
                         (summary 20 point)
                         (article 1.0)))))
(gnus-add-configuration
 '(summary
   (vertical 1.0
             (horizontal 1.0
                         (group 50)
                         (summary 1.0 point)
                         (if gnus-carpal
                             '(summary-carpal 4))))))


(defadvice gnus-summary-scroll-up
  (around cw:gnus-article-scroll-up activate)
  (ignore-errors
    (gnus-summary-display-buttonized)
    ad-do-it))

(setq
 mail-envelope-from 'header
 message-from-style 'angles
 gnus-message-archive-group 'cw:gnus:archive-message
 gnus-gcc-mark-as-read t
 mail-specify-envelope-from t
 message-send-mail-function 'smtpmail-send-it
 smtpmail-default-smtp-server "127.0.0.1"
 gnus-article-update-date-headers nil)



(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'message-mode-hook 'cw:gnus:configure-group)
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'flyspell-buffer)

(provide 'gnus-init)
