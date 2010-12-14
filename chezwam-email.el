;;; chezwam-email.el --- Fonctions related to email addresses

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-12-14
;; Last changed: 2010-12-14 15:51:29
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 

;;; Code:

(defvar cw:email-replacement-begin "❨" ;"•"
  "Character used to start a replacement (see `cw:encode-email').")

(defvar cw:email-replacement-end "❩" ;"•"
  "Character used to end a replacement (see `cw:encode-email').")

;; Letters could be found such as:
;; a b c d e f g h i j k l m n o p q r s t u v w x y z
;; ɑ ɓ ƈ ɖ ɛ ƒ ɠ ƕ ɩ ʝ ƙ Ɩ ɱ ɲ ɵ Ƥ ƍ Ʀ ʃ Ƭ ʋ ν ω χ ƴ ʒ
(defvar cw:email-lang-replacements
  '(en
    (("\\." "ɖɵʈ")
     ("@" "ɑƬ")
     ("-" "ɱɩɲʋʃ")
     ("_" "ʋɲɖɛƦʃƈɵƦɛ")
     ("\\+" "ƤƖʋʃ"))
    fr
    (("\\." "Ƥɵɩɲʈ")
     ("@" "ɑƦɵϐɑʂɛ")
     ("-" "ʈɩƦɛʈ")
     ("_" "ʋɲɖɛƦʃƈɵƦɛ")
     ("\\+" "ƤƖʋʃ")))
  "Map used to encode email addresses (see `cw:encode-email').")

(defvar cw:email-lang-default "en"
  "Default encoding language (see `cw:encode-email').")

(defun cw:encode-email (&optional lang)
  "Encode an email address in a spamproof format.

If LANG is set, use that language (see
`cw:email-lang-replacements' and `cw:email-lang-default')."
  (interactive)
  (let
      ((address (thing-at-point 'email)))
    (when address
      (let*
	  ((lang (if lang lang "en"))
	   (rpl (plist-get cw:email-lang-replacements 
			   (intern lang)))
	   (bounds (bounds-of-thing-at-point 'email)))
	(while rpl
	  (setq address
		(replace-regexp-in-string
		 (caar rpl)
		 (concat cw:email-replacement-begin
			 (cadar rpl)
			 cw:email-replacement-end)
		 address)
		rpl (cdr rpl)))
	(delete-region (car bounds) (cdr bounds)))
      (insert address))))


(provide 'chezwam-email)
