;;; chezwam-tramp.el --- Cssh customisation

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: 
;; Created: 2010-10-12
;; Last changed: 2010-12-03 11:41:13
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'tramp-sh)

;; (defadvice tramp-compute-multi-hops
;;   (around cw:tramp-compute-multi-hops activate)
(defun  tramp-compute-multi-hops (vec)
  "Expands VEC according to `tramp-default-proxies-alist'.
Gateway hops are already opened."
  
  (let ((target-alist `(,vec))
	 (choices tramp-default-proxies-alist)
	 item proxy)

    ;; Look for proxy hosts to be passed.
    (while choices
      (setq item (pop choices)
	    proxy (eval (nth 2 item)))
      (when (and
	     ;; host
	     (string-match (or (eval (nth 0 item)) "")
			   (or (tramp-file-name-host (car target-alist)) ""))
	     ;; user
	     (string-match (or (eval (nth 1 item)) "")
			   (or (tramp-file-name-user (car target-alist)) "")))
	(if (null proxy)
	    ;; No more hops needed.
	    (setq choices nil)
	  ;; Replace placeholders.
	  (setq proxy
		(format-spec
		 proxy
		 (format-spec-make
		  ?u (or (tramp-file-name-user (car target-alist)) "")
		  ?h (or (tramp-file-name-host (car target-alist)) ""))))
	  (with-parsed-tramp-file-name proxy l
	    ;; Add the hop.
	    (add-to-list 'target-alist l)
	    ;; Start next search.
	    (setq choices tramp-default-proxies-alist)))))

    ;; Handle gateways.
    (when (string-match
	   (format
	    "^\\(%s\\|%s\\)$" tramp-gw-tunnel-method tramp-gw-socks-method)
	   (tramp-file-name-method (car target-alist)))
      (let ((gw (pop target-alist))
	    (hop (pop target-alist)))
	;; Is the method prepared for gateways?
	(unless (tramp-get-method-parameter
		 (tramp-file-name-method hop) 'tramp-default-port)
	  (tramp-error
	   vec 'file-error
	   "Method `%s' is not supported for gateway access."
	   (tramp-file-name-method hop)))
	;; Add default port if needed.
	(unless
	    (string-match
	     tramp-host-with-port-regexp (tramp-file-name-host hop))
	  (aset hop 2
		(concat
		 (tramp-file-name-host hop) tramp-prefix-port-format
		 (number-to-string
		  (tramp-get-method-parameter
		   (tramp-file-name-method hop) 'tramp-default-port)))))
	;; Open the gateway connection.
	(add-to-list
	 'target-alist
	 (vector
	  (tramp-file-name-method hop) (tramp-file-name-user hop)
	  (tramp-compat-funcall 'tramp-gw-open-connection vec gw hop) nil))
	;; For the password prompt, we need the correct values.
	;; Therefore, we must remember the gateway vector.  But we
	;; cannot do it as connection property, because it shouldn't
	;; be persistent.  And we have no started process yet either.
	(tramp-set-file-property (car target-alist) "" "gateway" hop)))

    ;; Foreign and out-of-band methods are not supported for multi-hops.
    (when (cdr target-alist)
      (setq choices target-alist)
      (while choices
	(setq item (pop choices))
	(when
	    (or
	     (not
	      (tramp-get-method-parameter
	       (tramp-file-name-method item) 'tramp-login-program))
	     (tramp-get-method-parameter
	      (tramp-file-name-method item) 'tramp-copy-program))
	  (tramp-error
	   vec 'file-error
	   "Method `%s' is not supported for multi-hops."
	   (tramp-file-name-method item)))))

    ;; In case the host name is not used for the remote shell
    ;; command, the user could be misguided by applying a random
    ;; hostname.
    (let* ((v (car target-alist))
	   (method (tramp-file-name-method v))
	   (host (tramp-file-name-host v)))
      (unless
	  (or
	   ;; There are multi-hops.
	   (cdr target-alist)
	   ;; The host name is used for the remote shell command.
	   (member
	    '("%h") (tramp-get-method-parameter method 'tramp-login-args))
	   ;; The host is local.  We cannot use `tramp-local-host-p'
	   ;; here, because it opens a connection as well.
	   (string-match tramp-local-host-regexp host))
	;; (tramp-error
	;;  v 'file-error
	;;  "Host `%s' looks like a remote host, `%s' can only use the local host"
	;;  host method)))

	;; THIS IS THE PART TWB ADDED
	;; /sudo:x@y:z ==> /multi:sshx:x@y:sudo:root@y:z
	(setq target-alist
	      (cons (vector "sshx"
			    (unless (string= "root" (tramp-file-name-user v))
			      (tramp-file-name-user v))
			    (tramp-file-name-host v)
			    "")
		    (cons (vector (tramp-file-name-method v)
				  "root"
				  (tramp-file-name-host v)
				  (tramp-file-name-localname v))
			  (cdr target-alist)))))) ; END OF THE PART TWB ADDED
    ;; Result.
    target-alist))
;;(ad-unadvise 'tramp-compute-multi-hops)



(setq
 tramp-default-method "scp"
 tramp-terminal-type "screen"
 tramp-backup-directory-alist backup-directory-alist)


(provide 'chezwam-tramp)
