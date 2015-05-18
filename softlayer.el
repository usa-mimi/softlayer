;;; yuba.el --- elisp for SoftLayer

;; Copyright (C) 2014 by Shuichi Yukimoto

;; Author: yukimoto <yukimoto@usa-mimi.jp>
;; URL: xxxxxxxxx
;; Version: 0.01
;; Package-Requires: ((request.el "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;softlayer.el is a SoftLayer api library ....
;;....

;;; Code:

(provide 'softlayer)

;;

;;version
(defconst sl-version-number "0.1"
  "Version number for this version of Sl.")
(defconst sl-version (format "Sl version %s" sl-version-number)
  "Version string for this version of Sl.")

(defun sl-version-show ()
  "Show sl-version in minibuffer."
  (interactive)
  (message "%s" sl-version))


(require 'url)
(require 'json)
(require 'request)


;;;
;;;大域変数
;;;
(defvar *width* 10)
(defvar *cnt* 0)


(defun softlayer ()
  (interactive)
  (switch-to-buffer "*SoftLayer*")
  (message "%s" sl-version)
  (kill-all-local-variables)
  (setq mode-name "SoftLayer")
  (setq major-mode 'softlayer)
  (setq my-local-map (make-keymap))
  (define-key my-local-map "vl" 'vm-list)
  (define-key my-local-map "ul" 'user-list)
  (define-key my-local-map "\C-m" 'test-click)
  (use-local-map my-local-map)
  (run-hooks 'softlayer-hook))


;;;function
(defun vm-list()
  (interactive)
  (get-vm-account-list)
  )



;;;;
;;SoftLayer api function
;;;;

;;TODO:検索機能を追加
;;TODO ユーザ画面を作成
;;TODO vm作成コマンド実装
;;TODO ObjectStorage関連機能の実装
;;TODO APIコマンドTEST用コマンド実装

;;test

;;(defvar services '("SoftLayer_Account" "getCurrentUser"))
;;(defvar params '("objectMask" . "accountId;id"))
;;(get-sl-list services params)

;;;

(require 'cl)

;;;;
(defvar *hash*)
(defvar *width-list* '())
(defvar *header*)
;;;;;;;;;;
;;
;;
;;  sample for call api
;;  (get-sl-list services params)
;;
;;
(defun get-sl-list (services params)
  "call for SoftLayer API"
  (request
   (make-url (make-url-base (car services) (car (cdr services))))
   :params (list params)
   :parser 'json-read
   :success (function*
	     (lambda (&key data &allow-other-keys)
	       (set-buffer "*SoftLayer*")
	       (make-result-list data params)
	       (make-split-line *width-list*)
	       (erase-buffer)
	       (insert
		   (apply #'format
			  (concat
			   (mapconcat '(lambda (a)
					 (concat "%-"
						 (number-to-string (+ 1 a))))
				      *width-list* "s ") "s\n")
			  (sprit-param params)))
	       (insert
		(apply #'format
		       (concat
			(mapconcat '(lambda (a)
				      (concat "%-"
					      (number-to-string (+ 1 a))))
				   *width-list* "s ") "s\n")
		       *header*))
	       (mapc
		(lambda (i)
		  (insert
		   (apply #'format
			  (concat
			   (mapconcat '(lambda (a)
					 (concat "%-"
						 (number-to-string (+ 1 a))))
				      *width-list* "s ") "s\n")
			   (mapcar
			    (lambda (x)
			      (assoc-default (intern x) i))
			    (sprit-param params)
			    )
			  )
		   ))
		data
		)
	       (message "%s" "Success")
	       ))))


(defun make-split-line (list)
  (setq *header* nil)
  (mapc
   (lambda (x)
     (setq *header* (append *header* (list (make-string x ?-)))))
   list)
  )


(defun make-result-list(result params)
  (setq ht (make-hash-table :test 'equal))
  (mapc
   (lambda (j)
     (puthash j (length j) ht)
     )
   (sprit-param params))
  (mapc
   (lambda (i)
     (mapcar
      (lambda (x)
	(if (< (gethash x ht) (length (check-for-string (assoc-default (intern x) i))))

	    (puthash x (length (check-for-string (assoc-default (intern x) i))) ht)
	  )
	)
      (sprit-param params)
      ))
   result)
  (make-width-list (sprit-param params) ht)
  )

(defun make-width-list (param hash)
  (setq *width-list* '())
  (mapc
   (lambda (x)
     (setq *width-list* (append *width-list* (list (gethash x hash))))
     )
   param)
  )

;;;;
(defun check-for-string (data)
  (cond ((stringp data)
	 data)
	((numberp data)
	 (number-to-string data))
	((listp data)
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
	 )
	)
;;;
(defun get-vm-account-list ()
  "vm list vew"
  (request
    (make-url (make-url-base "SoftLayer_Account" "getVirtualGuests"))
 :params '(("objectMask" . "id;uuid;fullyQualifiedDomainName;primaryIpAddress;primaryBackendIpAddress;datacenter"))
 :parser 'json-read
 :success (function*
           (lambda (&key data &allow-other-keys)
	     (setq test data)
	     (erase-buffer)
	     (setq truncate-lines t)

	     (insert (format
		      "%-10s %-30s %-15s %-15s %-10s\n" "id" "hostname" "privert ip" "public ip" "datacenter"
		      ))
	     (insert
	      (format "%-10s %-30s %-15s %-15s %-10s\n"
		      (make-string 10 ?-)
		      (make-string 30 ?-)
		      (make-string 15 ?-)
		      (make-string 15 ?-)
		      (make-string 10 ?-)
		      ))

	     (mapc
	      (lambda (i)
		(insert (format "%-10s %-30s %-15s %-15s %s\n"
				(assoc-default 'id i)
				(assoc-default 'fullyQualifiedDomainName i )
				(assoc-default 'primaryIpAddress i )
				(assoc-default 'primaryBackendIpAddress i )
				(assoc-default 'name (assoc-default 'datacenter i ))
				)))
	      data)
	     ;;bafferのクリア
	     (set-buffer "*sl-detail*")
	     (erase-buffer)
	     (message "%s" "Success")
	     ))))




(defun get-vm-detail (id buffer)
  "vm list detail view"
  (request
   (make-url-id (make-url-base "SoftLayer_Virtual_Guest" "getObject") (string-to-number id))
 :params '(("objectMask" . "operatingSystem.passwords.username;operatingSystem.passwords.password"))
 :parser 'json-read
 :success (function*
           (lambda (&key data &allow-other-keys)
	     (setq *width* (get-max-length data))
	     (set-buffer "*sl-detail*")
	     (erase-buffer)
	     (insert (format
		     (concat (concat "%-" (number-to-string *width*)) "s : %s\n")
			     "password:"
			     (assoc-default
			      'password
			      (aref
			       (assoc-default
				'passwords
				(assoc-default
				 'operatingSystem
				 data))
			       0 ))))
	     (insert (format
		      (concat (concat "%-" (number-to-string *width*)) "s : %s\n")
			     "username:"
			     (assoc-default
			      'username
			      (aref
			       (assoc-default
				'passwords
				(assoc-default
				 'operatingSystem
				 data))
			       0 ))))
	     (mapc
	      (lambda (i)
		(if (not (eq (car i) 'operatingSystem))
		(insert
		 (format
		  (concat (concat "%-" (number-to-string *width*)) "s : %s\n")
			 (car i) (cdr i)))))
	      data)
	     (display-buffer "*sl-detail*")
	     (message "Success")
	     ))))


;;callback
(defun test-click ()
  (interactive)
  (let ((device-id
	 (car
	  (split-string
	   (buffer-substring-no-properties
	    (point-at-bol)
	    (point-at-eol)))))
	(buffer
	 (get-buffer-create "*sl-detail*")))
    (get-vm-detail device-id buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; make url
;;;

(defun make-url-base (service method)
   (concat
    (concat
     (concat
      (concat
       (concat
	(concat
	 (concat
	  (concat "https://" sl-user)
	  ":")
	 sl-api-key)
	"@")
       "api.softlayer.com/rest/v3/")
      service) "/")
    method))

(defun make-url (url-base)
  (concat url-base ".json"))

(defun make-url-id (url-base id)
  (concat
   (concat url-base "/")
   (concat (number-to-string id)
	   ".json")))

;;;;;;;;;;;
;;;
;;; util
;;;

(defun get-max-length (list)
  "count string length of list"
  (setq *cnt* 0)
  (max-length-count list)
  *cnt*
    )

(defun max-length-count (list)
  "string length count function"
  (if list
      (progn
	(if (< *cnt* (length (symbol-name(car (car list)))))
	    (setq *cnt* (length (symbol-name(car (car list))))))
	(max-length-count (cdr list)))
    *cnt*)
  )

(defun sprit-param (params)
  (split-string  (cdr  params) ";")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SoftLayer Account
;;;

(defcustom sl-user (user-name)
  "*User  name of sftlayer.
i.e. \"User name <user@mail-domain>\"."
  :group 'sl-basic
  :type '(choice string (const nil)))

(defcustom sl-api-key (api-key)
  "*Api key of SoftLayer."
  :group 'sl-basic
  :type 'string)

;;; softlayer.el ends here



