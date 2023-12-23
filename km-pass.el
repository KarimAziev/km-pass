;;; km-pass.el --- Extra utils for pass -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-pass
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra utils for pass

;;; Code:



(require 'transient)
(require 'password-store)
(require 'ivy-pass)
(require 'pass)
(require 'auth-source-pass)

(defcustom km-pass-user-fields '("login" "user" "username" "email")
  "List of user field names for password entries.

A list of field names to search for user identification information within a
password entry. The default field names are \"login\", \"user\", \"username\",
and \"email\".

Each element in the list is a string that specifies a field name to be searched
for within the password entry data. The functions `km-pass--copy-username' and
`km-pass-get-user' use this list to determine which fields to check when looking
for a username or user identification.

To customize the list, add or remove strings corresponding to the field names
that should be considered as containing user identification information. The
order of the strings in the list determines the priority of the fields during
the search."
  :group 'km-pass
  :type '(repeat (string :tag "Field name")))

(defcustom km-pass-url-fields '("url" "site" "location")
  "List of field names to search for URLs in password entries.

A list of field names to search for URLs within password entries. The default
field names are \"url\", \"site\", and \"location\".

Each element in the list should be a string representing a key within a password
entry that may contain a URL. These fields are used when attempting to extract a
URL associated with a password entry."
  :group 'km-pass
  :type '(repeat (string :tag "Field name")))

(defmacro km-pass--with-otp-error (&rest body)
  "Handle OTP errors in BODY, rephrase missing `pass-otp' extension message.

Remaining arguments BODY are forms that are evaluated with error handling for
OTP-related errors."
  (declare
   (indent defun))
  `(condition-case err
       ,@body
     (error
      (let ((error-msg (error-message-string err)))
       (if (string= error-msg "Error: otp is not in the password store.")
           (error "Error: pass extension `pass-otp' is not installed")
         (error error-msg))))))

(defun km-pass--store-otp-add-uri (method entry uri)
  "Add OTP URI to a password store entry.

Argument METHOD is a symbol that specifies the method of adding the OTP URI,
either `append' or `insert'.

Argument ENTRY is a string representing the name of the password store entry to
which the OTP URI will be added.

Argument URI is a string containing the OTP uri to be added to the password
store entry."
  (unless (memq method '(append insert))
    (error (format "Unrecognized method %s" method)))
  (km-pass--with-otp-error
    (password-store--run "otp" "--help"))  ;; make sure otp extension is installed.
  (message "%s" (shell-command-to-string (format "echo %s | %s otp %s -f %s"
                                                 (shell-quote-argument uri)
                                                 password-store-executable
                                                 method
                                                 (shell-quote-argument entry)))))





(defun km-pass--completing-read (&optional require-match)
  "Prompt user to select a password entry with optional match requirement.

Optional argument REQUIRE-MATCH determines whether the user must select an
existing entry. If non-nil, the user is not allowed to exit unless the input
matches one of the entries in the completion list."
  
  (completing-read "Password entry: " (password-store-list) nil require-match))

;;;###autoload
(defun km-pass-append-otp (entry otp-uri)
  "Append OTP URI to a password entry.

Argument ENTRY is the name of the password store entry to which the OTP URI will
be appended.

Argument OTP-URI is the URI containing the OTP token information to append to
the password store entry."
  (interactive (list (km-pass--completing-read)
                     (read-passwd "OTP URI: " t)))
  (km-pass--store-otp-add-uri 'append entry otp-uri))



;;;###autoload
(defun km-pass-append-otp-from-image (entry)
  "Append OTP from an image to a password entry.

Argument ENTRY is the name of the password store entry to which the OTP will be
appended."
  (interactive (list (password-store--completing-read)))
  (let ((qr-image-filename (read-file-name "Screenshot with QR code: ")))
    (with-temp-buffer
      (condition-case nil
          (call-process "zbarimg" nil t nil "-q" "--raw"
                        qr-image-filename)
        (error
         (error "It seems you don't have `zbar-tools' installed")))
      (km-pass-append-otp
       entry
       (buffer-substring (point-min)
                         (point-max))))
    (delete-file qr-image-filename)))


(defun km-pass--copy-username (entry)
  "Copy the username from a password ENTRY to the kill ring.

Argument ENTRY is the pass entry name or a parsed entry list."
  (if-let* ((user
             (km-pass-get-field entry
                                km-pass-user-fields)))
      (progn
        (password-store-clear)
        (message "Copied username to the kill ring.")
        (kill-new user))
    (error "Username not found")))


;;;###autoload
(defun km-pass-get-field (entry fields &optional noerror)
  "Extract specified FIELDS from a pass ENTRY.

Argument ENTRY is the pass entry name or a parsed entry list.

Arguments FIELDS is a list of field names or a single field name to search for
in the pass entry.

Optional argument NOERROR, when non-nil, prevents the function from signaling an
error if the ENTRY is not found."
  (setq fields (if (proper-list-p fields)
                   fields
                 (list fields)))
  (if-let ((data (if (listp entry)
                     entry
                   (auth-source-pass-parse-entry entry))))
      (let ((key)
            (found))
        (while (and
                (not found)
                (consp fields)
                (progn
                  (setq key
                        (pop fields))
                  (let ((it
                         (assoc key data)))
                    (or (not it)
                        (setq found
                              (cdr it))
                        nil)))))
        found)
    (unless noerror
      (error "Couldn't find entry: %s" entry))))

;;;###autoload
(defun km-pass-get-user (entry)
  "Retrieve user identification from a password entry.

Argument ENTRY is a string representing the password entry name."
  (km-pass-get-field entry km-pass-user-fields t))

;;;###autoload
(defun km-pass-get-secret (entry)
  "Retrieve the `secret' field from a password entry.

Argument ENTRY is the name of the pass entry from which to retrieve the secret."
  (km-pass-get-field entry 'secret t))

;;;###autoload
(defun km-pass-copy-user (entry)
  "Copy the username associated with ENTRY to the kill ring.

Argument ENTRY is the name of the password entry to copy the username from."
  (interactive
   (list
    (km-pass--completing-read)))
  (km-pass--copy-username entry))

(defun km-pass-get-otp-entries ()
  (let ((dir (password-store-dir))
        (inhibit-message t))
    (seq-filter (lambda (file)
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name
                      (concat
                       file
                       ".gpg")
                      dir))
                    (pass-view--has-otp-p)))
                (password-store-list))))

;;;###autoload (autoload 'km-pass-mode-transient "km-pass" nil t)
(transient-define-prefix km-pass-mode-transient ()
  "Define a transient for password management commands."
  [("w" "Copy password" pass-copy)
   ("f" "Copy field" pass-copy-field)
   ("b" "Copy username" pass-copy-username)
   ("u" "Copy url" pass-copy-url)
   ""
   ("i" "Insert" pass-insert)
   ("I" "Generate" pass-insert-generated)
   ("r" "Rename" pass-rename)
   ("k" "Delete" pass-kill)
   ("e" "Edit" pass-edit)
   ("j" "Jump to Entry" pass-goto-entry)
   ("U" "Browse url" pass-browse-url)
   ("g" "Update" pass-update-buffer)
   ("o" "OTP Support" pass-otp-options)
   ("RET" "View entry" pass-view)]
  [("n" "Next" pass-next-entry :transient t)
   ("p" "Previous" pass-prev-entry :transient t)
   ("M-n" "Next dir" pass-next-directory :transient t)
   ("M-p" "Previous dir" pass-prev-directory :transient t)])

;;;###autoload (autoload 'km-pass-transient "km-pass" nil t)
(transient-define-prefix km-pass-transient ()
  "Define a transient command menu for password operations."
  ["Passwords"
   ("p" "copy secret" ivy-pass)
   ("c" "Copy OTP" password-store-otp-token-copy)
   ("u" "copy [u]ser" km-pass-copy-user)
   ("o" "open pass" pass)])

(provide 'km-pass)
;;; km-pass.el ends here