;;; km-pass.el --- Extra utils for pass -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-pass
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (transient "0.4.3") (password-store "1.7.4") (password-store-otp "0.1.5") (pass "2.0"))
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
(require 'password-store-otp)
(require 'pass)
(require 'auth-source-pass)

(defcustom km-pass-user-fields '("login" "user" "username" "email")
  "List of user field names for password entries.

A list of field names to search for user identification information within a
password entry. The default field names are \"login\", \"user\", \"username\",
and \"email\".

Each element in the list is a string that specifies a field name to be searched
for within the password entry data. The functions `km-pass-copy-username' and
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


(defcustom km-pass-entry-actions '((?c "Copy password" km-pass-copy-password
                                    :if km-pass-has-secret-p)
                                   (?o "Copy Otp" password-store-otp-token-copy
                                    :if
                                    km-pass-has-otp-auth-p)
                                   (?u "Copy user name" km-pass-copy-username
                                    :if km-pass-has-username-p)
                                   (?w "Copy other field value"
                                    km-pass-copy-entry-field)
                                   (?e "Edit" password-store-edit)
                                   (?D "Delete" km-pass-delete-action)
                                   (?a "Add" km-pass-add-action)
                                   (?r "Rename" km-pass-rename-action)
                                   (?g "Generate" km-pass-generate-action))
  "Actions for password-store entries with conditional display.

A list of actions that can be performed on password-store entries. Each action
is a list with the following elements:

1. A character, which is the key to trigger the action.
2. A string, which is the description of the action.
3. A function or a symbol that will be called when the action is triggered.
4. An optional property list that can contain `:if' or `:if-not' followed by a
function or a symbol. The action will only be available if the specified
condition is met.

The default actions include copying the password, OTP token, username, other
field values, editing, deleting, adding, renaming, and generating new passwords
for entries.

Each action's availability can be conditionally determined by the presence of
certain fields in the entry, such as a secret, OTP, or username.

To use an action, invoke the function that handles the execution of actions on
password-store entries, passing the desired entry as an argument. The function
will present a list of available actions based on the conditions specified in
the property list. Selecting an action will execute the associated function or
symbol."
  :type `(repeat
          (list
           (character
            :tag "Key"
            :value ?x)
           (string :tag "Description" "Description")
           (choice
            (function :tag "Function")
            (symbol :tag "Symbol"))
           (repeat
            :tag "If"
            :inline t
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if"
               :doc "Enable if predicate returns non-nil."
               :if)
              (const
               :format "%v %d"
               :tag ":if-not"
               :doc "Enable if predicate returns nil."
               :if-not)
              (symbol :tag "other"))
             (choice
              (function :tag "Function")
              (symbol :tag "Symbol")
              (sexp :tag "Sexp"))))))
  :group 'km-pass)

(defun km-pass-add-action (key)
  "Ask for a new key based on KEY, then edit it."
  (let ((new-key (read-string "New key: " key)))
    (password-store-edit new-key)))

(defun km-pass-generate-action (key)
  "Ask for a new key based on KEY, then generate an entry and password for it.

Default PASSWORD-LENGTH is ‘password-store-password-length’."
  (let ((new-key (read-string "Generate password for new key: " key)))
    (password-store-generate new-key)
    (password-store-edit new-key)))

(defun km-pass-delete-action (key)
  "Delete entry for KEY."
  (when (yes-or-no-p (format "Really delete the entry `%s'?" key))
    (password-store-remove key)))

(defun km-pass-rename-action (key)
  "Rename entry for KEY."
  (let ((new-name (read-string (format "Rename `%s' to: " key) key)))
    (password-store-rename key new-name)))

(defun km-pass--password-action (key)
  "Add password for KEY to kill ring."
  (password-store-copy key))

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

(defun km-pass-has-username-p (entry)
  "Check if ENTRY has a username field.

Argument ENTRY is the pass entry name or a parsed entry list."
  (km-pass-get-field entry
                     km-pass-user-fields t))

(defun km-pass-has-secret-p (entry)
  "Check if ENTRY has a `secret' field.

Argument ENTRY is the pass entry name or a parsed entry list."
  (km-pass-get-field entry
                     'secret t))

(defun km-pass-has-url-p (entry)
  "Check if ENTRY contain a URL field.

Argument ENTRY is the pass entry name or a parsed entry list."
  (km-pass-get-field entry
                     km-pass-url-fields t))


(defun km-pass-kill-new (value &optional prompt)
  "Copy VALUE to the clipboard and optionally display PROMPT.

Argument VALUE is the string to be added to the kill ring.

Optional argument PROMPT is the message displayed after copying, if provided."
  (if (not value)
      (message  "Couldn't copy %s" (or prompt ""))
    (kill-new value)
    (message  "Copied %s" (or prompt "")))
  value)




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


(defun km-pass-can-decrypt-without-prompt (file)
  "Check if FILE can be decrypted without prompting for a passphrase."
  (let ((process-connection-type nil))
    (with-temp-buffer
      (let ((exit-code (call-process "gpg" nil (current-buffer) nil
                                     "--quiet" "--batch" "--yes" "--no-tty"
                                     "--decrypt" (expand-file-name file))))
        (and (zerop exit-code)
             (not (string-match-p "failed" (buffer-string))))))))


(defun km-pass--entry-to-file (entry)
  "Return file name corresponding to ENTRY."
  (let ((file entry)
        (ext (if (equal (file-name-extension entry) "gpg")
                 ""
               ".gpg")))
    (unless (file-name-absolute-p file)
      (setq file (expand-file-name file (password-store-dir))))
    (concat file ext)))

(defun km-pass-has-otp-auth-p (entry)
  "Check if ENTRY has OTP authentication data.

Argument ENTRY is the name of the pass entry to check for OTP authentication."
  (let ((file (km-pass--entry-to-file entry)))
    (when (and (file-exists-p file)
               (km-pass-can-decrypt-without-prompt file))
      (with-temp-buffer (insert-file-contents file)
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "otpauth://" nil t 1))))))

;;;###autoload
(defun km-pass-act (entry)
  "Execute an action on a password-store entry.

Argument ENTRY is the password entry to act upon."
  (interactive (list (completing-read "Password entry: " (password-store-list)
                                      nil)))
  (let* ((filtered-actions (seq-filter
                            (pcase-lambda (`(,_k ,_descr ,_action . ,plist))
                              (let ((if-pred (plist-get
                                              plist :if))
                                    (if-not-pred
                                     (plist-get plist
                                                :if-not)))
                                (and
                                 (or (not if-pred)
                                     (funcall
                                      if-pred
                                      entry))
                                 (or (not if-not-pred)
                                     (not (funcall
                                           if-not-pred
                                           entry))))))
                            km-pass-entry-actions))
         (choice (read-multiple-choice
                  (format "Action for %s" entry)
                  filtered-actions))
         (action (caddr choice)))
    (funcall action entry)))

;;;###autoload
(defun km-pass-copy-entry-field (entry)
  "Copy a field value from a specified password entry.

Argument ENTRY is the name of the password entry to copy the field from."
  (interactive (list (km-pass--completing-read t)))
  (let* ((alist (mapcar (pcase-lambda (`(,k . ,v))
                          (cons (format "%s" k) v))
                        (auth-source-pass-parse-entry entry)))
         (field (completing-read "Copy from entry"
                                 (mapcar #'car alist)))
         (value (cdr (assoc-string field alist))))
    (km-pass-kill-new value field)
    value))

;;;###autoload
(defun km-pass-copy-username (entry)
  "Copy the username associated with ENTRY to the kill ring.

Argument ENTRY is the name of the password entry to copy the username from."
  (interactive
   (list
    (km-pass--completing-read t)))
  (if-let* ((user
             (km-pass-get-field entry
                                km-pass-user-fields)))
      (progn
        (password-store-clear)
        (km-pass-kill-new user entry))
    (error "Username not found")))

;;;###autoload
(defun km-pass-copy-password (entry)
  "Copy the password associated with ENTRY to the kill ring.

Argument ENTRY is the name of the password entry to copy the username from."
  (interactive
   (list
    (km-pass--completing-read t)))
  (km-pass-kill-new (km-pass-get-secret entry) entry))

;;;###autoload (autoload 'km-pass-mode-transient "km-pass" nil t)
(transient-define-prefix km-pass-mode-transient ()
  "Define a transient for `pass-mode'."
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
   ("a" "Read entry and then prompt for action" km-pass-act)
   ("w" "Copy password" km-pass-copy-password)
   ("o" "Copy OTP" password-store-otp-token-copy)
   ("u" "copy [u]ser" km-pass-copy-username)
   ("U" "Browse url" password-store-url)
   ("v" "Copy version" password-store-version)
   ("R" "Rename" password-store-rename)
   ("D" "Delete" password-store-remove)
   ("p" "open pass" pass)])


(provide 'km-pass)
;;; km-pass.el ends here