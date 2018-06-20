;;; apparmor-mode.el --- major mode for editing AppArmor policy files         -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/apparmor-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; The following documentation sources were used:
;; https://gitlab.com/apparmor/apparmor/wikis/QuickProfileLanguage
;; https://gitlab.com/apparmor/apparmor/wikis/ProfileLanguage

;;;; Setup

;; (require 'apparmor-mode)

;;; Code:

(defvar apparmor-mode-keywords '("audit"
                                 "capability"
                                 "delegate"
                                 "dbus"
                                 "deny"
                                 "include"
                                 "network"
                                 "on"
                                 "owner"
                                 "quiet"
                                 "rlimit"
                                 "to"))

(defvar apparmor-mode-capabilities '("setuid"
                                     "setgid"))

(defvar apparmor-mode-network-permissions '("create" "accept" "bind" "connect"
                                            "listen" "read" "write" "send"
                                            "receive" "getsockname" "getpeername"
                                            "getsockopt" "setsockopt" "fcntl"
                                            "ioctl" "shutdown" "getpeersec"))

(defvar apparmor-mode-network-domains '("inet" "ax25" "ipx" "appletalk" "netrom"
                                        "bridge" "atmpvc" "x25" "inet6" "rose"
                                        "netbeui" "security" "key" "packet"
                                        "ash" "econet" "atmsvc" "sna" "irda"
                                        "pppox" "wanpipe" "bluetooth"))

(defvar apparmor-mode-network-types '("stream" "dgram" "seqpacket" "raw" "rdm"
                                      "packet" "dccp"))

;; TODO: this is not complete since is not fully documented
(defvar apparmor-mode-network-protocols '("tcp" "udp" "icmp"))

(defvar apparmor-mode-dbus-permissions '("r" "w" "rw" "send" "receive"
                                         "acquire" "bind" "read" "write"))

(defvar apparmor-mode-rlimit-types '("fsize" "data" "stack" "core" "rss" "as" "memlock" "msgqueue"
                                     "nofile" "locks" "sigpending" "nproc" "rtprio" "cpu"
                                     "nice"))

(defvar apparmor-mode-variable-regexp "^\\s-*\\(@{[[:alpha:]]+}\\)\\s-*\\(+?=\\)\\s-*\\([[:graph:]]+\\)\\(\\s-+\\([[:graph:]]+\\)\\)?\\s-*\\(#.*\\)?$")

(defvar apparmor-mode-profile-name-regexp "[[:word:]/_-]+")

(defvar apparmor-mode-profile-regexp
  (concat "^\\s-*\\(\\^?" apparmor-mode-profile-name-regexp "\\)\\s-+{\\s-*$"))

(defvar apparmor-mode-file-rule-regexp
  (concat "^\\s-*\\(\\(audit\\|owner\\|deny\\)\\s-+\\)?"
          "\\([][[:word:]/@{}*.,_-]+\\)\\s-+\\([CPUacilmpruwx]+\\)\\s-*"
          "\\(->\\s-*\\(" apparmor-mode-profile-name-regexp "\\)\\)?\\s-*"
          ","))

(defvar apparmor-mode-network-rule-regexp
  (concat
   "^\\s-*\\(\\(audit\\|quiet\\|deny\\)\\s-+\\)?network\\s-*"
   "\\(" (regexp-opt apparmor-mode-network-permissions 'words) "\\)?\\s-*"
   "\\(" (regexp-opt apparmor-mode-network-domains 'words) "\\)?\\s-*"
   "\\(" (regexp-opt apparmor-mode-network-types 'words) "\\)?\\s-*"
   "\\(" (regexp-opt apparmor-mode-network-protocols 'words) "\\)?\\s-*"
   ;; TODO: address expression
   "\\(delegate\\s+to\\s+\\(" apparmor-mode-profile-name-regexp "\\)\\)?\\s-*"
   ","))

(defvar apparmor-mode-dbus-rule-regexp
  (concat
   "^\\s-*\\(\\(audit\\|deny\\)\\s-+\\)?dbus\\s-*"
   "\\(\\(bus\\)=\\(system\\|session\\)\\)?\\s-*"
   "\\(\\(dest\\)=\\([[:alpha:].]+\\)\\)?\\s-*"
   "\\(\\(path\\)=\\([[:alpha:]/]+\\)\\)?\\s-*"
   "\\(\\(interface\\)=\\([[:alpha:].]+\\)\\)?\\s-*"
   "\\(\\(method\\)=\\([[:alpha:]_]+\\)\\)?\\s-*"
   ;; permissions - either a single permission or multiple permissions in
   ;; parentheses with commas and whitespace separating
   "\\("
   (regexp-opt apparmor-mode-dbus-permissions 'words)
   "\\|"
   "("
   (regexp-opt apparmor-mode-dbus-permissions 'words)
   "\\("
   (regexp-opt apparmor-mode-dbus-permissions 'words) ",\\s-+"
   "\\)"
   "\\)?\\s-*"
   ","))

(defvar apparmor-mode-font-lock-defaults
  `(((,(regexp-opt apparmor-mode-keywords 'words) . font-lock-keyword-face)
     (,(regexp-opt apparmor-mode-capabilities 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-network-permissions 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-network-domains 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-network-types 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-dbus-permissions 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-rlimit-types 'words) . font-lock-constant-face)
     ("," . 'font-lock-builtin-face)
     ("->" . 'font-lock-builtin-face)
     ("=" . 'font-lock-builtin-face)
     ("+=" . 'font-lock-builtin-face)
     ("<=" . 'font-lock-builtin-face) ; rlimit
     ;; variables
     (,apparmor-mode-variable-regexp 1 font-lock-variable-name-face t)
     ;; profiles
     (,apparmor-mode-profile-regexp 1 font-lock-function-name-face t)
     ;; file rules
     (,apparmor-mode-file-rule-regexp 4 font-lock-constant-face t)
     (,apparmor-mode-file-rule-regexp 6 font-lock-function-name-face t)
     ;; dbus rules
     (,apparmor-mode-dbus-rule-regexp 4 font-lock-variable-name-face t) ;bus
     (,apparmor-mode-dbus-rule-regexp 5 font-lock-constant-face t) ;system/session
     (,apparmor-mode-dbus-rule-regexp 7 font-lock-variable-name-face t) ;dest
     (,apparmor-mode-dbus-rule-regexp 10 font-lock-variable-name-face t)
     (,apparmor-mode-dbus-rule-regexp 13 font-lock-variable-name-face t)
     (,apparmor-mode-dbus-rule-regexp 16 font-lock-variable-name-face t))))

(defvar apparmor-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode apparmor-mode fundamental-mode "aa"
  "apparmor-mode is a major mode for editing AppArmor profiles."
  :syntax-table apparmor-mode-syntax-table
  (setq font-lock-defaults apparmor-mode-font-lock-defaults)
  (setq imenu-generic-expression `(("Profiles" ,apparmor-mode-profile-regexp 1)))
  (setq comment-start "#")
  (setq comment-end ""))

;;;###autoload
;; todo - files in /etc/apparmor.d/ should use this syntax perhaps?
;;(add-to-list 'auto-mode-alist '("\\.apparmor\\'" . apparmor-mode))

(provide 'apparmor-mode)
;;; apparmor-mode.el ends here
