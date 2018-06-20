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

(defvar apparmor-mode-keywords '("include"
                                 "capability"
                                 "deny"
                                 "network"
                                 "owner"))

(defvar apparmor-mode-capabilities '("setuid"
                                     "setgid"))

(defvar apparmor-mode-network-families '("inet"
                                 "packet"
                                 "raw"))

(defvar apparmor-mode-network-types '("dgram"
                                      "stream"))

(defvar apparmor-profile-regexp "^\\s-*\\(\\^?[[:word:]/_-]+\\)\\s-+{\\s-*$")

(defvar apparmor-file-rule-regexp "^\\s-*\\(\\(owner\\|deny\\)\\s-+\\)?\\([[:word:]/@{}*.,_-]+\\)\\s-+\\([CPilmprwx]+\\)\\(\\s-*->\\s-*\\([[:word:]/]+\\)\\)?\\s-*,\\s-*#?.*$")

(defvar apparmor-mode-font-lock-defaults
  `(((,(regexp-opt apparmor-mode-keywords 'words) . font-lock-keyword-face)
     (,(regexp-opt apparmor-mode-capabilities 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-network-families 'words) . font-lock-constant-face)
     (,(regexp-opt apparmor-mode-network-types 'words) . font-lock-constant-face)
     ("," . 'font-lock-builtin-face)
     ("->" . 'font-lock-builtin-face)
     ;; profile names
     (,apparmor-profile-regexp 1 font-lock-function-name-face t)
     ;; file paths
     (,apparmor-file-rule-regexp 3 font-lock-variable-name-face t)
     ;; file permissions
     (,apparmor-file-rule-regexp 4 font-lock-constant-face t)
     ;; transition profile
     (,apparmor-file-rule-regexp 6 font-lock-function-name-face t))))

(defvar apparmor-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode apparmor-mode fundamental-mode "aa"
  "apparmor-mode is a major mode for editing AppArmor profiles."
  :syntax-table apparmor-mode-syntax-table
  (setq font-lock-defaults apparmor-mode-font-lock-defaults)
  (setq imenu-generic-expression `(("Profiles" ,apparmor-profile-regexp 1)))
  (setq comment-start "#")
  (setq comment-end ""))

;;;###autoload
;; todo - files in /etc/apparmor.d/ should use this syntax perhaps?
;;(add-to-list 'auto-mode-alist '("\\.apparmor\\'" . apparmor-mode))

(provide 'apparmor-mode)
;;; apparmor-mode.el ends here
