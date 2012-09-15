;;; yaml-mode.el --- Major mode for editing YAML files

;; Copyright (C) 2010  Yoshiki Kurihara

;; Author: Yoshiki Kurihara <kurihara@cpan.org>
;;         Marshall T. Vandegrift <llasram@gmail.com>
;; Keywords: data yaml
;; Version: 0.0.7

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a major mode for editing files in the YAML data
;; serialization format.  It was initially developed by Yoshiki
;; Kurihara and many features were added by Marshall Vandegrift.  As
;; YAML and Python share the fact that indentation determines
;; structure, this mode provides indentation and indentation command
;; behavior very similar to that of python-mode.

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it.  To automatically
;; handle files ending in '.yml', add something like:
;;
;;    (require 'yaml-mode)
;;    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; to your .emacs file.
;;
;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; Known Bugs:

;; YAML is easy to write but complex to parse, and this mode doesn't
;; even really try.  Indentation and highlighting will break on
;; abnormally complicated structures.

;;; Code:


;; User definable variables

(defgroup yaml nil
  "Support for the YAML serialization format"
  :group 'languages
  :prefix "yaml-")

(defcustom yaml-mode-hook nil
  "*Hook run by `yaml-mode'."
  :type 'hook
  :group 'yaml)

(defcustom yaml-indent-offset 2
  "*Amount of offset per level of indentation."
  :type 'integer
  :group 'yaml)

(defcustom yaml-backspace-function 'backward-delete-char-untabify
  "*Function called by `yaml-electric-backspace' when deleting backwards."
  :type 'function
  :group 'yaml)

(defcustom yaml-block-literal-search-lines 100
  "*Maximum number of lines to search for start of block literals."
  :type 'integer
  :group 'yaml)

(defcustom yaml-block-literal-electric-alist
  '((?| . "") (?> . "-"))
  "*Characters for which to provide electric behavior.
The association list key should be a key code and the associated value
should be a string containing additional characters to insert when
that key is pressed to begin a block literal."
  :type 'alist
  :group 'yaml)

(defface yaml-tab-face
   '((((class color)) (:background "red" :foreground "red" :bold t))
     (t (:reverse-video t)))
  "Face to use for highlighting tabs in YAML files."
  :group 'faces
  :group 'yaml)

(defcustom yaml-imenu-generic-expression
  '((nil  "^\\(:?[a-zA-Z_-]+\\):"          1))
  "The imenu regex to parse an outline of the yaml file."
  :type 'string
  :group 'yaml)


;; Constants

(defconst yaml-mode-version "0.0.7" "Version of `yaml-mode.'")

(defconst yaml-blank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst yaml-comment-re "\\(?:^\\|\\s-+\\)\\(#.*\\)"
  "Regexp matching a line containing a YAML comment or delimiter.")

(defconst yaml-directive-re "^\\(?:--- \\)? *%\\(\\w+\\)"
  "Regexp matching a line contatining a YAML directive.")

(defconst yaml-document-delimiter-re "^ *\\(?:---\\|[.][.][.]\\)"
  "Rexex")
(provide 'yaml-mode)