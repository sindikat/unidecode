;;; unidecode.el --- Convert Unicode text into safe ASCII strings
;;
;; Copyright (C) 2013 sindikat
;;
;; Author: sindikat <sindikat at mail36 dot net>
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.4"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is placed in the public domain to the extent possible by law.
;;
;;; Commentary:

;; Transliterate Unicode characters into one of 128 ASCII characters.
;; This package is an Emacs Lisp port of Python Unidecode package.
;;
;; Python Unidecode can be found here:
;; http://pypi.python.org/pypi/Unidecode/
;;
;; More information in file README.org

;;; Code:
(require 'cl-lib)

(defconst unidecode-chars
  (read (find-file-noselect "unidecode-chars.el"))
  "Contains vector of unidecoded chars corresponding to Unicode
  code point of an original char")

(defun unidecode-unidecode (s)
  (apply #'concat (mapcar (lambda (ch) (elt unidecode-chars ch)) s)))

(defun unidecode-sanitize (s)
  "Strip all chars from string that are not alphanumeric or
hyphen, convert space to hyphen"
  (let ((s (replace-regexp-in-string " " "-" (unidecode-unidecode (downcase s))))
        (valid "abcdefghijklmnopqrstuvwxyz1234567890-"))
    (cl-remove-if-not (lambda (ch) (cl-find ch valid)) s)))

(provide 'unidecode)
;;; unidecode.el ends here
