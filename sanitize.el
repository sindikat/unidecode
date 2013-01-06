;;; sanitize.el --- convert Unicode text into safe ASCII strings
;; Copyright (C) 2013 sindikat
;;
;; Author: sindikat <sindikat at mail36 dot net>
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.
;;
;; This file is put into public domain to the extend possible by law.
;;
;;; Commentary:

;; This package is Python Unidecode implementation in Emacs Lisp.
;;
;; Python Unidecode can be found here:
;; http://pypi.python.org/pypi/Unidecode/
;;
;; The following is the explanation of the process of converting the
;; Python package to Emacs Lisp.
;;
;; The following Python script was used
;; to export Unidecode data to JSON.

;; import os, sys
;;
;; xs = os.listdir('.')
;; xs.remove('__init__.py')
;; xs.sort()
;;
;; final_data = []
;; for filename in xs:
;;     module = __import__(filename[:-3])
;;     module_data = list(module.data)
;;
;;     # some modules have data of 255 entries, fill them up to 256
;;     if len(module_data) < 256:
;;         module_data += [''] * (256 - len(module_data))
;;
;;     final_data.extend(module_data)
;;
;; import json
;;
;; with open('unidecode.json', 'w') as filename:
;;     json.dump(final_data, filename)

;; The following command was used to load JSON data as Emacs Lisp
;; vector, after installing `json` package in Emacs:
;;
;; (json-read-file "unidecode.json")
;;
;; After that the resulting vector was just stored verbatim in
;; "sanitize-chars.el".

;;; Code:

(setq sanitize/unidecode-chars (read (find-file-noselect "sanitize-chars.el")))

(defun sanitize/unidecode (s)
  (apply #'concat (mapcar (lambda (ch) (elt sanitize/unidecode-chars ch)) s)))

(defun sanitize/sanitize (s)
  "Strip all chars from string that are not alphanumeric or
hyphen, convert space to hyphen"
  (let ((s (replace-regexp-in-string " " "-" (sanitize/unidecode (downcase s))))
        (valid "abcdefghijklmnopqrstuvwxyz1234567890-"))
    (remove-if-not (lambda (ch) (find ch valid)) s)))

;;; sanitize.el ends here
