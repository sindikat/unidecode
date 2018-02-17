;;; unidecode.el --- Transliterate Unicode to ASCII -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013 sindikat
;;
;; Author: sindikat <sindikat at mail36 dot net>
;; Version: 0.1
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

(defconst unidecode-chars
  (with-temp-buffer
    (insert-file-contents "unidecode-chars.el")
    (read (current-buffer)))
  "Vector mapping Unicode code points to unidecoded data.")

(defun unidecode-region (beg end)
  "Transliterate Unicode chars between BEG and END to ASCII."
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (chr new)
      (while (setq chr (char-after))
        (delete-char 1)
        (insert (elt unidecode-chars chr))))))

(defun unidecode-unidecode (string)
  "Transliterate Unicode chars in STRING and return the result."
  (with-temp-buffer
    (insert string)
    (unidecode-region (point-min) (point-max))
    (buffer-string)))

(defun unidecode-sanitize-region (beg end)
  "Sanitize region between BEG and END.
Strip all characters that are not alphanumeric or hyphen, and
convert space to hyphen."
  (save-restriction
    (narrow-to-region beg end)
    (downcase-region (point-min) (point-max))
    (unidecode-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "[[:blank:]]" nil t)
      (replace-match "-"))
    (goto-char (point-min))
    (while (re-search-forward "[^a-z0-9-]+" nil t)
      (replace-match ""))))

(defun unidecode-sanitize (string)
  "Sanitize STRING and return the result.
Strip all characters that are not alphanumeric or hyphen, and
convert space to hyphen."
  (with-temp-buffer
    (insert string)
    (unidecode-sanitize-region (point-min) (point-max))
    (buffer-string)))

(provide 'unidecode)
;;; unidecode.el ends here
