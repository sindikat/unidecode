;;; unidecode-test.el --- unit tests for Unidecode
;; Copyright (C) 2013 sindikat
;;
;; Author: sindikat <sindikat at mail36 dot net>
;;
;; This file is not part of GNU Emacs.
;;
;; This file is put into public domain to the extend possible by law.
;;
;;; Commentary:

;;

;;; Code:
(require 'ert)

(defun unidecode-test ()
  "Unit test Unidecode functions."
  (interactive)
  (ert "^unidecode-"))

(ert-deftest unidecode-test-unidecode ()
  "Test if Unicode characters convert to ASCII.
\"Привет\" -> \"Privet\"."
  (let ((s-original "æб¦ ")
        (s-decoded "aeb| "))
    (let ((result (unidecode-unidecode s-original)))
      (should (equal result s-decoded)))))

(ert-deftest unidecode-test-sanitize ()
  "Test that sanitization produces strings of only alphanumeric
characters with whitespace converted into hyphen."
  (let ((s-original "æб¦ ")
        (s-sanitized "aeb-"))
    (let ((result (unidecode-sanitize s-sanitized)))
      (should (equal result s-sanitized)))))
;;; unidecode-test.el ends here
