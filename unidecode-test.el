;;; unidecode-test.el --- unit tests for Unidecode
;; Copyright (C) 2013 sindikat
;;
;; Author: sindikat <sindikat at mail36 dot net>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.
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
    (let ((result (unidecode s-original)))
      (should (equal result s-decoded)))))

(ert-deftest unidecode-test-sanitize ()
  "Test that sanitization produces strings of only alphanumeric
characters with whitespace converted into hyphen."
  (let ((s-original "æб¦ ")
        (s-sanitized "aeb-"))
    (let ((result (unidecode-sanitize s-sanitized)))
      (should (equal result s-sanitized)))))
;;; unidecode-test.el ends here
