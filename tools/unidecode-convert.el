;;; unidecode-convert.el --- Convert character tables -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018 John Mastro
;;
;; Author: John Mastro <john.b.mastro@gmail.com>
;; Version: 0.2
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

;;; Commentary:
;;
;; Convert the character tables in the Python Unidecode package to Emacs Lisp
;; data.
;;
;; See README.org for additional information.

;;; Code:

(require 'json)

(defvar unidecode-convert-python-program "python"
  "The Python executable to use.")

(defvar unidecode-convert-python-script
  (let* ((file (or load-file-name buffer-file-name))
         (here (file-name-directory (file-chase-links file))))
    (expand-file-name "unidecode_convert.py" here))
  "The Python script to use.")

(defun unidecode-convert-json-file (source destination)
  "Convert JSON file in SOURCE to Lisp data in DESTINATION."
  (let ((print-circle t)
        (data (json-read-file source)))
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp -*-\n")
      (prin1 data (current-buffer))
      (insert "\n")
      (write-region nil nil destination))))

(defun unidecode-convert-json-files (source destination)
  "Convert JSON files in SOURCE to Lisp data in DESTINATION."
  (let ((files (directory-files source t "\\.json\\'")))
    (dolist (file files)
      (let* ((base-name (file-name-nondirectory file))
             (sans-ext (file-name-sans-extension base-name))
             (out-file (concat "unidecode-" sans-ext ".eld"))
             (out-file (expand-file-name out-file destination)))
        (unidecode-convert-json-file file out-file)))))

(defun unidecode-convert (source destination)
  "Convert Unidecode modules in SOURCE to Lisp data.
Save the Lisp data files in DESTINATION."
  (interactive (list (read-directory-name "Source: ")
                     (read-directory-name "Destination: ")))
  (let ((tmp (make-temp-file "unidecode-tmp-" t)))
    (unwind-protect
        (with-current-buffer (get-buffer-create "*unidecode-convert*")
          (erase-buffer)
          (let ((ret (call-process unidecode-convert-python-program
                                   nil t nil
                                   unidecode-convert-python-script source tmp)))
            (if (zerop ret)
                (progn (unidecode-convert-json-files tmp destination)
                       (kill-buffer (current-buffer)))
              (pop-to-buffer (current-buffer))
              (message "unidecode_convert.py error"))))
      (delete-directory tmp t))))

(provide 'unidecode-convert)
;;; unidecode-convert.el ends here
