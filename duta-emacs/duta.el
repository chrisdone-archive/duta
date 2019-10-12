;;; duta.el --- Duta mail server

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar-local duta-server-url nil)
(defvar-local duta-username nil)
(defvar-local duta-password nil)

(defun duta-get (path)
  "Make a GET request to PATH."
  (unless duta-server-url (error "Need duta-server-url"))
  (unless duta-username (error "Need duta-username"))
  (unless duta-password (error "Need duta-password"))
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Content-Type" . "Content-Type: application/json")
           ("user" . ,duta-username)
           ("pass" . ,duta-password))))
    (let ((buffer (url-retrieve-synchronously (concat duta-server-url path) t)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward-regexp "\r?\n\r?\n" nil nil 1)
        (json-read-from-string
         (decode-coding-string
          (buffer-substring-no-properties (point) (point-max))
          'utf-8))))))

(provide 'duta)
