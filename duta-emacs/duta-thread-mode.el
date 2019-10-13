;;; duta-thread-mode.el --- Thread viewer

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

(defvar-local duta-thread-mode-id nil)

(define-derived-mode duta-thread-mode
  fundamental-mode "Duta-Thread"
  "Major mode for showing a thread.
 \\{duta-thread-mode-map}"
  (setq buffer-read-only t)
  (hl-line-mode 1))

(defun duta-thread-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert "Refreshing ...\n\n")
    (redisplay)
    (let ((thread (duta-get (format "thread/%d" duta-thread-mode-id))))
      (save-excursion
        (erase-buffer)
        (insert (format "%S" thread))))))

(defun duta-thread-buffer-name (thread-id)
  (format "*duta:thread-%d*" thread-id))

(provide 'duta-thread-mode)
