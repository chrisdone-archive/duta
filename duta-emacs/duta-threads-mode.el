;;; duta-threads-mode.el --- Threads listing mode

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

(defvar-local duta-threads-mode-path "/"
  "Path to pull threads from.")

(defun duta ()
  "Start duta in buffer *duta*, listing threads in the inbox."
  (interactive)
  (with-current-buffer (get-buffer-create "*duta*")
    (duta-threads-mode)
    (switch-to-buffer (current-buffer))))

(define-derived-mode duta-threads-mode
  fundamental-mode "Duta"
  "Major mode for listing threads.
 \\{duta-threads-mode-map}"
  (setq buffer-read-only t)
  (duta-threads-refresh))

(defface duta-threads-mode-unread-face
  '((((class color) (min-colors 88) (background light))
     :weight bold)
    (((class color) (min-colors 88) (background dark))
     :weight bold
     :foreground "#8cd0d3")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-threads-mode-read-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#8cd0d3")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(defface duta-threads-mode-timestamp-face
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark))
     :foreground "#88b090")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'duta)

(define-key duta-threads-mode-map (kbd "g") 'duta-threads-refresh)

(defun duta-threads-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (let ((threads (duta-get duta-threads-mode-path)))
      (save-excursion
        (erase-buffer)
        (mapc (lambda (thread) (insert (duta-threads-render-thread thread)))
              threads)))))

(defun duta-threads-render-thread (thread)
  (let* ((tags (mapcar #'identity (cdr (assoc 'tags thread))))
         (unread (cl-remove-if-not
                  (lambda (tag)
                    (let ((label (cdr (assoc 'label tag))))
                      (string= label "unread")))
                                   tags)))
    (format "%s (%d)\n%s\n\n"
          (propertize (cdr (assoc 'subject thread))
                      'face
                      (if unread
                          'duta-threads-mode-unread-face
                        'duta-threads-mode-read-face))
          (cdr (assoc 'messages thread))
          (propertize (cdr (assoc 'updated thread))
                      'face 'duta-threads-mode-timestamp-face))))

(provide 'duta-threads-mode)
