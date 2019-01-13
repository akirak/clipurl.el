;;; clipsave.el --- Minimalistic clipboard history -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.5"))
;; Keywords: convenience
;; URL: https://github.com/akirak/clipurl.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides `clipsave-mode', which watches the system
;; clipboard and saves every new content to the kill ring.
;; This makes Emacs a clipboard manager for your operating system.

;;; Code:

(defcustom clipsave-timer-interval 0.5
  "Interval at which check for an update in the system clipboard."
  :group 'clipsave
  :type 'number)

(defvar clipsave-timer nil)

(defvar clipsave--orig-save-interprogram-paste-before-kill)

(define-minor-mode clipsave-mode
  "Minimalistic clipboard history.

clipsave-mode is a minimalistic implementation of clipboard history
on Emacs. It just watches the system clipboard via
interprogram-paste-function and saves every new content to the kill
ring."
  :global t
  :require 'clipsave
  :lighter "clipsave"
  (cond
   (clipsave-mode
    (setq clipsave--orig-save-interprogram-paste-before-kill save-interprogram-paste-before-kill
          save-interprogram-paste-before-kill nil)
    (setq clipsave-timer (run-at-time nil clipsave-timer-interval
                                      #'clipsave--check)))
   (clipsave-timer
    (cancel-timer clipsave-timer)
    (setq clipsave-timer nil
          save-interprogram-paste-before-kill clipsave--orig-save-interprogram-paste-before-kill))))

(defun clipsave--check (&rest _args)
  "Check for an update in the selection."
  (let* ((sel (funcall interprogram-paste-function)))
    (when (and sel
               (not (equal (substring-no-properties sel)
                           (substring-no-properties (car kill-ring)))))
      (kill-new sel))))

(provide 'clipsave)
;;; clipsave.el ends here
