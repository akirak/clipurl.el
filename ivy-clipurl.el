;;; ivy-clipurl.el --- Ivy interface to clipurl -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.10"))
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

;; This library provides `ivy-clipurl', which is an Ivy interface to
;; clipurl.

;;; Code:

(require 'ivy)
(require 'clipurl)

(defgroup ivy-clipurl nil
  "Ivy for clipurl."
  :group 'clipurl)

(defcustom ivy-clipurl-default-action #'kill-new
  "Default action of `ivy-clipurl'."
  :group 'ivy-clipurl
  :type '(choice (const :tag "Save into the kill ring" kill-new)
                 (const :tag "Browse the URL" clipurl-browse-url)
                 (const :tag "Browse the URL in other window" clipurl-browse-url-other-window)
                 (const :tag "Browse the URL externally" clipurl-browse-url-external-browser)
                 function))

;;;###autoload
(defun ivy-clipurl ()
  "Pick a URL in the clipboard and the kill ring."
  (interactive)
  (ivy-read "URLs: " #'clipurl--complete-url-in-kill-ring
            :caller 'ivy-clipurl
            :action ivy-clipurl-default-action))

(ivy-add-actions 'ivy-clipurl
                 '(("I" clipurl-insert-url-dwim "insert (dwim)")
                   ("g" clipurl-browse-url "browse")
                   ("j" clipurl-browse-url-other-window "browse other window")
                   ("x" clipurl-browse-url-external-browser "browse externally")))

(provide 'ivy-clipurl)
;;; ivy-clipurl.el ends here
