;;; clipurl.el --- Work with URLs in the kill ring -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12") (s "1.12"))
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

;; This library provides a set of commands to operate on URLs in
;; the kill ring and the system clipboard.

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)

(autoload 'browse-url-default-browser "browse-url")
(declare-function 'browse-url-default-browser "browse-url")
(autoload 'org-web-tools-read-url-as-org "org-web-tools")
(declare-function 'org-web-tools-read-url-as-org "org-web-tools")
(autoload 'org-make-link-string "org")
(declare-function 'org-make-link-string "org")

(defgroup clipurl nil
  "Operations on URLs in the kill ring."
  :group 'convenience)

(defconst clipurl-url-regexp
  (rx "http" (?  "s") "://"
      (* (+ (or (char "-") wordchar)) ".")
      (or "localhost" (repeat 2 6 lower))
      (* "/" (* (or wordchar (char "-=.%_@"))))
      (?  "?" (* (or wordchar (char "-=&%"))))
      (?  "#" (* (or wordchar (char "-"))))))

(defcustom clipurl-browse-url-function
  #'browse-url-default-browser
  "Function used to browse a URL."
  :group 'clipurl
  :type '(choice (const :tag "Default browser" browse-url-default-browser)
                 (const eww)
                 (const org-web-tools-read-url-as-org)
                 function))

(defcustom clipurl-browse-url-other-window-function
  #'clipurl--eww-other-window
  "Function used to browse a URL in other window."
  :group 'clipurl
  :type '(choice (const :tag "Eww in other window" clipurl--eww-other-window)
                 (const :tag "org-web-tools-read-url-as-org in other window"
                        clipurl--org-web-tools-read-url-as-org-other-window)
                 function))

(defcustom clipurl-external-browser-function
  #'browse-url-default-browser
  "Function used to browse a URL using an external browser."
  :group 'clipurl
  :type '(choice (const browse-url-default-browser)
                 (const browse-url-firefox)
                 (const browse-url-chromium)
                 (const browse-url-chrome)
                 function))

(defcustom clipurl-use-org-web-tools-insert-link-for-url t
  "Use `org-web-tools-insert-link-for-url' to insert a URL.

If this variable is non-nil, org-web-tools is used to produce a
link in some modes like `org-mode' and `markdown-mode'.  That is,
a title is fetched from an actual web page of the URL and used as
the link text."
  :group 'clipurl
  :type 'boolean)

(defun clipurl--get-selection ()
  "Get selections.

This function returns a list of strings."
  (let ((selection (funcall interprogram-paste-function)))
    (if (listp selection)
        selection
      (list selection))))

(defun clipurl--url-source-in-string-list ()
  "Return a list of clipboard contents appended by the kill ring."
  (append (clipurl--get-selection)
          kill-ring))

(defun clipurl--urls-in-string (string)
  "Extract URLs from STRING."
  (->> string
       (s-match-strings-all clipurl-url-regexp)
       (-flatten-n 1)
       (-map #'substring-no-properties)))

(defun clipurl--urls-in-string-list (string-list)
  "Extract URLs from STRING-LIST."
  (cl-remove-duplicates
   (->> string-list
        (-map #'clipurl--urls-in-string)
        (-flatten-n 1))
   :test #'string-equal))

(defun clipurl--urls-in-kill-ring ()
  "Extract URLs from the clipboard and the kill ring."
  (clipurl--urls-in-string-list (clipurl--url-source-in-string-list)))

(defun clipurl--complete-url-in-kill-ring (&rest _args)
  "Completion function for a URL in the kill ring."
  (clipurl--urls-in-kill-ring))

(defun clipurl-complete-url (prompt)
  "Complete a URL with contents in the kill ring (with PROMPT)."
  (completing-read prompt (clipurl--urls-in-kill-ring)))

(defmacro clipurl--run-in-other-window (&rest progn)
  "Macro to run PROGN in other window."
  `(progn
     (if (> (length (window-list)) 1)
         (other-window 1)
       (split-window-sensibly))
     ,@progn))

(defun clipurl--eww-other-window (url)
  "Open URL with eww in other window."
  (clipurl--run-in-other-window
   (eww-browse-url url)))

(defun clipurl--org-web-tools-read-url-as-org-other-window (url)
  "Open URL with org-web-tools in other window."
  (clipurl--run-in-other-window
   (org-web-tools-read-url-as-org url)))

;;;###autoload
(defun clipurl-browse-url (url)
  "Browse URL in the kill ring.

You can customize the browser through `clipurl-browse-url-function'."
  (interactive
   (list (clipurl-complete-url "Browse URL: ")))
  (funcall clipurl-browse-url-function url))

;;;###autoload
(defun clipurl-browse-url-other-window (url)
  "Browse URL in the kill ring in other window.

You can customize the browse through
`clipurl-browse-url-other-window-function'."
  (interactive
   (list (clipurl-complete-url "Browse URL (other window): ")))
  (if clipurl-browse-url-other-window-function
      (funcall clipurl-browse-url-other-window-function url)
    (funcall clipurl-browse-url-function url)))

;;;###autoload
(defun clipurl-browse-url-external-browser (url)
  "Browse URL in the kill ring using an external browser.

You can customize the browse through
`clipurl-browse-url-external-browser'."
  (interactive
   (list (clipurl-complete-url "Browse URL (external): ")))
  (funcall clipurl-external-browser-function url))

;;;###autoload
(defun clipurl-insert-url-dwim (url)
  "Insert URL in the kill ring into the current buffer.

In certain modes, e.g. `org-mode' and `markdown-mode', this
function inserts a link to the URL rather than the URL itself.
The link title is fetched by `org-web-tools' package if and only
if it is available and
`clipurl-use-org-web-tools-insert-link-for-url' is set to
non-nil.  Otherwise, this function inserts the URL itself."
  (interactive
   (list (clipurl-complete-url "Insert a link or URL: ")))
  (cond
   ((derived-mode-p 'org-mode)
    (if (and (fboundp 'org-web-tools-insert-link-for-url)
             clipurl-use-org-web-tools-insert-link-for-url)
        (org-web-tools-insert-link-for-url url)
      (let ((title (read-string (format "Title for the link to %s: " url))))
        (org-make-link-string url title))))
   ((derived-mode-p 'markdown-mode)
    (insert (format "[%s](%s)"
                    (or (and (fboundp 'org-web-tools-insert-link-for-url)
                             clipurl-use-org-web-tools-insert-link-for-url
                             (if (fboundp 'org-web-tools--html-title)
                                 (ignore-errors
                                   (org-web-tools--html-title url))
                               (error "Undefined function: org-web-tools--html-title")))
                        (read-string (format "Title for the link to %s: " url)))
                    url)))
   (t (insert url))))

(provide 'clipurl)
;;; clipurl.el ends here
