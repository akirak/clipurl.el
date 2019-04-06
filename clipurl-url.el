;;; clipurl-url.el --- URL regular expressions

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12") (s "1.12"))
;; Keywords: matching
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

;; This library provides regular expressions to be used in clipurl.el.

;;; Code:


(defconst clipurl-url-regexp
  (eval-when-compile
    (let* ((safe "$-_@.&+-")
           (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex)))
           (xalpha `(or (char alpha digit ,safe ,extra) ,escape))
           (ialpha `(and (char alpha) (* ,xalpha)))
           (hostname `(and ,ialpha (* (and "." ,ialpha))))
           (hostnumber '(and (+ (char digit))
                             (repeat 3 (and "." (+ (char digit))))))
           (host `(or ,hostname ,hostnumber))
           (port '(+ (char digit)))
           (segment `(+ ,xalpha))
           (search `(and (+ ,xalpha) (* "+" (+ ,xalpha))))
           (fragment `(+ (or (+ ,xalpha) (char "/?")))))
      (rx "http" (?  "s")
          "://"
          (eval host)
          (?  ":" (eval port))
          (?  (or "/" (and (+ "/" (eval segment)) (?  "/"))))
          (?  "?" (eval search))
          (?  "#" (eval fragment))))))

(provide 'clipurl-url)
;;; clipurl-url.el ends here
