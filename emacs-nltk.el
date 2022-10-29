;;; emacs-nltk.el --- Emacs plugin using nltk        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'epc)
(require 'emacs-nltk-epcs)

(defvar emacs-nltk-epc nil)

(defun nltk-epcs-start()
  (when (not nltk-epcs:server-processes)
    (let ((connect-function
           (lambda (mngr)
             (epc:define-method mngr 'nltk-overlay-current-line-from
                                'nltk-overlay-current-line-from)
             (epc:define-method mngr 'nltk-echo 'nltk-echo))))
      (nltk-epcs:server-start connect-function))))

(defun nltk-get-server-port ()
  (when nltk-epcs:server-processes
    (cl-struct-slot-value 'nltk-epcs:server 'port
                          (cdr (car nltk-epcs:server-processes)))))

(defun nltk-start()
  (interactive)
  (nltk-epcs-start)
  (setq emacs-nltk-epc (epc:start-epc "python" '("emacs-nltk.py")))
  (epc:call-sync emacs-nltk-epc 'set_epcs_port
                 (list (nltk-get-server-port))))

(defun nltk-restart() (interactive) (nltk-stop) (nltk-start))

(defun nltk-parse-current-line ()
  (interactive)
  (epc:call-sync emacs-nltk-epc 'parse_sentence
                 (list (thing-at-point 'line t))))

(defun nltk-overlay-current-line-from (begin end)
  (let ((ov
         (make-overlay
          (+ (line-beginning-position) begin)
          (+ (line-beginning-position) end))))
    (overlay-put ov 'face 'bold)))

(defun ntl-classify-word ()
  "Get current word class."
  (interactive)
  (epc:call-sync emacs-nltk-epc 'classify_word
                 (list
                  (thing-at-point 'line t)
                  (- (point) (line-beginning-position)))))

(defun nltk-stop ()
  (cl-loop for mngr in epc:live-connections collect
           (epc:stop-epc mngr))
  (setq nltk-epcs:server-processes nil emacs-nltk-epc nil))

(defun nltk-echo(msg) (print msg))

(provide 'emacs-nltk)
;;; emacs-nltk.el ends here



