;;; helm-symbol-hint.el --- Show symbol hint for Helm -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2021/04/10
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (popup "0.5.8") (helm "3.6.2"))
;; URL: https://github.com/twlz0ne/helm-symbol-hint
;; Keywords: tools

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

;; Show symbol hint for helm.
;;
;; This project was inspired by [[https://github.com/emacs-helm/helm/blob/4f16ec21f5ac4d0b9e36768c27abd453a959388d/helm-utils.el#L1037][helm-popup-tip-mode]].
;;
;; * Installing
;;
;; #+begin_src emacs-lisp
;; (quelpa '(helm-symbol-hint
;;           :repo "twlz0ne/helm-symbol-hint"
;;           :fetcher github
;;           :files ("*.el")))
;; #+end_src
;;
;; * Usage
;;
;; Just =M-x helm-symbol-hint-mode RET=.
;;
;; ** Options
;;
;; - =helm-symbol-hint-delay= (default =0.5=)
;;
;;   Seconds to wait before start timer.
;;
;; - =helm-symbol-hint-popup-p= (default =t=)
;;
;;   Non-nil show hint in popup, otherwish show hint in echo area.

;;; Change Log:

;;  0.1.0  2021/04/10  Initial version.

;;; Code:

(require 'subr-x)
(require 'popup)
(require 'helm)

(defface helm-symbol-hint-face
  '((t (:inherit popup-tip-face :slant italic)))
  "Face for helm symbol hint."
  :group 'helm-symbol-hint)

(defvar helm-symbol-hint-buffers '("Emacs Commands history"
                                   "Emacs Commands"
                                   "describe-function"
                                   "describe-variable")
  "A list of helm buffers where need show symbol hint.")

(defvar helm-symbol-hint-popup-p t
  "Non-nil show hint in popup, otherwish show hint in echo area.")

(defvar helm-symbol-hint-delay 0.5
  "Seconds to wait before start timer.")

(defvar helm-symbol-hint--timer nil
  "Timer of helm symbol hint.")

(defvar helm-symbol-hint-advice-re
  (rx bos (1+ (seq
               ;; 27 or newer
               (? "This function has ")
               ;; see ‘advice--where-alist’
               (| ":before" ":before-until" ":before-while"
                  ":around" ":override" ":filter-args" ":filter-return"
                  ":after" ":after-until" ":after-while")
               " advice: " (0+ nonl) "\n"))
      "\n")
  "Regexp to match the advice in documentation.")

(defun helm-symbol-hint-1 (symbol-name)
  "Return useful one-line documentation of SYMBOL-NAME."
  (let* ((symbol (intern symbol-name)))
    (if (fboundp symbol)
        (let ((doc (documentation symbol t)))
          (if (and doc (not (string-empty-p doc)))
              (elisp--docstring-first-line
               (string-trim-left
                (replace-regexp-in-string helm-symbol-hint-advice-re "" doc)))
            ;; function: (ARG) --> (fn ARG)
            (replace-regexp-in-string (if (<= 28 emacs-major-version)
                                          "^(\\(fn\s?\\)?"
                                        (concat "^" symbol-name ": ("))
                                      "(fn "
                                      (elisp-get-fnsym-args-string symbol))))
      (or (let ((doc (documentation-property symbol 'variable-documentation t)))
            (elisp--docstring-first-line doc))
          "Not documented."))))

(defun helm-symbol-hint-cancel-timer ()
  "Cancle the symbol hint timer."
  (when helm-symbol-hint--timer
    (cancel-timer helm-symbol-hint--timer)
    (setq helm-symbol-hint--timer nil)))

(defvar helm-symbol-hint-indicator
  (propertize
   " " 'face `(:foreground ,(face-background 'helm-symbol-hint-face nil t)
                :background ,(face-background 'helm-selection)
                :slant italic))
  "String to indicate the hint.")

(defun helm-symbol-hint-start-timer ()
  "Start the symbol hint timer."
  (when (and helm-alive-p
             helm-symbol-hint-mode
             (member (assoc-default 'name (helm-get-current-source))
                     helm-symbol-hint-buffers))
    (helm-symbol-hint-cancel-timer)
    (setq helm-symbol-hint--timer
          (run-with-timer
           helm-symbol-hint-delay nil
           (lambda ()
             (when helm-alive-p
               (save-selected-window
                 (with-helm-window
                   (when-let
                       ((selection (helm-current-line-contents))
                        (not-empty-p (not (string-empty-p selection)))
                        (hint (helm-symbol-hint-1 selection)))
                     (if helm-symbol-hint-popup-p
                         (popup-tip
                          (concat helm-symbol-hint-indicator
                                  (propertize " " 'face nil)
                                  (propertize hint 'face 'helm-symbol-hint-face))
                          :nostrip t
                          :around nil
                          :point (save-excursion
                                   (end-of-visual-line) (point)))
                       (eldoc-message hint)))))))))))

(define-minor-mode helm-symbol-hint-mode
  "Show symbol hint for helm."
  :global t
  (if helm-symbol-hint-mode
      (progn
        (add-hook 'helm-move-selection-after-hook 'helm-symbol-hint-start-timer)
        (add-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer))
    (remove-hook 'helm-move-selection-after-hook 'helm-symbol-hint-start-timer)
    (remove-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer)))

(provide 'helm-symbol-hint)

;;; helm-symbol-hint.el ends here
