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
;; * Customization
;;
;; - =helm-symbol-hint-style= (default =grid=)
;;
;;   Specify how the symbol hint display.
;;
;;   - =grid= Show all symbol hints at once.
;;   - =popup= Show current symbol hint in popup.
;;   - =echo= Show current symbol hint in echo area.
;;
;; - =helm-symbol-hint-delay= (default =0.5=)
;;
;;   Seconds to wait before start popup/echo timer.
;;
;; - =helm-symbol-hint-grid-spec= (defualt =(0.3 . helm-buffer-size)=)
;;
;;   Specification of symbol hint grid.
;;   It is in the form of (SYMBOL-WIDTH . HINT-FACE).  SYMBOL-WIDTH is a float
;;   number from 0.0 to 1.0 meas the percentage of the window width.

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

(defvar helm-symbol-hint-source-method-alist
  '(("Emacs Commands history" . helm-symbol-hint--symbol-summary)
    ("Emacs Commands"         . helm-symbol-hint--symbol-summary)
    ("describe-function"      . helm-symbol-hint--symbol-summary)
    ("describe-variable"      . helm-symbol-hint--symbol-summary)
    ("describe-command"       . helm-symbol-hint--symbol-summary)
    ("describe-package"       . helm-symbol-hint--package-summary)
    ("package-install"        . helm-symbol-hint--package-summary)
    ("package-reinstall"      . helm-symbol-hint--package-summary)
    ("package-delete"         . helm-symbol-hint--installed-package-summary))
  "A list of (HELM-SOURCE . SUMMARY-METHOD).")

(defcustom helm-symbol-hint-delay 0.5
  "Seconds to wait before start popup/echo timer."
  :group 'helm-symbol-hint
  :type 'float)

(defvar helm-symbol-hint--timer nil
  "Timer of helm symbol hint.")

(defvar-local helm-symbol-hint--last-window-start nil
  "The last position of helm window start.")

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

(defcustom helm-symbol-hint-style 'grid
  "Specify how the symbol hint display."
  :group 'helm-symbol-hint
  :type '(choice
          (const :tag "Show all symbol hints at once" grid)
          (const :tag "Show current symbol hint in popup" popup)
          (const :tag "Show current symbol hint in echo area" echo)))

(defcustom helm-symbol-hint-grid-spec
  '(0.3 . helm-buffer-size)
  "Specification of symbol hint grid.
It is in the form of (SYMBOL-WIDTH . HINT-FACE).  SYMBOL-WIDTH is a float number
from 0.0 to 1.0 meas the percentage of the window width."
  :group 'helm-symbol-hint
  :type 'cons)

(defun helm-symbol-hint--hint-method ()
  "Return hint method of current source."
  (assoc-default (assoc-default 'name (helm-get-current-source))
                 helm-symbol-hint-source-method-alist))

(defun helm-symbol-hint--symbol-summary (symbol-name)
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

(defun helm-symbol-hint--package-summary (package-name)
  "Return summary of PACKAGE-NAME."
  (let* ((pkg (intern package-name))
         (desc (or
                (if (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents)))))))
    (let ((summary (when desc (package-desc-summary desc))))
      (if (and summary (not (string= "" summary)))
          summary
        "Not documented."))))

(defun helm-symbol-hint--installed-package-summary (installed-package-name)
  "Return summary of INSTALLED-PACKAGE-NAME."
  (helm-symbol-hint--package-summary (replace-regexp-in-string
                                      "-[0-9]+\\.[0-9]+\\'" ""
                                      installed-package-name)))

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

(defun helm-symbol-hint--show-current ()
  "Start the symbol hint timer."
  (helm-symbol-hint-cancel-timer)
  (setq helm-symbol-hint--timer
        (run-with-timer
         helm-symbol-hint-delay nil
         (lambda ()
           (when helm-alive-p
             (save-selected-window
               (with-helm-window
                 (when-let
                     ((sym-str (helm-symbol-hint--current-symbol-name))
                      (not-empty-p (not (string-empty-p sym-str)))
                      (hint-method (helm-symbol-hint--hint-method))
                      (hint (funcall hint-method (string-trim-right sym-str))))
                   (if (equal helm-symbol-hint-style 'popup)
                       (popup-tip
                        (concat helm-symbol-hint-indicator
                                (propertize " " 'face nil)
                                (propertize hint 'face 'helm-symbol-hint-face))
                        :nostrip t
                        :around nil
                        :point (save-excursion
                                 (end-of-visual-line) (point)))
                     (eldoc-message hint))))))))))

(defun helm-symbol-hint--show-all (&optional scroll-p)
  "Show all symbol hints.
If SCROLL-P not nil, consider as mouse wheel scrolling."
  (let* ((face (cdr helm-symbol-hint-grid-spec))
         (hint-method (helm-symbol-hint--hint-method))
         (sym-width
          (round (* (car helm-symbol-hint-grid-spec) (window-width))))
         (init-p (or (and (= (point-max) (window-end))
                          (= (point-min) (window-start)))
                     (and (not this-command)
                          (memq last-command '(self-insert-command yank)))))
         (draw-down-p (or scroll-p
                          (not (memq this-command
                                     '(helm-previous-line helm-previous-page
                                       helm-end-of-buffer)))))
         (line-move-step (if draw-down-p 1 -1))
         (point-at-edge-p
          (if init-p nil
            (<= (if draw-down-p
                    (- (line-number-at-pos (window-end)) (line-number-at-pos))
                  (- (line-number-at-pos) (line-number-at-pos (window-start))))
                1)))
         (page-update-p (or init-p
                            point-at-edge-p
                            (not (memq this-command '(helm-next-line
                                                      helm-previous-line)))))
         (max (if point-at-edge-p
                  (1+ (/ (window-height) 2))
                (window-height)))
         (num 0)
         key-str
         (check-hint-fn
          (lambda (point)
            (and (equal ?\s (char-before point))
                 (when-let ((disp (get-text-property (1- point) 'display)))
                   (if (equal (get-text-property 0 'face disp) 'helm-M-x-key)
                       (progn (setq key-str disp) nil)
                     t))))))
    (when page-update-p
      (save-excursion
        (catch 'break
          (when scroll-p
            (goto-char (window-start)))
          (while (< num max)
            (cl-incf num)
            (when-let ((not-header-p (not (helm-pos-header-line-p) ))
                       (sym-str (helm-symbol-hint--current-symbol-name))
                       (not-empty-p (not (string-empty-p sym-str))))
              (setq key-str nil)
              (unless (funcall check-hint-fn (point-at-eol))
                (add-text-properties
                 (point-at-bol) (point-at-eol) (list 'helm-realvalue sym-str))
                (goto-char (point-at-eol))
                (let* ((hint (funcall hint-method sym-str))
                       (disp (if key-str
                                 (concat sym-str "  " key-str)
                               (helm-symbol-hint--current-symbol-display)))
                       (padding-width (- sym-width
                                         (if key-str (1- (length key-str)) 0)
                                         (- (point-at-eol) (point-at-bol) 1))))
                  (if (< 1 padding-width)
                      (insert (make-string padding-width ?\s))
                    (add-text-properties
                     (point-at-eol) (point-at-bol)
                     (list 'display
                           (propertize
                            (truncate-string-to-width
                             disp (- sym-width 1) nil nil "…"))))
                    (insert "  "))
                  (put-text-property
                   (- (point) 1) (point)
                   'display (propertize hint 'face face)))))
            (unless (zerop (forward-line line-move-step))
              (throw 'break nil))))))))

(defun helm-symbol-hint--show-hint (&optional scroll-p)
  "Function to be used in `helm-move-selection-after-hook' to show hint.
If SCROLL-P not nil, consider as mouse wheel scrolling."
  (when (and helm-alive-p (helm-symbol-hint--hint-method))
    (with-helm-window
      (if (equal helm-symbol-hint-style 'grid)
          (helm-symbol-hint--show-all scroll-p)
        (helm-symbol-hint--show-current)))))

(defun helm-symbol-hint--window-scroll (window start)
  "Function to be used in `window-scroll-functions'.
WINDOW is the window where the mouse scrolling.  START is the starting position
of the window."
  (when (equal window (helm-window))
    (with-helm-window
      (let ((last-start helm-symbol-hint--last-window-start))
        (setq helm-symbol-hint--last-window-start start)
        (when (and last-start (not (equal last-start start)))
          (helm-symbol-hint--show-hint t))))))

(defun helm-symbol-hint--prepare ()
  "Function to be used in `helm-after-update-hook' to do some preparations."
  (when (and helm-alive-p (helm-symbol-hint--hint-method))
    (with-helm-buffer
      (setq-local truncate-lines t))))

(defun helm-symbol-hint--current-symbol-name ()
  "Return the name of current selected symbol."
  (or (get-text-property (point-at-bol) 'helm-realvalue)
      (replace-regexp-in-string
       "\\([^\s]+\\).*" "\\1"
       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

(defun helm-symbol-hint--current-symbol-display ()
  "Return the display of current selected symbol."
  (buffer-substring (point-at-bol) (point-at-eol)))

;;;###autoload
(define-minor-mode helm-symbol-hint-mode
  "Show symbol hint for helm."
  :global t
  (if helm-symbol-hint-mode
      (progn
        (add-hook 'helm-after-update-hook #'helm-symbol-hint--prepare)
        (add-hook 'helm-move-selection-after-hook 'helm-symbol-hint--show-hint)
        (add-hook 'window-scroll-functions 'helm-symbol-hint--window-scroll)
        (add-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer))
    (remove-hook 'helm-after-update-hook #'helm-symbol-hint--prepare)
    (remove-hook 'helm-move-selection-after-hook 'helm-symbol-hint--show-hint)
    (remove-hook 'window-scroll-functions 'helm-symbol-hint--window-scroll)
    (remove-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer)))

(provide 'helm-symbol-hint)

;;; helm-symbol-hint.el ends here
