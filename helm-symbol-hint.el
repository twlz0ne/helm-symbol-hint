;;; helm-symbol-hint.el --- Show symbol hint for Helm -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2021/04/10
;; Version: 0.1.0
;; Last-Updated: 2023-10-31 11:28:47 +0800
;;           by: Gong Qijian
;; Package-Requires: ((emacs "25.1") (helm "3.6.2"))
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
;; - =helm-symbol-hint-delay= (default =0.5=)
;;
;;   Seconds to wait before show current in echo area.
;;   Nil means don't show current value.
;;
;; - =helm-symbol-hint-window-spec= (defualt =(0.3 . helm-buffer-size)=)
;;
;;   Specification of symbol hint window.
;;   It is in the form of (SYMBOL-WIDTH . HINT-FACE).  SYMBOL-WIDTH is a float
;;   number from 0.0 to 1.0 meas the percentage of the window width.

;;; Change Log:

;;  0.1.0  2021/04/10  Initial version.

;;; Code:

(require 'subr-x)
(require 'helm)

(defvar helm-symbol-hint-source-method-alist
  '(("Emacs Commands history" . helm-symbol-hint--function-summary)
    ("Emacs Commands"         . helm-symbol-hint--function-summary)
    ("describe-function"      . helm-symbol-hint--function-summary)
    ("describe-variable"      . helm-symbol-hint--variable-summary)
    ("describe-command"       . helm-symbol-hint--function-summary)
    ("completion-at-point"    . helm-symbol-hint--function-summary)
    ("Imenu"                  . helm-symbol-hint--imenu-function-hint)
    ("describe-package"       . helm-symbol-hint--package-summary)
    ("package-install"        . helm-symbol-hint--package-summary)
    ("package-reinstall"      . helm-symbol-hint--package-summary)
    ("package-delete"         . helm-symbol-hint--installed-package-summary))
  "A list of (HELM-SOURCE . SUMMARY-METHOD).")

(defvar helm-symbol-hint-show-current-sources
  '("describe-variable" "describe-function" "describe-command")
  "A list of source enable current value in echo area.")

(defcustom helm-symbol-hint-delay 0.5
  "Seconds to wait before show current value in echo area.

Nil means don't show current value."
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

(defcustom helm-symbol-hint-window-spec
  '(0.3 . helm-buffer-size)
  "Specification of symbol hint window.
It is in the form of (SYMBOL-WIDTH . HINT-FACE).  SYMBOL-WIDTH is a float number
from 0.0 to 1.0 meas the percentage of the window width."
  :group 'helm-symbol-hint
  :type 'cons)

(defvar helm-symbol-hint-imenu-type-alist '((emacs-lisp-mode defadvice
                                                             define-advice
                                                             cl-defgeneric
                                                             defgeneric
                                                             cl-defmethod
                                                             defmethod))
  "An alist specifying which type of symbol hint will be displayed.

Each element of it is in the form of (MAJOR-MODE . TYPE-LIST), e.g.:

    (emacs-lisp-mode . (defadvice
                        define-advice
                        cl-defgeneric
                        defgeneric
                        cl-defmethod
                        defmethod))")

(defvar-local helm-symbol-hint--current-imenu-types nil "Current imenu types.")

(defun helm-symbol-hint--hint-method ()
  "Return hint method of current source."
  (setq helm-symbol-hint--current-imenu-types
        (with-current-buffer helm-current-buffer
          (cdr (assq major-mode helm-symbol-hint-imenu-type-alist))))
  (assoc-default (assoc-default 'name (helm-get-current-source))
                 helm-symbol-hint-source-method-alist))

(defvar helm-symbol-hint--echo-string-p nil)

(defvar helm-symbol-hint--preselected-p nil "A non-first candidate is pre-selected.")

(defvar helm-symbol-hint--method nil)
(defvar helm-symbol-hint--source nil)

(defun helm-symbol-hint--variable-summary (symbol-name)
  "Return useful one-line documentation of variable SYMBOL-NAME."
  (let ((symbol (intern symbol-name)))
    (if helm-symbol-hint--echo-string-p
        (concat "Value: "
                (let ((val (or (buffer-local-value symbol (current-buffer))
                               (symbol-value symbol))))
                  (if (stringp val) val (format "%S" val))))
      (or (let ((doc (documentation-property symbol 'variable-documentation t)))
            (elisp--docstring-first-line doc))
          "Not documented."))))

(defun helm-symbol-hint--function-summary (symbol-name)
  "Return useful one-line documentation of function SYMBOL-NAME."
  (let* ((symbol (intern symbol-name)))
    (if helm-symbol-hint--echo-string-p
        (concat "Arg: " (elisp-get-fnsym-args-string symbol))
      (let ((doc (condition-case _err
                     (documentation symbol t)
                   (wrong-type-argument "[Failed to read document.]"))))
        (if (and doc (not (string-empty-p doc)))
            (elisp--docstring-first-line
             (string-trim-left
              (replace-regexp-in-string helm-symbol-hint-advice-re "" doc)))
          (let ((arg-str (elisp-get-fnsym-args-string symbol)))
            (if arg-str
                ;; function: (ARG) --> (fn ARG)
                (replace-regexp-in-string (if (<= 28 emacs-major-version)
                                              "^(\\(fn\s?\\)?"
                                            (concat "^" symbol-name ": ("))
                                          "(fn "
                                          arg-str)
              "Not documented.")))))))

(defun helm-symbol-hint--imenu-function-hint (menu-item)
  "Return hint of function that MENU-ITEM pointed to."
  (when helm-symbol-hint--current-imenu-types
    (let* ((types helm-symbol-hint--current-imenu-types)
           (point (marker-position (get-text-property 0 'position menu-item)))
           (sexp (with-current-buffer helm-current-buffer
                   (when types
                     (save-excursion
                       (goto-char point)
                       (sexp-at-point))))))
      (when (and (consp sexp) (memq (nth 0 sexp) types))
        (format "%s" (nth 2 sexp))))))

(defun helm-symbol-hint--advice-around-imenu-action (fn candidate)
  "Advice around `helm-imenu-action' (FN) to restore the CANDIDATE."
  (funcall fn (if (stringp candidate)
                  (cons candidate (get-text-property 0 'position candidate))
                candidate)))

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
                     ((helm-symbol-hint--echo-string-p t)
                      (sym-str (helm-symbol-hint--current-symbol-name))
                      (not-empty-p (not (string-empty-p sym-str)))
                      (hint-method (helm-symbol-hint--hint-method))
                      (hint
                       (with-current-buffer helm-current-buffer
                         (funcall hint-method (string-trim-right sym-str)))))
                   ;; (message "==> \tsym-str: %s \n\tmethod: %s \n\thint: %s" sym-str hint-method hint)
                   ;; (message "==> minibuffer prompt: %s input: %s" (minibuffer-prompt) helm-input)
                   (funcall eldoc-message-function
                            (truncate-string-to-width
                             hint
                             (- (window-text-width (minibuffer-window))
                                (length (minibuffer-prompt))
                                (length helm-input)
                                4)
                             nil nil t))))))))))

(defun helm-symbol-hint--show-all (&optional scroll-p)
  "Show all symbol hints.
If SCROLL-P not nil, consider as mouse wheel scrolling."
  (let* ((face (cdr helm-symbol-hint-window-spec))
         (hint-method helm-symbol-hint--method)
         (scroll-p (or scroll-p helm-symbol-hint--preselected-p))
         (window-end (if (< (window-end) 0) (1+ (buffer-size)) (window-end)))
         (sym-width
          (round (* (car helm-symbol-hint-window-spec) (window-width))))
         (init-p (or (and (= (point-max) window-end)
                          (= (point-min) (window-start)))
                     (and (not this-command)
                          (memq last-command '(self-insert-command yank)))))
         (draw-down-p (or scroll-p
                          (not (memq this-command
                                     '(helm-previous-line helm-previous-page
                                       helm-end-of-buffer)))))
         (line-move-step (if draw-down-p 1 -1))
         (point-at-edge-p
          (if (or init-p helm-symbol-hint--preselected-p)  nil
            (<= (if draw-down-p
                    (- (line-number-at-pos window-end) (line-number-at-pos))
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
            (goto-char (window-start))
            (when (helm-pos-header-line-p)
              (forward-line 1)))
          (while (< num max)
            (cl-incf num)
            (when-let ((not-header-p (not (helm-pos-header-line-p) ))
                       (sym-str (helm-symbol-hint--current-symbol-name))
                       (not-empty-p (not (string-empty-p sym-str))))
              (setq key-str nil)
              (unless (funcall check-hint-fn (line-end-position))
                (add-text-properties
                 (line-beginning-position) (line-end-position) (list 'helm-realvalue sym-str))
                (goto-char (line-end-position))
                (when-let* ((hint (funcall hint-method sym-str))
                            (disp (if key-str
                                      (concat sym-str "  " key-str)
                                    (helm-symbol-hint--current-symbol-display)))
                            (padding-width (- sym-width
                                              (if key-str (1- (length key-str)) 0)
                                              (- (line-end-position) (line-beginning-position) 1))))
                  (if (< 1 padding-width)
                      (insert (make-string padding-width ?\s))
                    (add-text-properties
                     (line-end-position) (line-beginning-position)
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
      (unless (equal (and helm-symbol-hint--source
                          (assoc-default 'name helm-symbol-hint--source))
                     (assoc-default 'name (helm-get-current-source)))
        (setq helm-symbol-hint--method (helm-symbol-hint--hint-method)))
      (setq helm-symbol-hint--source (helm-get-current-source))
      (helm-symbol-hint--show-all scroll-p)
      (when (member (assoc-default 'name (helm-get-current-source))
                    helm-symbol-hint-show-current-sources)
        (when helm-symbol-hint-delay
          (helm-symbol-hint--show-current))))))

(defun helm-symbol-hint--show-hint-cap (&optional scroll-p)
  "Show hint for source `completing-at-point'.
If SCROLL-P not nil, consider as mouse wheel scrolling."
  (when (and helm-alive-p (helm-symbol-hint--hint-method))
    (with-helm-window
      (setq helm-symbol-hint--method
            (lambda (symbol-name)
              (let ((symbol (intern symbol-name)))
                (cond
                 ((functionp symbol)
                  (helm-symbol-hint--function-summary symbol-name))
                 ((keywordp symbol) "Not documented.")
                 (t (helm-symbol-hint--variable-summary symbol-name))))))
      (setq helm-symbol-hint--source (helm-get-current-source))
      (helm-symbol-hint--show-all scroll-p)
      (when (member (assoc-default 'name (helm-get-current-source))
                    helm-symbol-hint-show-current-sources)
        (when helm-symbol-hint-delay
          (helm-symbol-hint--show-current))))))

(defun helm-symbol-hint--window-scroll (window start)
  "Function to be used in `window-scroll-functions'.
WINDOW is the window where the mouse scrolling.  START is the starting position
of the window."
  (when (equal window (helm-window))
    (with-helm-window
      (let ((last-start helm-symbol-hint--last-window-start))
        (setq helm-symbol-hint--last-window-start start)
        (when (and last-start (not (equal last-start start)))
          (helm-symbol-hint--show-hint t)
          (setq helm-symbol-hint--preselected-p nil))))))

(defun helm-symbol-hint--prepare ()
  "Function to be used in `helm-after-update-hook' to do some preparations."
  (when (and helm-alive-p (helm-symbol-hint--hint-method))
    (with-helm-buffer
      (setq-local truncate-lines t))))

(defun helm-symbol-hint--after-preselect ()
  "Function to be use in `helm-after-preselection-hook'.

For situations (e.g. invoking the iemnu inside a function) that a non-first
candidate is pre-selected."
  (when (> (with-helm-buffer (line-number-at-pos)) 1)
    (setq helm-symbol-hint--preselected-p t)))

(defun helm-symbol-hint--current-symbol-name ()
  "Return the name of current selected symbol."
  (let ((v (get-text-property (line-beginning-position) 'helm-realvalue)))
    (cond ((null v)
           (string-trim-right
            (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          ((stringp v) v)
          ((and (consp v) (stringp (car v))) ;; Imenu item
           (propertize (car v) 'position (cdr v)))
          (t (error "Unknown helm-realvalue: %s" v)))))

(defun helm-symbol-hint--string-trim-right (str)
  "Remove trailing spaces of STR for `completion-at-point'."
  (if (and (stringp str) (helm-symbol-hint--hint-method))
      (string-trim-right str)
    str))

(defun helm-symbol-hint--current-symbol-display ()
  "Return the display of current selected symbol."
  (buffer-substring (line-beginning-position) (line-end-position)))

;;;###autoload
(define-minor-mode helm-symbol-hint-mode
  "Show symbol hint for helm."
  :global t
  :group 'helm-symbol-hint
  (if helm-symbol-hint-mode
      (progn
        (add-hook 'helm-after-update-hook #'helm-symbol-hint--prepare)
        (add-hook 'helm-minibuffer-set-up-hook 'helm-symbol-hint--show-hint-cap)
        (add-hook 'helm-move-selection-after-hook 'helm-symbol-hint--show-hint)
        (add-hook 'window-scroll-functions 'helm-symbol-hint--window-scroll)
        (add-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer)
        (add-hook 'helm-after-preselection-hook 'helm-symbol-hint--after-preselect)
        (advice-add 'helm-get-selection :filter-return
                    'helm-symbol-hint--string-trim-right)
        (advice-add 'helm-imenu-action
                    :around #'helm-symbol-hint--advice-around-imenu-action)
        (advice-add 'helm-imenu-persistent-action
                    :around #'helm-symbol-hint--advice-around-imenu-action))
    (remove-hook 'helm-after-update-hook #'helm-symbol-hint--prepare)
    (remove-hook 'helm-minibuffer-set-up-hook 'helm-symbol-hint--show-hint-cap)
    (remove-hook 'helm-move-selection-after-hook 'helm-symbol-hint--show-hint)
    (remove-hook 'window-scroll-functions 'helm-symbol-hint--window-scroll)
    (remove-hook 'helm-cleanup-hook 'helm-symbol-hint-cancel-timer)
    (remove-hook 'helm-after-preselection-hook 'helm-symbol-hint--after-preselect)
    (advice-remove 'helm-get-selection 'helm-symbol-hint--string-trim-right)
    (advice-remove 'helm-imenu-action
                   #'helm-symbol-hint--advice-around-imenu-action)
    (advice-remove 'helm-imenu-persistent-action
                   #'helm-symbol-hint--advice-around-imenu-action)))

(provide 'helm-symbol-hint)

;;; helm-symbol-hint.el ends here
