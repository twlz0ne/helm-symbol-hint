;;; helm-symbol-hint-test.el --- tests for helm-symbol-hint.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'helm-symbol-hint)


;;; utils

(defun elisp-get-fnsym-args-string@fix-not-documented (orig-fn sym &optional index)
  "Fix not documented for older Emacs."
  (let ((argsdoc (elisp-function-argstring (help-function-arglist sym))))
    (if (<= (length argsdoc) 9)
        (concat (format "%s: " sym) argsdoc)
      (funcall orig-fn sym index))))

(when (< emacs-major-version 28)
  (advice-add 'elisp-get-fnsym-args-string
              :around #'elisp-get-fnsym-args-string@fix-not-documented))


;;; samples

(defvar var-not-documented nil)
(defvar var-documented "This is a variable.")

(defun fn-not-documented () nil)
(defun fn-not-documented-1 (arg1) nil)
(defun fn-not-documented-2 (arg1 arg2) nil)
(defun fn-not-documented-3 (arg1 arg2 arg3) nil)
(defun fn-documented () "This is a Function" nil)
(defun fn-advice (:before () before) nil)
(defun fn-documented-1 (arg1) "This is a Function 1" nil)
(defun fn-documented-2 (arg1 arg2) "This is a Function 2" nil)
(defun fn-documented-3 (arg1 arg2 arg3) "This is a Function 3" nil)


;;; tests

(ert-deftest helm-symbol-hint-test--symbol-summary ()
  (should (equal "Not documented."      (helm-symbol-hint--symbol-summary "var-not-documented")))
  (should (equal "(fn )"                (helm-symbol-hint--symbol-summary "fn-not-documented")))
  (should (equal "(fn ARG1)"            (helm-symbol-hint--symbol-summary "fn-not-documented-1")))
  (should (equal "(fn ARG1 ARG2)"       (helm-symbol-hint--symbol-summary "fn-not-documented-2")))
  (should (equal "(fn ARG1 ARG2 ARG3)"  (helm-symbol-hint--symbol-summary "fn-not-documented-3")))
  (should (equal "This is a Function"   (helm-symbol-hint--symbol-summary "fn-documented")))
  (should (equal "This is a Function 1" (helm-symbol-hint--symbol-summary "fn-documented-1")))
  (should (equal "This is a Function 2" (helm-symbol-hint--symbol-summary "fn-documented-2")))
  (should (equal "This is a Function 3" (helm-symbol-hint--symbol-summary "fn-documented-3")))

  ;; full document
  (should (string-match-p "\
This is a Function" (documentation 'fn-documented t)))

  (advice-add 'fn-documented :before #'fn-advice)

  ;; full document changed by advice
  (should (string-match-p "\
\\(This function has \\)?:before advice: ‘fn-advice’\\.?

This is a Function" (documentation 'fn-documented t)))

  ;; still get correct hint
  (should (equal "This is a Function" (helm-symbol-hint--symbol-summary "fn-documented"))))

(ert-deftest helm-symbol-hint-test--package-summary ()
  (require 'package)
  (package-initialize)
  (let ((install-pkg
         (lambda (pkg doc)
           (with-temp-buffer
             (insert
              (format ";;; %s.el --- %s -*- lexical-binding: t -*-

                       ;; Copyright (C) 2011 Free Software Foundation, Inc.

                       ;; Author: John Doe <johndoe@example.com>
                       ;; Version: 1.0
                       ;; Package-Requires: ((emacs \"25.1\"))
                       ;; Keywords: tools
                       ;; URL: https://example.com/johndoe/%s

                       ;;; Commentary:

                       ;; Example package

                       (provide '%s)
                       ;;; %s.el ends here "
                      pkg doc pkg pkg pkg))
             (indent-region (point-min) (point-max))
             (package-install-from-buffer)))))
    (funcall install-pkg 'nodoc "")
    (funcall install-pkg 'hasdoc "Package summary"))
  (should (equal (helm-symbol-hint--package-summary "nodoc") "Not documented."))
  (should (equal (helm-symbol-hint--package-summary "hasdoc") "Package summary")))

(provide 'helm-symbol-hint-test)

;;; helm-symbol-hint-test.el ends here
