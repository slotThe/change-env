;;; change-env.el --- Change in and out of LaTeX environments -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Solid
;;
;; Author: Solid <soliditsallgood@mailbox.org>
;; Keywords: convenience, tex
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://gitlab.com/slotThe/change-env
;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a way to modify LaTeX environments, as well as
;; the display math mode (seeing it as an environment of sorts).  Thus,
;; henceforth the world "environment" shall—in addition to
;; \begin--\end-style environments—also refer to display math.
;;
;; To use this package, simply put `change-env.el' somewhere inside of
;; your `load-path' and `require' it:
;;
;;     (with-eval-after-load 'latex
;;       (require 'change-env)
;;       (bind-key (kbd "C-c r") #'change-env LaTeX-mode-map))
;;
;; or, using `use-package':
;;
;;     (use-package change-env
;;       :after latex
;;       :bind (:map LaTeX-mode-map ("C-c r" . change-env)))
;;
;; The entry point is the `change-env' function, which—when invoked from
;; inside an environments—pops up a list of possible actions, as defined
;; by the `change-env-options' variable.  By default, the possible
;; options are
;;
;;   - Delete the current environment.
;;
;;   - Modify the current environment; the completion list is provided
;;     by `LaTeX-enviroment-list'.
;;
;;   - Convert the current environment to display math.
;;
;; Note that, while the second option could be implemented to
;; incorporate the third option as well, it is consciously not.  The
;; goal is to make switching to display math as prominent as possible.
;; This fits quite well with the authors particular sensibilities.
;;
;; What exactly we mean by "display math" is controlled by the
;; `change-env-display' variable.  By default, it is set to `\[...\]'
;; style display math.  If the excellent `math-delimiters' package [1]
;; is also used, customizing it along the lines of
;;
;;     (setq change-env-display math-delimiters-display)
;;
;; keeps consistency with that package.
;;
;; This package is not compatible with plain tex, as it depends on
;; AUCTeX—but you are already using that anyways.
;;
;; [1]: https://github.com/oantolin/math-delimiters

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'latex)

(defgroup change-env nil
  "Change in and out of LaTeX environments."
  :group 'tex)

(defcustom change-env-options
  '((?k "Delete"       change-env--change         )
    (?d "Display Math" change-env--to-display-math)
    (?m "Modify"       change-env--modify         ))
  "Options for `change-env'.
Takes a list of three items; namely,

  - a key to trigger the action,
  - a label to display to the user in the minibuffer,
  - the action to execute as a function taking no (mandatory)
    arguments."
  :group 'change-env
  :type '(repeat (list
                  (character :tag "Key")
                  (string    :tag "Label")
                  (symbol    :tag "Command"))))

(defcustom change-env-display '("\\[" . "\\]")
  "Set the preferred display math style."
  :group 'change-env
  :type '(choice (const :tag "Brackets" ("\\[" . "\\]"))
                 (const :tag "Dollars"  ("$$"  . "$$"))))

(defcustom change-env-labels '(("remark"     . "rem:")
                               ("definition" . "def:")
                               ("lemma"      . "lem:")
                               ("theorem"    . "thm:")
                               ("equation"   . "eq:" )
                               ("corollary"  . "cor:"))
  "Label prefixes with their associated environments."
  :group 'change-env
  :type '(repeat (cons
                  (string :tag "Environment")
                  (string :tag "Label prefix"))))

;;;###autoload
(defun change-env ()
  "Change a LaTeX environment.
When inside an environment or display math, execute an action as
specified by `change-env-options'."
  (interactive)
  (let ((key (read-key (change-env--prompt))))
    (funcall (cadr (alist-get key change-env-options)))))

(defun change-env--prompt ()
  "How to show options to the user."
  (mapconcat (pcase-lambda (`(,key ,label _))
               (format "[%s] %s"
                       (propertize (single-key-description key) 'face 'bold)
                       label))
             change-env-options
             " "))

(defun change-env--change (&optional beg end)
  "Change an environment.
Delete the old one, and possibly insert new beginning and end
delimiters, as indicated by the optional arguments BEG and END."
  (cl-flet* ((delete-env (env-begin find-end open-end close-beg)
               ;; delete beginning, possibly insert a new one
               (goto-char env-begin)
               (if (not beg)
                   (change-env--delete-line)
                 (delete-region (point) (funcall open-end))
                 (insert beg))
               ;; delete end, possibly insert a new one
               (funcall find-end)
               (if (not end)
                   (change-env--delete-line)
                 (delete-region (funcall close-beg) (point))
                 (insert end))))
    (save-mark-and-excursion
      (push-mark)
      (pcase-let ((`(,env . ,beg) (change-env--closest-env))
                  (`(,open . ,close) change-env-display))
        (if (equal env 'math)           ; display math
            (delete-env beg
                        #'(lambda () (search-forward close))
                        #'(lambda () (+ (point) (length open)))
                        #'(lambda () (- (point) (length close))))
          (delete-env beg
                      #'LaTeX-find-matching-end
                      #'(lambda () (save-excursion (end-of-line) (point)))
                      #'(lambda () (save-excursion (back-to-indentation) (point))))))
      (setq mark-active t)
      (indent-region (mark) (point)))))

(defun change-env--to-display-math ()
  "Transform an environment to display math."
  (change-env--change (car change-env-display)
                      (cdr change-env-display)))

(defun change-env--change-label (old-env new-env)
  "Change the label (prefix) of an environment."
  (LaTeX-find-matching-begin)
  (let ((orig-point (point))
        (old-lbl (alist-get old-env change-env-labels nil nil 'string=))
        (new-lbl (alist-get new-env change-env-labels nil nil 'string=)))
    (cond
     ((s-ends-with? "*" new-env)
      (search-forward "}")
      (delete-region (point) (point-at-eol)))
     ((and old-lbl new-lbl
           (not (equal orig-point
                       (progn (search-forward "\\label" (point-at-eol) t)
                              (point)))))
      (forward-char)
      (delete-char (length old-lbl))
      (insert new-lbl)))))

(defun change-env--modify ()
  "Modify a LaTeX environment.
Most of the implementation stolen from `LaTeX-environment', just
also act on display math environments."
  (let* ((default (cond ((TeX-near-bobp) "document")
                        ((and LaTeX-default-document-environment
                              (string-equal (LaTeX-current-environment) "document"))
                         LaTeX-default-document-environment)
                        (t LaTeX-default-environment)))
         (new-env (completing-read (concat "Environment type (default " default "): ")
                                   (LaTeX-environment-list-filtered) nil nil
                                   nil 'LaTeX-environment-history default)))
    (unless (equal new-env default)
      (setq LaTeX-default-environment new-env))
    (let ((entry (assoc new-env (LaTeX-environment-list)))
          (old-env (car (change-env--closest-env))))
      (when (null entry)
        (LaTeX-add-environments (list new-env)))
      (save-mark-and-excursion
        (if (equal old-env 'math)         ; in a display math environment
            (change-env--change (concat "\\begin{" new-env "}")
                                (concat "\\end{"   new-env "}"))
          (LaTeX-modify-environment new-env)
          (change-env--change-label old-env new-env))))))

(defun change-env--closest-env ()
  "Find the starting position of the closest environment."
  (pcase-let* ((`(,math-sym . ,math-beg) (and (texmathp) texmathp-why))
               (in-display (equal math-sym (car change-env-display)))
               (env-beg (ignore-errors (save-excursion (LaTeX-find-matching-begin)
                                                       (point)))))
    (cond
     ;; Possibly fancy math environment.
     ((and math-beg env-beg)
      (if (>= env-beg math-beg)         ; prefer inner
          (cons math-sym env-beg)
        (unless in-display
          (error "Not in a display math environment"))
        `(math . ,math-beg)))
     ;; Other non-math environment.
     (env-beg (goto-char env-beg)
              (search-forward "{" (point-at-eol))
              (cons (current-word) env-beg))
     ;; Display math.
     (math-beg (unless in-display
                 (error "Not in a display math environment"))
               `(math . ,math-beg))
     (t (error "Not in any environment")))))

(defun change-env--delete-line ()
  "Delete the current line."
  (delete-region (progn (beginning-of-line) (point))
                 (progn (forward-line 1)    (point))))

(provide 'change-env)
;;; change-env.el ends here
