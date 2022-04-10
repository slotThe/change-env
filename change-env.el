;;; change-env.el --- Change in and out of LaTeX environments -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Tony Zorman
;;
;; Author: Tony Zorman <soliditsallgood@mailbox.org>
;; Keywords: convenience, tex
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;; Refer to the README for a full account of the package's
;; functionality, as well as how to install it.  Briefly:
;;
;; + The entry point is the `change-env' function, which—when invoked
;;   from inside an environments—pops up a list of possible actions, as
;;   defined by the `change-env-options' variable.
;;
;; + Labels are changed/deleted in a previous way, with an option to
;;   edit the respective label across the whole project; see below.
;;   Also, deleted labels are stored for the current session (based on
;;   the specific contents of the environment) and potentially restored
;;   when switching from e.g. display math to an environment with an
;;   associated label prefix in `change-env-labels'.
;;
;; + What exactly we mean by "display math" is controlled by the
;;   `change-env-display' variable.
;;
;; + This package depends on AUCTeX—but you are already using that
;;   anyways.
;;
;; + If you're customizing `change-env-edit-project-labels', we also
;;   depend on project.el, meaning Emacs 27.1 and up.

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

(defcustom change-env-edit-project-labels nil
  "Whether to change labels after an edit."
  :group 'change-env
  :type 'boolean
  :initialize #'(lambda (symbol exp)
                  (when exp
                    (require 'project))
                  (custom-initialize-default symbol exp)))

(defvar change-env--deleted-labels (make-hash-table)
  "Environments that used to have labels.
Associated to each is the respective content of the latter.")

(defun change-env--get-labels (env)
  "Return the label prefix for ENV."
  (alist-get env change-env-labels nil nil 'string=))

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

(defun change-env-find-matching-begin ()
  "Find the beginning of the current environment.
Like `LaTeX-find-matching-begin', but take care of corner cases
like being at the very beginning/end of the current environment."
  (change-env--find-match 'LaTeX-find-matching-begin))

(defun change-env-find-matching-end ()
  "Find the end of the current environment.
Like `LaTeX-find-matching-end', but take care of corner cases
like being at the very beginning/end of the current environment."
  (change-env--find-match 'LaTeX-find-matching-end))

(defun change-env--find-match (find-match)
  "Find match according to FIND-MATCH.
See `change-env-find-matching-begin' and
`change-env-find-matching-end' for documentation."
  (let ((boi (save-excursion (LaTeX-back-to-indentation) (point))))
    (cond ((equal (point) boi) (forward-char))
          ((point-at-eol)      (backward-char)))
    (funcall find-match)))

(defun change-env--env->hash ()
  "Get the hash of the contents in the current environment.
Before hashing, strip all non essential characters (i.e.,
whitespace) from the string."
  (cl-flet* ((get-env (goto-beg goto-end)
               (funcall goto-beg)
               (push-mark)
               (funcall goto-end)
               (replace-regexp-in-string
                "[ \t\n\r]+"
                ""
                (buffer-substring-no-properties (mark) (point)))))
    (save-excursion
      (pcase-let ((`(,env . ,beg) (change-env--closest-env))
                  (`(,open . ,close) change-env-display))
        (sxhash
         (if (equal env 'math)
             ;; Display math
             (get-env #'(lambda ()
                          (goto-char beg)
                          (forward-char (length open)))
                      #'(lambda ()
                          (search-forward close)
                          (backward-char (length close))))
           ;; Proper environment
           (get-env #'(lambda ()
                        (change-env-find-matching-begin)
                        (forward-line))
                    #'(lambda ()
                        (change-env-find-matching-end)
                        (forward-line -1)
                        (end-of-line)))))))))

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
    (push-mark)
    (pcase-let ((`(,env . ,beg) (change-env--closest-env))
                (`(,open . ,close) change-env-display))
      (if (equal env 'math)             ; display math
          (delete-env beg
                      #'(lambda () (search-forward close))
                      #'(lambda () (+ (point) (length open)))
                      #'(lambda () (- (point) (length close))))
        (delete-env beg
                    #'change-env-find-matching-end
                    #'(lambda () (save-excursion (end-of-line) (point)))
                    #'(lambda () (save-excursion (back-to-indentation) (point))))))
    (setq mark-active t)
    (indent-region (mark) (point))))

(defun change-env--to-display-math ()
  "Transform an environment to display math."
  (save-mark-and-excursion
    (change-env--change-label (car (change-env--closest-env)))
    (change-env--change (car change-env-display)
                        (cdr change-env-display))))

(defun change-env--change-label (old-env &optional new-env)
  "Change the label for OLD-ENV to the one for NEW-ENV.
If NEW-ENV is not given, delete (and save) the label instead."
  (change-env-find-matching-begin)
  (let* ((orig-point (point))
         (old-lbl (change-env--get-labels old-env))
         (new-lbl (change-env--get-labels new-env)))
    (cl-flet* ((goto-label? ()
                 (search-forward "\\label{"
                                 (save-excursion (forward-line) (point-at-eol))
                                 t)
                 (not (eql orig-point (point))))
               (get-label-text ()
                 (save-excursion
                   (forward-char (length old-lbl))
                   (push-mark)
                   (search-forward "}")
                   (backward-char)
                   (buffer-substring-no-properties (mark) (point))))
               (replace-label (old new)
                 (when change-env-edit-project-labels
                   (project-query-replace-regexp old new))))
      (if (goto-label?)
          (let ((label (get-label-text)))
            (if (and old-lbl new-lbl)   ; replace old label with new one
                (progn
                  (delete-char (length old-lbl))
                  (insert new-lbl)
                  (replace-label (concat old-lbl label) (concat new-lbl label)))
              ;; Only the old label exists: delete and save it.
              (puthash (change-env--env->hash) ; key
                       (get-label-text)        ; val
                       change-env--deleted-labels)
              (delete-region (- (point) (length "\\label{")) (point-at-eol))
              (replace-label (concat "\\\\ref{" old-lbl label "}") "")))
        ;; No label found -> check if we can restore something.
        (let ((label (gethash (change-env--env->hash) change-env--deleted-labels)))
          (when (and new-lbl label)
            (change-env-find-matching-begin)
            (end-of-line)
            (insert (concat " \\label{" new-lbl label "}"))))))))

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
        (if (equal old-env 'math)       ; in a display math environment
            (change-env--change (concat "\\begin{" new-env "}")
                                (concat "\\end{"   new-env "}"))
          (LaTeX-modify-environment new-env))
        (change-env--change-label old-env new-env)))))

(defun change-env--closest-env ()
  "Find the starting position of the closest environment."
  (pcase-let* ((`(,math-sym . ,math-beg) (and (texmathp) texmathp-why))
               (in-display (equal math-sym (car change-env-display)))
               (env-beg (ignore-errors (save-excursion (change-env-find-matching-begin)
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
