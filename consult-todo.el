;;; consult-todo.el --- Search hl-todo keywords in consult -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Eki Zhang

;; Author: Eki Zhang <liuyinz95@gmail.com>
;; Maintainer: Eki Zhang <liuyinz95@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1") (consult "1.9") (hl-todo "3.8.2"))
;; Homepage: https://github.com/eki3z/consult-todo
;; License: GPL-3.0-or-later

;; This file is not a part of GNU Emacs.

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

;; Provide commands `consult-todo' to search, filter, jump to hl-todo keywords.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase)
  (require 'subr-x))

(require 'seq)
(require 'compile)
(require 'grep)
(require 'consult)
(require 'hl-todo)

(declare-function project-root "project")

(defgroup consult-todo nil
  "Search hl-todo keywords in consult."
  :group 'consult-todo)

(defcustom consult-todo-narrow nil
  "Alist of (NARROW . KEYWORD) to display."
  :type '(repeat (cons (character :tag "Narrow")
                       (string :tag "Keyword")))
  :group 'consult-todo)

(defcustom consult-todo-other (cons ?. "OTHER")
  "Cons mapping for narrow and missing keywords."
  :type '(cons character string)
  :group 'consult-todo)

(defcustom consult-todo-only-comment nil
  "If non-nil, only search todo keywords in comments.
Only effective on buffers."
  :type 'boolean
  :group 'consult-todo)

(defcustom consult-todo-dir-preview-key nil
  "Preview trigger keys for `consult-todo-dir' related command.
Value can be nil, `any', a single key or a list of keys."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key))
  :group 'consult-todo)

(defcustom consult-todo-cache-threshold 3
  "The time threshold in seconds for using cache when grepping is time-consuming."
  :type 'number
  :group 'consult-todo)

(defcustom consult-todo-dir-function #'consult-todo--rgrep
  "The function used to grep keywords in directory.
Accept one argument: the directory to search in."
  :type 'function
  :group 'consult-todo)

(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")

(defvar consult-todo--narrow-extend nil
  "Default mapping of narrow and keywords include OTHER if exists.")

(defun consult-todo--narrow ()
  "Return narrow alist."
  (or consult-todo-narrow consult-todo--narrow))

(defun consult-todo--narrow-extend ()
  "Return narrow alist include `consult-todo-other' if it's non-nil."
  (or consult-todo--narrow-extend
      (if-let* (((consp consult-todo-other))
                (narrow (car consult-todo-other))
                (group (cdr consult-todo-other))
                ((and (characterp narrow)
                      (not (assoc narrow (consult-todo--narrow)))))
                ((and (stringp group) (not (rassoc group (consult-todo--narrow))))))
          (setq consult-todo--narrow-extend
                (cons consult-todo-other (consult-todo--narrow)))
        (user-error
         "Consult-todo-other: format error or conflicts with consult-todo-narrow")
        (setq consult-todo--narrow-extend nil))))

(defun consult-todo--format (candidates)
  "Return formatted string according to CANDIDATES."
  (if candidates
      (mapcar
       (pcase-lambda (`(,name ,line ,type ,pos ,narrow ,text))
         (propertize
          (format (apply #'format "%%-%ds %%-%ds %%-%ds %%s"
                         (cl-loop for i to 2
                                  collect (seq-max (mapcar
                                                    (lambda(x) (length (nth i x)))
                                                    candidates))))
                  (propertize name 'face 'consult-file)
                  (propertize line 'face 'consult-line-number)
                  ;; WONTFIX don't support regexp keywords face
                  (propertize type 'face (hl-todo--combine-face
                                          (cdr (assoc type hl-todo-keyword-faces))))
                  text)
          'consult-location (cons pos line)
          'consult--type narrow))
       candidates)
    (user-error "No hl-todo keywords")))

(defun consult-todo-grep-state ()
  "Lookup SELECTED in CANDIDATES list of `consult-location' category.
Return the location marker."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand (funcall open))
      (when cand
        (setq cand (car (get-text-property 0 'consult-location cand)))
        (funcall jump action (consult--marker-from-line-column
                              (ignore-errors
                                (funcall (or (and (not (eq action 'return)) open)
                                             #'find-file-noselect)
                                         (nth 0 cand)))
                              (nth 1 cand) (nth 2 cand)))))))


(defun consult-todo--parse-bufs (buffers)
  "Return list of hl-todo keywords in BUFFERS."
  (cl-loop for buf in (ensure-list (or buffers (current-buffer)))
           append
           (with-current-buffer buf
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (cl-loop while (hl-todo--search)
                          when (or (null consult-todo-only-comment)
                                   (nth 4 (syntax-ppss)))
                          collect
                          ;; HACK when buffer is too large, match-string return nil
                          ;; in one match, use thing-at-point to get the match-string
                          ;; as type. At this failed match, point is still accurate,
                          ;; match-begin and match-end is limit to accessible portion
                          ;; of buffer
                          (let ((type (or (match-string-no-properties 2)
                                          (save-excursion
                                            (backward-to-word)
                                            (substring-no-properties
                                             (save-match-data
                                               (thing-at-point 'word)))))))
                            (list (buffer-name)
                                  (number-to-string (line-number-at-pos))
                                  type
                                  (copy-marker (point))
                                  (car (or (rassoc type (consult-todo--narrow))
                                           consult-todo-other))
                                  (string-trim (buffer-substring-no-properties
                                                (point)
                                                (line-end-position)))))))))))

(defun consult-todo--rgrep (dir)
  "Function to use rgrep to search keywords in DIR."
  (let* ((todo-buf (format "*consult-todo-dir %s*" dir))
         (grep-command "grep --color=auto -nH --null -I -e ")
         (compilation-auto-jump-to-first-error nil)
         cache-p)
    (cl-letf ((compilation-buffer-name-function
               (lambda (&rest _) (format "%s" todo-buf))))
      (rgrep (hl-todo--regexp) "* .*" dir)
      (let ((proc (get-buffer-process todo-buf)))
        (run-with-timer
         consult-todo-cache-threshold nil
         (lambda ()
           (when (and proc (process-live-p proc)
                      (eq (process-status proc) 'run))
             (message "consult-todo: dir %s is caching!" dir)
             (setq cache-p t))))
        (set-process-sentinel
         proc
         (lambda (_ event)
           (unwind-protect
               (when (string-equal "finished\n" event)
                 (let ((result
                        (consult-todo--format
                         (with-current-buffer todo-buf
                           (goto-char (point-min))
                           (cl-loop while (and (null (eobp))
                                               (condition-case nil
                                                   (progn
                                                     (compilation-next-error 1)
                                                     t)
                                                 (user-error nil)))
                                    when (save-excursion
                                           (save-match-data
                                             (text-property-search-forward 'font-lock-face 'match t)))
                                    collect
                                    (let* ((msg (get-text-property (point) 'compilation-message))
                                           (loc (compilation--message->loc msg))
                                           (line (compilation--loc->line loc))
                                           (col (compilation--loc->col loc))
                                           (file (caar (compilation--loc->file-struct loc)))
                                           (type (buffer-substring-no-properties
                                                  (prop-match-beginning it)
                                                  (prop-match-end it))))
                                      (list (file-name-nondirectory file)
                                            (number-to-string line)
                                            type
                                            (list (expand-file-name file compilation-directory)
                                                  line col)
                                            (car (or (rassoc type (consult-todo--narrow))
                                                     consult-todo-other))
                                            (string-trim
                                             (buffer-substring-no-properties
                                              (prop-match-end it)
                                              (line-end-position))))))))))
                   (if (null cache-p)
                       (condition-case nil
                           (consult-todo--dir result)
                         (quit (message "Quit")))
                     (setf (alist-get dir consult-todo--cache
                                      nil nil #'equal)
                           result)
                     (message "consult-todo: dir %s caching complete!" dir))))
             (kill-buffer todo-buf))))))))

;;;###autoload
(defun consult-todo (&optional buffers)
  "Jump to hl-todo keywords in BUFFERS.
If BUFFERS is nil, use current buffer instead."
  (interactive "P")
  (consult--forbid-minibuffer)
  (consult--read
   (consult-todo--format
    (consult-todo--parse-bufs buffers))
   :prompt "Go to hl-todo: "
   :category 'consult-location
   :require-match t
   :sort nil
   :group (consult--type-group (consult-todo--narrow-extend))
   :narrow (consult--type-narrow (consult-todo--narrow-extend))
   :lookup #'consult--lookup-location
   :state (consult--jump-state)))

;;;###autoload
(defun consult-todo-all ()
  "Jump to hl-todo keywords in all `hl-todo-mode' enabled buffers."
  (interactive)
  (consult-todo (seq-filter (lambda (x)
                              (buffer-local-value 'hl-todo-mode x))
                            (buffer-list))))

(defvar consult-todo--cache nil)

(defun consult-todo--dir (formatted-candidates)
  "Jump to hl-todo keywords with FORMATTED-CANDIDATES."
  (consult--forbid-minibuffer)
  (consult--read
   formatted-candidates
   :prompt "Go to hl-todo in dir: "
   :category 'consult-grep
   :require-match t
   :sort nil
   :preview-key consult-todo-dir-preview-key
   :group (consult--type-group (consult-todo--narrow-extend))
   :narrow (consult--type-narrow (consult-todo--narrow-extend))
   :lookup #'consult--lookup-member
   :state (consult-todo-grep-state)))

;;;###autoload
(defun consult-todo-clear-cache (&optional all)
  "Clear cache stored in `consult-todo--cache'.
If arg ALL is non-nil, clear all cache."
  (interactive "P")
  (if-let* ((dirs (mapcar #'car consult-todo--cache)))
      (if all
          (setq consult-todo--cache nil)
        (when-let* ((dir (completing-read "consult-todo: clear cache in "
                                          dirs nil t)))
          (setf (alist-get dir consult-todo--cache nil 'remove #'equal) nil)))
    (message "consult-todo: no cache yet.")))

;;;###autoload
(defun consult-todo-dir (&optional directory)
  "Jump to hl-todo keywords in files located in DIRECTORY.
If optional arg DIRECTORY is nil, rgrep in default directory. With
\\[universal-argument] enable, select DIRECTORY instead."
  (interactive)
  (let* ((dir (or directory
                  (and current-prefix-arg
                       (read-directory-name "select directory: "))
                  default-directory)))
    (if-let* ((result (alist-get dir consult-todo--cache
                                 nil nil #'equal)))
        (consult-todo--dir result)
      (save-window-excursion
        (funcall consult-todo-dir-function dir)))))

;;;###autoload
(defun consult-todo-project ()
  "Jump to hl-todo keywords in current project."
  (interactive)
  (consult-todo-dir
   (when-let* ((project (project-current)))
     (expand-file-name
      (if (fboundp 'project-root)
          (project-root project)
        (car (with-no-warnings
               (project-roots project))))))))

(provide 'consult-todo)
;;; consult-todo.el ends here
