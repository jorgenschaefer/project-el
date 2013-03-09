;;; project.el --- Define and manage a project root.

;; Copyright (C) 2013  Jorgen Schaefer <forcer@forcix.cx>

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a very simple extension to define a project root for the
;; current file. This extension does nothing with the project root,
;; but it can be used by other extensions to work on the "current
;; project" without each and every one of them defining their own
;; project root.

;; To use in an extension, simply use (project-root) to get the
;; current project root. This will either return a configured project
;; root, use a number of heuristics to find it, or ask the user.

;; To extend the heuristics, add new functions to
;; `project-guess-root-functions', or customize
;; `project-vc-root-marker' and `project-vc-directory-marker'.

;; Finally, this library allows project-specific variables to be set
;; and read. To enable this functionality, set `project-root-file' to
;; a string. This will cause a file of that name to be created in the
;; project root. You can use `project-set' and `project-get' to set
;; and get variables from that file.

;;; Code:

(defgroup project nil
  "Project root settings."
  :group 'programming)

(defcustom project-root-file nil
  "The file name to use to indicate the project root.

If this is non-nil, when a project root is guessed or explicitly
set by the user, this file is created in the project root to
reuse the same project root in the future.")

(defcustom project-guess-root-functions '(project-guess-indicator
                                          project-guess-root-vc)
  "A list of functions to be called to guess the current project root.

This is called if the project root is needed, but not specified.
The functions are called in the context of the file's buffer for
which a project root is requested, in order until the first
returns a non-nil value, which will be used as the new project
root.")

(defcustom project-vc-root-marker '(".git" ".hg" ".bzr")
  "Files or directories present in the root of a repository.

These are used as indicators of the directory being a suitable
project root.")

(defvar project-vc-directory-marker '(".svn" "CVS")
  "Files or directories present in repository directories.

These are used to find the project root. If a directory contains
such a file or directory, but the parent directory does not, this
is assumed to be a good candidate for a project root.")

(defvar project-root nil
  "The current project root.")
(make-variable-buffer-local 'project-root)
(put 'project-root 'safe-local-variable 'file-directory-p)

;;;###autoload
(defun project-root ()
  "Return the current project root directory.

If there is no project root configured, try to find a sensible
choice by calling `project-guess-root'."
  (when (not project-root)
    (project-set-root (project-guess-root)))
  project-root)

;;;###autoload
(defun project-set-root (new-root)
  "Set the current project root directory to NEW-ROOT.

This will also adjust the directory-local variable to point to
the new root."
  (interactive "DProject root: ")
  (setq project-root new-root)
  (project-set 'project-root project-root))

(defun project-guess-root ()
  "Use `project-guess-root-functions' to guess the current project root."
  (or (run-hook-with-args-until-success 'project-guess-root-functions)
      (read-directory-name "Project root: ")))

(defun project-guess-indicator ()
  "Guess the project root based on `project-root-file'."
  (when project-root-file
    (locate-dominating-file (buffer-file-name) project-root-file)))

(defun project-guess-root-vc ()
  "Guess the project root based on version control files.

See the variables `project-vc-root-marker' and
`project-vc-directory-marker'."
  (locate-dominating-file
   (buffer-file-name)
   (lambda (dir)
     (catch 'return
       (dolist (marker project-vc-root-marker)
         (when (file-exists-p (concat dir "/" marker))
           (throw 'return t)))
       (dolist (marker project-vc-directory-marker)
         (when (and (file-exists-p (concat dir "/" marker))
                    (not (file-exists-p (concat dir "/../" marker))))
           (throw 'return t)))
       nil))))

(defun project-set (varname value)
  "Set the variable VARNAME to VALUE.

This uses the `project-root-file' file for settings."
  (when project-root-file
    (let ((project-file (concat project-root "/"
                                project-root-file)))
      (with-temp-buffer
        (when (file-exists-p project-file)
          (insert-file-contents-literally project-file))
        (goto-char (point-min))
        (let* ((data (ignore-errors
                       (read buf)))
               (cell (assq varname data)))
          (if cell
              (setcdr cell value)
            (setq data (cons (cons varname value)
                             data)))
          (erase-buffer)
          (insert ";; Project data for Emacs' project.el\n\n")
          (pp data (current-buffer))
          (write-region (point-min) (point-max) project-file
                        nil 'dont-display-message))))))

(defun project-get (varname &optional default)
  "Retrieve the value of VARNAME. If none is set, return DEFAULT.

This uses the `project-root-file' file for settings."
  (when project-root-file
    (let ((project-file (concat project-root "/"
                                project-root-file)))
      (with-temp-buffer
        (when (file-exists-p project-file)
          (insert-file-contents-literally project-file))
        (goto-char (point-min))
        (let* ((data (ignore-errors
                       (read buf)))
               (cell (assq varname data)))
          (if cell
              (cdr cell)
            default))))))

(provide 'project)
;;; project.el ends here
