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

;;; Code:

(defgroup project nil
  "Project root settings."
  :group 'programming)

(defcustom project-guess-root-functions '(project-guess-root-ede
                                          project-guess-root-vc
                                          project-guess-root-ask-user)
  "A list of functions to be called to guess the current project root.

This is called if the project root is needed, but not specified.
The functions are called in the context of the file's buffer for
which a project root is requested, in order until the first
returns a non-nil value, which will be used as the new project
root."
  :type 'hook
  :group 'project)

(defcustom project-root-changed-hook nil
  "Hook run when the current project root is set or changed."
  :type 'hook
  :group 'project)

(defcustom project-vc-root-marker '(".git" ".hg" ".bzr")
  "Files or directories present in the root of a repository.

These are used as indicators of the directory being a suitable
project root."
  :type '(repeat string)
  :group 'project)

(defcustom project-vc-directory-marker '(".svn" "CVS")
  "Files or directories present in repository directories.

These are used to find the project root. If a directory contains
such a file or directory, but the parent directory does not, this
is assumed to be a good candidate for a project root."
  :type '(repeat string)
  :group 'project)


;;; Internal variables

(defvar project-root nil
  "The current project root.

Don't access this variable directly. Use the `project-root' and
`project-set-root' functions, which see.")
(make-variable-buffer-local 'project-root)
(put 'project-root 'safe-local-variable 'file-directory-p)


;;; External API

;;;###autoload
(defun project-root ()
  "Return the current project root directory.

If there is no project root configured, try to find a sensible
choice by calling `project-guess-root'."
  (when (not project-root)
    (project-set-root
     (run-hook-with-args-until-success 'project-guess-root-functions)))
  project-root)

;;;###autoload
(defun project-set-root (new-root)
  "Set the current project root directory to NEW-ROOT.

This will also adjust the directory-local variable to point to
the new root."
  (interactive "DProject root: ")
  (setq project-root new-root)
  (run-hooks 'project-root-changed-hook))


;;; Possible values for `project-guess-root-functions'.

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

(defun project-guess-root-ede ()
  "Return the project root of the EDE project, if any."
  (when (and (boundp 'ede-object-root-project)
             ede-object-root-project)
    (ede-project-root-directory ede-object-root-project)))

(defun project-guess-root-ask-user ()
  "Ask the user for the project root."
  (read-directory-name "Project root: "))


(provide 'project)
;;; project.el ends here
