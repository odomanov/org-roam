;;; org-roam-id.el --- ID-related utilities for Org-roam -*- lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (org "9.4") (magit-section "3.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This module provides ID-related facilities using the Org-roam database.
;;
;;; Code:
(require 'org-id)

(defun org-roam-id-at-point ()
  "Return the ID at point, if any.
Recursively traverses up the headline tree to find the
first encapsulating ID."
  (org-with-wide-buffer
   (org-back-to-heading-or-point-min t)
   (while (and (not (org-roam-db-node-p))
               (not (bobp)))
     (org-roam-up-heading-or-point-min))
   (when (org-roam-db-node-p)
     (org-id-get))))

(defun org-roam-id-find (id &optional markerp)
  "Return the location of the entry with the id ID using the Org-roam db.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((node (org-roam-populate (org-roam-node-create :id id))))
    (when-let ((file (org-roam-node-file node)))
      (if markerp
          (unwind-protect
              (let ((buffer (or (find-buffer-visiting file)
                                (find-file-noselect file))))
                (with-current-buffer buffer
                  (move-marker (make-marker) (org-roam-node-point node) buffer))))
        (cons (org-roam-node-file node)
              (org-roam-node-point node))))))

(defun org-roam-id-open (id _)
  "Go to the entry with id ID.
Like `org-id-open', but additionally uses the Org-roam database."
  (org-mark-ring-push)
  (let ((m (or (org-roam-id-find id 'marker)
               (org-id-find id 'marker)))
        cmd)
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    ;; Use a buffer-switching command in analogy to finding files
    (setq cmd
          (or
           (cdr
            (assq
             (cdr (assq 'file org-link-frame-setup))
             '((find-file . switch-to-buffer)
               (find-file-other-window . switch-to-buffer-other-window)
               (find-file-other-frame . switch-to-buffer-other-frame))))
           'switch-to-buffer-other-window))
    (if (not (equal (current-buffer) (marker-buffer m)))
        (funcall cmd (marker-buffer m)))
    (goto-char m)
    (move-marker m nil)
    (org-show-context)))

(org-link-set-parameters "id" :follow #'org-roam-id-open)

;;;###autoload
(defun org-roam-update-org-id-locations (&rest directories)
  "Scan Org-roam files to update `org-id' related state.
This is like `org-id-update-id-locations', but will automatically
use the currently bound `org-directory' and `org-roam-directory'
along with DIRECTORIES (if any), where the lookup for files in
these directories will be always recursive.

Note: Org-roam doesn't have hard dependency on
`org-id-locations-file' to lookup IDs for nodes that are stored
in the database, but it still tries to properly integrates with
`org-id'. This allows the user to cross-reference IDs outside of
the current `org-roam-directory', and also link with \"id:\"
links to headings/files within the current `org-roam-directory'
that are excluded from identification in Org-roam as
`org-roam-node's, e.g. with \"ROAM_EXCLUDE\" property."
  (interactive)
  (cl-loop for dir in (cons org-roam-directory directories)
           for org-roam-directory = dir
           nconc (org-roam-list-files) into files
           finally (org-id-update-id-locations files org-roam-verbose)))

(defun odm/org-link-extra-attrs (orig-result)
  "Advice function to post process ORIG-RESULT of the `org-element-link-parser' function."
  ;;; Retrieving inital values that should be replaced
  (setq raw-path (plist-get (nth 1 orig-result) :raw-link))

  ;; Checking if the link is an `id:' link
  (if (and raw-path
           (string-match-p "^id:.*|\s*:" raw-path))
      (let* (;; Retrieving parameters after the vertical bar
             (results (s-split "|" raw-path))
             (raw-path (car results))
             (path (s-chop-prefix "id:" raw-path))

             ;; Cleaning, splitting and making symbols
             (results (s-split "\s" (s-trim (s-collapse-whitespace
                                             (car (-slice results 1))))))
             (results (-map #'intern results))

             ;; Updating the ouput with the new values
             (orig-result-clean (plist-put (nth 1 orig-result) :raw-link raw-path))
             (orig-result-clean (plist-put orig-result-clean :path path)))

        ;; Check that the length is even
        (if (= 2 (length (-last-item (-partition-all 2 results))))
            (list 'link (-snoc orig-result-clean :extra-attrs results))
          (progn
            (message "Links properties are incorrect. Ignoring them.")
            (list 'link orig-result-clean))))

    ;; Or returning original value of the function
    orig-result))

(advice-add 'org-element-link-parser :filter-return #'odm/org-link-extra-attrs)

(provide 'org-roam-id)

;;; org-roam-id.el ends here
