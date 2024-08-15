;;; org-roam-prop-search.el --- Org-roam property search interface -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.2.2
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-roam "2.1"))

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
;; This package provides the necessary changes required to make org-export work out-of-the-box.
;;
;; To enable it, run:
;;
;;    (require 'org-roam-prop-search)
;;
;;
;;; Code:

(use-package helm)
(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'ol)
(require 'dash)

(defcustom org-roam-props-to-filter
  '("CATEGORY" "ITEM" "FILE" "ALLTAGS" "ID" "PRIORITY" "TITLE"
    "REL" "TAGS" "TODO")
  "Properties to filter."
  :group 'org-roam)

(defface org-roam-props--property-face
    '((t (:foreground "blue"
          :weight bold
          )))
  "Face for 'properties' in search."
  :group 'org-roam-faces)

(defun org-roam-props--completions ()
  "Make list [prop,value,id] from list [properties,id]."
  (let* ((props+ids (org-roam-db-query [:select [properties,id] :from nodes]))
         ;; (to-filter org-roam-props-to-filter)
         (raw-lst (seq-mapcat (lambda (props+id)
                                (seq-mapcat (lambda (prop)
                                              (mapcar (lambda (x)
                                                        (list (car prop) x (cadr props+id)))
                                                      (split-string-and-unquote (cdr prop))))
                                            (car props+id)))
                              props+ids)))
    (seq-remove (lambda (p)
                  (member (car p) org-roam-props-to-filter))
                raw-lst)))

(defun odm/sort-uniq (n lst)
  "Make LST unique and sorted by the Nth element.
Comparison functions are `string<' and `'string='."
  (-sort (lambda (x y) (string< (elt x n) (elt y n)))
         (let ((-compare-fn (lambda (x y)
                              (string= (elt x n) (elt y n)))))
           (-uniq lst))))

(defun org-roam-props--mk-prop-candidates (filtered)
  ""
  (-map (lambda (x)
          (let ((str (concat (propertize (format ":%s: " (elt x 0))
                                         'face 'org-roam-props--property-face)
                             (elt x 1))))
            (cons str x)))
        filtered))

(defun org-roam-props--mk-val-candidates (v filtered)
  ""
  (let ((flt (-filter (lambda (x) (string= (cadr v) (cadr x)))
                      filtered)))
    (-map (lambda (x)
            (let ((str (concat (propertize
                                (format ":%s: " (string-join `(,(elt x 0) ,(elt x 1)) ":"))
                                'face 'org-roam-props--property-face)
                               (org-roam-node-title (org-roam-node-from-id (elt x 2))))))
              (cons str x)))
          flt)))

(defun org-roam-search-property ()
  "Search properties interactively."
  (interactive)
  (let* ((props (org-roam-props--completions))
         (prop (completing-read "Property: " (odm/sort-uniq 0 props)))
         (filtered (-filter (lambda (x) (equal (car x) prop)) props))
         )
    (helm :sources (helm-build-sync-source "Properties"
                     :candidates (odm/sort-uniq 2 (org-roam-props--mk-prop-candidates filtered))
                     :action (lambda (v)
                               (helm :sources
                                     (helm-build-sync-source "Values"
                                       :candidates
                                       (odm/sort-uniq 3 (org-roam-props--mk-val-candidates v filtered))
                                       :action (lambda (x)
                                                 (org-roam-node-open (org-roam-node-from-id (elt x 2)))))))))))


(provide 'org-roam-prop-search)
;;; org-roam-prop-search.el ends here
