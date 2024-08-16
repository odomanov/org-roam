;;; org-roam-search.el --- Org-roam property search interface -*- coding: utf-8; lexical-binding: t; -*-

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
;;    (require 'org-roam-search)
;;
;;
;;; Code:

(use-package helm)
(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'ol)
(require 'dash)

(defcustom org-roam-search-to-filter
  '("CATEGORY" "ITEM" "FILE" "ALLTAGS" "ID" "PRIORITY" "TITLE"
    "REL" "TAGS" "TODO")
  "Properties to filter."
  :group 'org-roam)

(defface org-roam-search--property-face
    '((t (:foreground "blue"
          :weight bold
          )))
  "Face for 'properties' in search."
  :group 'org-roam-faces)

(defun org-roam-search--completions ()
  "Make list [prop,value,id] from list [properties,id]."
  (let* ((props+ids (org-roam-db-query [:select [properties,id] :from nodes]))
         ;; (to-filter org-roam-search-to-filter)
         (raw-lst (seq-mapcat (lambda (props+id)
                                (seq-mapcat (lambda (prop)
                                              (mapcar (lambda (x)
                                                        (list (car prop) x (cadr props+id)))
                                                      (split-string-and-unquote (cdr prop))))
                                            (car props+id)))
                              props+ids)))
    (seq-remove (lambda (p)
                  (member (car p) org-roam-search-to-filter))
                raw-lst)))

(defun odm/sort-uniq (n lst)
  "Make LST unique and sorted by the Nth element.
Comparison functions are `string<' and `'string='."
  (-sort (lambda (x y) (string< (elt x n) (elt y n)))
         (let ((-compare-fn (lambda (x y)
                              (string= (elt x n) (elt y n)))))
           (-uniq lst))))

(defun org-roam-search--mk-prop-candidates (filtered)
  ""
  (--map (let ((str (concat (propertize (format ":%s: " (elt it 0))
                                        'face 'org-roam-search--property-face)
                            (elt it 1))))
           (cons str it))
         filtered))

(defun org-roam-search--mk-val-candidates (filtered)
  ""
  (--map (let ((str (concat (propertize
                             (format ":%s: " (string-join (list (elt it 0) (elt it 1)) ":"))
                             'face 'org-roam-search--property-face)
                            (org-roam-node-title (org-roam-node-from-id (elt it 2))))))
           (cons str it))
         filtered))

(defun org-roam-search--mk-tag-candidates (filtered)
  ""
  (--map (let* ((title (org-roam-node-title (org-roam-node-from-id (elt it 1))))
                (str (concat (propertize
                             (format ":%s: " (elt it 0))
                             'face 'org-roam-search--property-face)
                            title)))
           (cons str (cons title it)))
         filtered))

(defun org-roam-search-property ()
  "Search properties interactively."
  (interactive)
  (let* ((props (org-roam-search--completions)))
    (helm
     :sources (helm-build-sync-source "Properties"
                :candidates (odm/sort-uniq 0 (--map (cons (car it) it) props))
                :action
                (lambda (_)
                  (let* ((pcands (helm-marked-candidates))
                         (pcands (-map #'car pcands))
                         (pflt (--filter (member (car it) pcands) props)))
                    (helm
                     :sources (helm-build-sync-source "Values"
                                :candidates (odm/sort-uniq 2 (org-roam-search--mk-prop-candidates pflt))
                                :action
                                (lambda (_)
                                  (let* ((vcands (helm-marked-candidates))
                                         (vcands (-map #'cadr vcands))
                                         (vflt (--filter (member (cadr it) vcands) pflt)))
                                    (helm
                                     :sources (helm-build-sync-source "Pages"
                                                :nomark t
                                                :candidates (odm/sort-uniq 3 (org-roam-search--mk-val-candidates vflt))
                                                :action
                                                (lambda (x)
                                                  (org-roam-node-open (org-roam-node-from-id (elt x 2)))))))))
                     :buffer "*helm Prop search*"
                     :prompt "Select page: ")))))))

(defun org-roam-search-tags ()
  "Search by tags."
  (interactive)
  (setq odm/tags (org-roam-db-query [:select [tag node-id] :from tags]))
  (helm
   :sources (helm-build-sync-source "Tags"
              :candidates (odm/sort-uniq 0 (--map (cons (car it) it) odm/tags))
              :action
              '(("Include" . (lambda (_)
                               (let* ((tcands (helm-marked-candidates))
                                      (tcands (-map #'car tcands))
                                      (tflt (--filter (member (car it) tcands) odm/tags)))
                                 (helm
                                  :sources (helm-build-sync-source "Pages"
                                             :nomark t
                                             :candidates (odm/sort-uniq 1 (org-roam-search--mk-tag-candidates tflt))
                                             :action
                                             (lambda (x)
                                               (org-roam-node-open (org-roam-node-from-id (elt x 2)))))))))
                ("Exclude" . (lambda (_)
                               (let* ((tcands (helm-marked-candidates))
                                      (tcands (-map #'car tcands))
                                      (tflt (--remove (member (car it) tcands) odm/tags)))
                                 (helm
                                  :sources (helm-build-sync-source "Pages"
                                             :nomark t
                                             :candidates (odm/sort-uniq 1 (org-roam-search--mk-tag-candidates tflt))
                                             :action
                                             (lambda (x)
                                               (org-roam-node-open (org-roam-node-from-id (elt x 2)))))))))))
   :buffer "*helm Tag search*"
   :prompt "Select tags: "))

(provide 'org-roam-search)
;;; org-roam-search.el ends here
