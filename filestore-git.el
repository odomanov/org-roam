;;; filestore-git.el --- Git based filestore   -*- coding: utf-8; lexical-binding: t; -*-

(require 'dash)
(require 'magit)

(defun fs-index ()
  "Get the list of files."
  (magit-git-lines "ls-tree" "-r" "HEAD" "--name-only"))

(defun fs-content (file)
  "Get the content of FILE."
  (with-temp-buffer
    (magit-git-insert "cat-file" "blob" (concat "HEAD:" file))
    (buffer-string)))

(defun fs-insert (file)
  "Insert the content of FILE."
  (magit-git-insert "cat-file" "blob" (concat "HEAD:" file)))

(defun fs-commit (file &optional commit-message)
  "Commit FILE with optional COMMIT-MESSAGE."
  (magit-git-success "add" file)
  (setq commit-msg (or (commit-message)
                       (concat "Automatic commit: " file)))
  (if (magit-git-success "diff-index" "HEAD" "--cached")
      (magit-git-success "commit" "-m" commit-msg)))

(defun fs-checkout (file)
  "Checkout FILE."
  (magit-git-lines "ls-files" "--other")  ; untracked files
  )

(defun fs-update-buffer ()
  "Update current buffer."
  (buffer-file-name))

(defun org-roam-link--get-titles ()
  "Return all titles within Org-roam."
  (let ((files (fs-index))
        titles)
    (dolist (file files titles)
      (setq titles (append (fs-get-titles file) titles)))))

(defun fs-get-titles (file)
  "Returns the list of titles for FILE."
  (with-temp-buffer
    (fs-insert file)
    (org-roam--extract-titles)))

(defun org-roam-link--get-file-from-title (title &optional no-interactive)
  "Return the file path corresponding to TITLE.
When NO-INTERACTIVE, return nil if there are multiple options."
  (setq files (-filter (lambda (x) (-contains? (fs-get-titles x) title)) (fs-index)))
  (pcase files
    ('nil nil)
    (`(,file) file)
    (_
     (unless no-interactive
       (completing-read "Select file: " files)))))


;;  SERVER

(defun fs-get-edges ()
  "Get edges.
[to from type properties]"
  (let ((files (fs-index))
        links)
    (dolist (file files links)
      (with-temp-buffer
        (fs-insert file)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (goto-char (org-element-property :begin link))
          (let* ((type (org-roam--collate-types (org-element-property :type link)))
                 (path (org-element-property :path link))
                 (element (org-element-at-point))
                 (begin (or (org-element-property :content-begin element)
                            (org-element-property :begin element)))
                 (content (or (org-element-property :raw-value element)
                              (buffer-substring-no-properties
                               begin
                               (or (org-element-property :content-end element)
                                   (org-element-property :end element)))))
                 (content (string-trim content))
                 (properties (list :outline (org-roam--get-outline-path)
                                   :content content
                                   :point begin))
                 (names (pcase type
                          ("id"
                           (list (car (org-roam-id-find path))))
                          ("cite" (list path))
                          ("website" (list path))
                          ("fuzzy" (list path))
                          ("roam" (list path))
                          (_ (if (or (file-remote-p path)
                                     (org-roam--url-p path))
                                 (list path)
                               (let ((file-maybe (expand-file-name path (file-name-directory file-path))))
                                 (if (f-exists? file-maybe)
                                     (list file-maybe)
                                   (list path))))))))
            (dolist (name names)
              (when name
                (push (vector file name type properties) links))))))
      links))))

(defun org-roam-server-visjs-json (node-query)
  "Convert `org-roam` NODE-QUERY db query to the visjs json format."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer nil
    (let* ((-compare-fn (lambda (x y) (string= (car x) (car y))))
           (nodes (-distinct (org-roam-db-query node-query)))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [to from type] :from links
                    :where (and (in to selected) (in from selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [file from links:type] :from links
                    :inner :join refs :on (and (= links:to refs:ref)
                                               (= links:type "cite")
                                               (= refs:type "cite"))
                    :where (and (in file selected) (in from selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query))
           (graph (list (cons 'nodes (list))
                        (cons 'edges (list)))))
      (dotimes (idx (length nodes))
        (let* ((file (xml-escape-string (car (elt nodes idx))))
               (title (or (cadr (elt nodes idx))
                          (org-roam--path-to-slug file)))
               (tags (elt (elt nodes idx) 2)))
          (push (append (list (cons 'id (org-roam--path-to-slug file))
                              (cons 'title title)
                              (cons 'tags tags)
                              (cons 'label (s-word-wrap
                                            org-roam-server-network-label-wrap-length
                                            (if org-roam-server-network-label-truncate
                                                (s-truncate
                                                 org-roam-server-network-label-truncate-length
                                                 title)
                                              title)))
                              (cons 'url (concat "org-protocol://roam-file?file="
                                                 (url-hexify-string file)))
                              (cons 'path file))
                        (pcase org-roam-server-extra-node-options
                          ('nil nil)
                          ((pred functionp)
                           (funcall org-roam-server-extra-node-options (elt nodes idx)))
                          ((pred listp)
                           org-roam-server-extra-node-options)
                          (wrong-type
                           (error "Wrong type of org-roam-server-extra-node-options: %s"
                                  wrong-type))))
                (cdr (elt graph 0)))))
      (dolist (edge edges)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (append (list (cons 'from title-source)
                                          (cons 'to title-target)
                                          (cons 'arrows org-roam-server-network-arrows))
                                    (pcase org-roam-server-extra-edge-options
                                      ('nil nil)
                                      ((pred functionp)
                                       (funcall org-roam-server-extra-edge-options edge))
                                      ((pred listp)
                                       org-roam-server-extra-edge-options)
                                      (wrong-type
                                       (error "Wrong type of org-roam-server-extra-edge-options: %s"
                                              wrong-type)))))
                (cdr (elt graph 1)))))
      (dolist (edge edges-cites)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (append (list (cons 'from title-source)
                                          (cons 'to title-target)
                                          (cons 'arrows org-roam-server-network-arrows))
                                    (pcase org-roam-server-extra-edge-options
                                      ('nil nil)
                                      ((pred functionp)
                                       (funcall org-roam-server-extra-edge-options edge))
                                      ((pred listp)
                                       org-roam-server-extra-edge-options)
                                      (wrong-type
                                       (error "Wrong type of org-roam-server-extra-edge-options: %s"
                                              wrong-type)))))
                (cdr (elt graph 1)))))
      (json-encode graph))))

(provide 'filestore-git)
;;; filestore-git.el ends here

