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

(provide 'filestore-git)
;;; filestore-git.el ends here

