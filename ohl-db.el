;;; ohl-db.el --- JSON API for Hugo Org Lazy

;; (require 'emacsql)

(defcustom ohl-db-dir (f-expand "ohl" user-emacs-directory)
  "Where to save last modification time for files"
  :group 'ohl-db
  :type 'string)

(defvar ohl-db--obj nil
  "An alist of JSON. KEY is symbol")

(defun ohl-db-load (project)
  "Load JSON database, return the file list"
  (with-temp-buffer
	(find-file-literally (f-expand
						  (concat project ".json")
						  ohl-db-dir))
	(unless (f-exists-p (f-this-file))
	  (insert "{}")
	  (save-buffer)
	  (beginning-of-buffer))
	(setq ohl-db--obj
		  (json-parse-buffer :object-type 'alist))
	(kill-buffer))
  (--map
   (symbol-name (car it))
   ohl-db--obj))

(defun ohl-db-save (project)
  "Save alist to json file"
  (with-temp-file (f-expand
				   (concat project ".json")
				   ohl-db-dir)
	(insert (json-serialize ohl-db--obj)))
  (setq ohl-db--obj nil))

(defun ohl-db-get (filename)
  (when (eql (type-of filename) 'string)
	(setq filename (intern filename)))
  (assoc-default filename ohl-db--obj))

(defun ohl-db-update-files (table)
  "TABLE: '((file . lastmod))"
  (--map
   (progn
	 (assoc-delete-all (car it) ohl-db--obj)
	 (push it ohl-db--obj))
   table))

(defun ohl-db-delete-files (lst)
  "TABLE: '((file . lastmod), for convenience"
  (--map
   (assoc-delete-all (intern it) ohl-db--obj)
   lst))

(provide 'ohl-db)
;;; ohl-db.el ends

