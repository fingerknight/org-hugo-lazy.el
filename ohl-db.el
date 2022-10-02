;;; ohl-db.el --- SQLite API for Hugo Org Lazy

(require 'emacsql)

(defcustom ohl-db-path (f-expand "org-hugo-lazy.db" user-emacs-directory)
  "Where to save md5 of file cotent for Org Hugo"
  :group 'ohl-db
  :type 'string)

(defvar ohl-db--conn nil
  "Database handle.")

(defun ohl-db-load ()
  "Load database"
  (let ((db-exists-p (f-exists-p ohl-db-path)))
    (setq ohl-db--conn (emacsql-sqlite ohl-db-path))
    (unless db-exists-p
      (emacsql ohl-db--conn [:create-table data
			     ([(id integer :primary-key)
			       (file string :unique)
			       (lastmod integer)
			       (not-kill integer)])]))
    (message "[Org Hugo Lazy] Database loaded: %s" ohl-db-path)))

(defun ohl-db-get (key)
  "Get value from database"
  (let ((res (emacsql ohl-db--conn [:select lastmod
				    :from data
				    :where (= file $s1)]
		      key)))
    (when res (setq res (caar res)))
    res))

(defun ohl-db-set (key val)
  "Set value for key to database"
  (if (ohl-db-get key)
      (emacsql ohl-db--conn [:update data
			     :set [(= lastmod $s2) (= not-kill 1)] 
			     :where (= file $s1)]
	       key val)
    (emacsql ohl-db--conn [:insert
			   :into data
			   :values ([nil $s1 $s2 1])]
	     key val)))

(defun ohl-db-clear-old ()
  "Clear that old/non-existent items"
  (emacsql ohl-db--conn [:delete-from data
			 :where (= not-kill 0)])
  (emacsql ohl-db--conn [:update data
			 :set (= not-kill 0)]))

(provide 'ohl-db)
;;; ohl-db.el ends

