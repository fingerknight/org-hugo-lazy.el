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
			       (project string)
			       (file string)
			       (lastmod integer)
			       (not-kill integer)])]))
    (message "[Org Hugo Lazy] Database loaded: %s" ohl-db-path)))

(defun ohl-db-get (project file)
  "Get last modification date from database"
  (let ((res (emacsql ohl-db--conn [:select lastmod
				    :from data
				    :where (and (= project $s1)
						(= file $s2))]
		      project file)))
    (when res (setq res (caar res)))
    res))

(defun ohl-db-get-all (project)
  "Get all items of PROJECT"
  (--map (cons (car it) (cadr it))
	 (emacsql ohl-db--conn
		  [:select [file lastmod]
		   :from data
		   :where (= project $s1)]
		  project)))

(defun ohl-db-add-files (project table)
  "TABLE: '((file . lastmod))"
  (unless table
    (emacsql ohl-db--conn
	     [:insert-into data
	      :values $v1]
	     (--map (vector nil project
			    (car it)
			    (cdr it))
		    table))))

(defun ohl-db-delete-files (project table)
  "TABLE: '((file . lastmod)
for convenience"
  (unless table
    (emacsql ohl-db--conn
	     [:delete-from data
	      :where (and (in file $v1)
			  (= project $s2))]
	     (vconcat (-map #'car table))
	     project)))


(defun ohl-db-set (project file date)
  "Set last modification date for file in project to database"
  (if (ohl-db-get project file)
      (emacsql ohl-db--conn [:update data
			     :set [(= lastmod $s3) (= not-kill 1)] 
			     :where (and (= project $s1)
					 (= file $s2))]
	       project file date)
    (emacsql ohl-db--conn [:insert
			   :into data
			   :values ([nil $s1 $s2 $s3 1])]
	     project file date)))

(defun ohl-db-clear-old (project)
  "Clear that old/non-existent items"
  (emacsql ohl-db--conn [:delete-from data
			 :where (and (= project $s1)
				     (= not-kill 0))]
	   project)
  (emacsql ohl-db--conn [:update data
			 :set (= not-kill 0)
			 :where (= project $s1)]
	   project))

(provide 'ohl-db)
;;; ohl-db.el ends

