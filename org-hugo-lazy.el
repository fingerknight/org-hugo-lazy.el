;;; org-hugo-lazy.el --- A workflow from org to hugo  -*- lexical-binding: t -*-

;; Author: Fingker Knight <mrdust1880@outlook.com>
;; URL: https://github.com/fingerknight/jieba.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28") (use-package "2.4.1") (emacsql "3.0.0") (ox-hugo "0.12.1") (f "0.20.0") (s "1.13.1"))
;; Keywords: org-mode hugo

(require 'f)
(require 's)

(defcustom org-hugo-lazy-git-commit-date-format "%Y%m%d%H%M%S. Updated by Org Hugo Lazy."
  "Formatter of git commit using `format-time-strig'"
  :group 'org-hugo-lazy
  :type 'string)

(defcustom org-hugo-lazy-git-commit-shell-path "/tmp/orghugolazy.sh"
  "Where to save generated shell script"
  :group 'org-hugo-lazy
  :type 'string)

(defcustom org-hugo-lazy-git-script-template
  (concat "#!/bin/bash\n"
	  "cd $1\n"
	  "git add *\n"
	  "git commit -a -m \"$2\"\n"
	  "git push")
  "The script to git commit"
  :group 'org-hugo-lazy
  :type 'string)

(defcustom org-hugo-lazy-git-repo-dir "project"
  "Where the github repo is. Default is `./project'"
  :group 'org-hugo-lazy
  :type 'string
  )

(defcustom org-hugo-lazy-db-path (f-expand "org-hugo-lazy.db" user-emacs-directory)
  "Where to save md5 of file cotent for Org Hugo"
  :group 'org-hugo-lazy
  :type 'string)

(defcustom org-hugo-lazy-source-dir ""
  "Where to save the raw posts"
  :group 'org-hugo-lazy
  :type 'string)

(defcustom org-hugo-lazy-auto-gitalk nil
  "Set t so that gitalk will be settle down automatically"
  :group 'org-hugo-lazy
  :type 'boolean)

(defvar org-hugo-lazy--loaded nil
  "A sigh used to check if deps loaded")

(defvar org-hugo-lazy--git-issue-list nil
  "List records all the ISSUEs.
Each ISSUE is a assoc-list. Key is the md5 of relative path of the post.\
Value is the ordered number of Github Issue")

(defvar org-hugo-lazy--db nil
  "Database handle for Org Hugo")

(defun org-hugo-lazy-message (str &rest args)
  "Print message with prefix \"[Org Hugo Lazy]\"."
  (apply #'message
	 (append `(,(concat "[Org Hugo Lazy] " str))
		 args)))

(defun eh-org-clean-space-for-md (text backend info)
  "在export为 MarkDown时,删除中文之间不必要的空格"
  (when (or (org-export-derived-backend-p backend 'hugo))
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org默认将一个换行符转换为空格,但中文不需要这个空格,删除.
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) +\\(\\*\\*%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\*\\*\\) +\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      string)))

;;;###autoload
(defun org-hugo-lazy-load-all ()
  "A activating function"
  (interactive)
  (unless org-hugo-lazy--loaded
    (org-hugo-lazy-message "Loading all the configurations...")
    (require 'ox)
    (require 'ox-hugo)
    (require 'emacsql)
    
    (setq org-hugo-default-static-subdirectory-for-externals "img"
	  org-hugo-auto-set-lastmod t
	  org-hugo-export-with-toc nil)
    
    ;; 修改 center ，不加 <style> ，在 css 里面调样式
    (setq org-blackfriday-center-block
	  (lambda (_center-block contents _info)
	    "Center-align the text in CONTENTS using CSS."
	    (let* ((class "org-center"))
	      (format "<div class=\"%s\">\n  <div></div>\n\n%s\n</div>" ;See footnote 1
		      class contents))))

    (add-to-list 'org-export-filter-paragraph-functions
		 'eh-org-clean-space-for-md)

    (unless org-hugo-lazy--db
      (org-hugo-lazy-db-load))
    
    (setq org-hugo-lazy--loaded t)))

;;; ----------
;;; Database
(with-eval-after-load 'emacsql

  (defun org-hugo-lazy-db-load ()
    "Load database"
    (let ((db-exists-p (f-exists-p org-hugo-lazy-db-path)))
      (setq org-hugo-lazy--db (emacsql-sqlite org-hugo-lazy-db-path))
      (unless db-exists-p
	(emacsql org-hugo-lazy--db [:create-table data
				    ([(id integer :primary-key)
				      (file string :unique)
				      (lastmod integer)
				      (not-kill integer)])]))
      (org-hugo-lazy-message "Database loaded: %s" org-hugo-lazy-db-path)))

  (defun org-hugo-lazy-db-get (key)
    "Get value from database"
    (let ((res (emacsql org-hugo-lazy--db [:select lastmod
					   :from data
					   :where (= file $s1)]
			key)))
      (when res (setq res (caar res)))
      res))

  (defun org-hugo-lazy-db-set (key val)
    "Set value for key to database"
    (if (org-hugo-lazy-db-get key)
	(emacsql org-hugo-lazy--db [:update data
				    :set [(= lastmod $s2) (= not-kill 1)] 
				    :where (= file $s1)]
		 key val)
      (emacsql org-hugo-lazy--db [:insert
				  :into data
				  :values ([nil $s1 $s2 1])]
	       key val)))

  (defun org-hugo-lazy-db-clear-old ()
    "Clear that old/non-existent items"
    (emacsql org-hugo-lazy--db [:delete-from data
				:where (= not-kill 0)])
    (emacsql org-hugo-lazy--db [:update data
				:set (= not-kill 0)]))
  )
;;; ----------

;;; ----------
;;; Git

(defun org-hugo-lazy--gen-shell ()
  "Generate git commit shell script."
  (unless (f-exists-p   org-hugo-lazy-git-commit-shell-path)
    (with-temp-file org-hugo-lazy-git-commit-shell-path
      (insert org-hugo-lazy-git-script-template))))

(defun org-hugo-lazy--git-commit ()
  "Try to generate git sh script, and commit."
  (org-hugo-lazy--gen-shell)
  (async-shell-command (format "/bin/bash %s %s \"%s\""
			       org-hugo-lazy-git-commit-shell-path
			       (f-expand "project/" org-hugo-base-dir)
			       (format-time-string org-hugo-lazy-git-commit-date-format))))

(defun org-hugo-lazy--git-get-issue-list ()
  "Get all comments(issues) from repo"
  (let* ((dir org-hugo-lazy-git-repo-dir)
	 (cmd-output (shell-command-to-string
		      (format "cd %s; gh issue list" dir)))
	 (issue-list (mapcar #'(lambda (each) (s-split-words each))
			     (cl-subseq (s-lines cmd-output) 0 -1)))
	 (tmp '()))
    (when issue-list
      (dolist (issue issue-list)
        (let ((label-list (s-split-words (nth 3 issue))))
	  (when (and (member "Gitalk" label-list)
		     (length= label-list 2))
	    
	    (push `(,(car (remove "Gitalk" (s-split-words (nth 3 issue))))
		    .
		    ,(car issue))
		  tmp)))))
      (setq org-hugo-lazy--git-issue-list tmp)))
    
(defun org-hugo-lazy--git-add-issue (title uri id)
  "Inital a new issue used to comment"
  (unless (cdr (assoc-string id org-hugo-lazy--git-issue-list))
    (let ((cmd-formatter "cd %s; gh label create \"%s\"; gh issue create --title \"%s\" --body \"%s\" --label \"Gitalk,%s\"")
	  (dir org-hugo-lazy-git-repo-dir))
      (org-hugo-lazy-message "Adding new issue: %s, %s" title id)
      (org-hugo-lazy-message (shell-command-to-string (format cmd-formatter
							      dir id title uri id))))))

;;; ----------


;;; ----------
;;; org to md

(defun org-hugo-lazy--gernerate-single (&optional file-name new-issue-p force)
  "Export a single org file FILE-NAME to md.

Default: NEW-ISSUE-P: nil, FORCE: nil.

If NEW-ISSUE-P is t, then try to create a new issue according to post.
If FORCE is t, then export even file is not modified.

If functio is called interactively, then FORCE is t."
  (interactive)
  (setq file-name (if file-name file-name
		    (buffer-file-name))
	force (or (called-interactively-p 'any)
		  force))

  (org-hugo-lazy-load-all)
  
  (let* ((file-short-name (f-relative file-name org-hugo-lazy-source-dir))
	 (file-buffer (get-file-buffer file-name))
	 (file-buffer-exists-p file-buffer)
	 (file-time (org-time-convert-to-integer (f-modification-time file-name)))
	 (file-time-in-db (unless force (org-hugo-lazy-db-get file-short-name)))
	 (title "")
	 (outfile ""))
    (org-hugo-lazy-message "Processing: %s" file-name)
    
    (unless file-buffer-exists-p
	(setq file-buffer (find-file file-name)))
    
    (with-current-buffer file-buffer
      (setq title (s-trim (nth 1 (s-match "#\\+[tT][iI][tT][lL][eE]:\\([^\n]+\\)\n" (buffer-string)))))
      (if (and file-time-in-db
	       (equal file-time file-time-in-db))
	  ;; This file has not been modified.
	  (org-hugo-lazy-message "Skipping unmodified file: %s" file-name)
	;; New file or modified file
	(setq outfile (org-hugo-export-wim-to-md t nil))

	;; Try to create new issue
	(when (and new-issue-p org-hugo-lazy-auto-gitalk)
	  (org-hugo-lazy--git-add-issue title
					(downcase (concat org-hugo-blog-url (f-base file-name) "/"))
					(md5 (f-relative outfile (f-expand "content/" org-hugo-base-dir))))))
      
      (unless file-buffer-exists-p
	(kill-buffer file-buffer)))

    (org-hugo-lazy-db-set file-short-name  file-time)
    (org-hugo-lazy-message "Done: %s" file-name)))


;;;###autoload
(defun org-hugo-lazy-generate ()
  "Transform all the org files in project to markdown.
- If `org-hugo-lazy-generate' is called interactively,
  it won't emit operations about issus, i.e., only org-md
  transmission emits.
- Else, it will try to create github labels and create a
  new issue used to comment.

`org-hugo-lazy--gernerate-single' is used for each file.
You can specify files needed to be transformed by force."
  (interactive)
  (org-hugo-lazy-load-all)
  
  (let ((cur-bf (buffer-name))
	(new-issue-p (not (called-interactively-p 'any))))
    
    (when (and new-issue-p org-hugo-lazy-auto-gitalk)
      (org-hugo--git-get-issue-list))

    (dolist (file-name (f-files org-hugo-lazy-source-dir
				(lambda (file) (equal (f-ext file) "org"))
				t))
      (org-hugo-lazy--gernerate-single file-name new-issue-p nil))
    
    (org-hugo-lazy-db-clear-old)
    (switch-to-buffer cur-bf)))

;;; ----------


;;; ----------
;;; entrance
;;;###autoload
(defun org-hugo-lazy-publish ()
  "Publish posts to Github"
  (interactive)
  ;; Export org to md
  (org-hugo-lazy-generate)
  ;; Export md to HTML
  (org-hugo-lazy-message (shell-command-to-string (format "cd %s; hugo" org-hugo-base-dir)))
  ;; Commit post
  (org-hugo-lazy--git-commit)
  )

(defun org-hugo-lazy-insert-formatter (title tags-string category)
  (interactive "MPost Title: \nMTags(seperated by space): \nMCategory(could be empty): \n")
  (insert (format "#+title: %s\n" title)
	  (format-time-string "#+date: [%Y-%m-%d %a]\n")
	  (format "#+hugo_tags: %s\n" tags-string)
	  (if (length> category 0)
	      (format "#+hugo_categories: %s\n" category)
	    "")
	  "#+hugo_draft: true\n\n"))
;;; ----------

(provide 'org-hugo-lazy)
;;; org-hugo-lazy.el ends
