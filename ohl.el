;;; ohl.el --- A workflow from org to hugo  -*- lexical-binding: t -*-

;; Author: Fingker Knight <mrdust1880@outlook.com>
;; URL: https://github.com/fingerknight/ohl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28") (use-package "2.4.1") (emacsql "3.0.0") (ox-hugo "0.12.1") (f "0.20.0") (s "1.13.1"))
;; Keywords: org-mode hugo

(require 'f)
(require 's)
(require 'ohl-gitalk)
(require 'ohl-db)
(require 'ohl-git)

(defcustom ohl-project-plist nil
  "A plist containing export projects.
Each project:
(\"project name\"

:base-directory
STRING
Where the Hugo project is

:static-relative-directory
STRING assets
The relavtive path of statics to /

:source-directory
STRING
Where the org files locate

:repository-directory
STRING
Where the GitHub repo is

:url
STRING
Blog's URL

:gitalk
BOOL, nil
If automatically handle with Gitalk

:paried-shortcodes
STING \"\"
Allowed shortcodes, which is seperated by space

:auto-lastmod
BOOL t

:with-toc
BOOL 
)"
  :group 'ohl
  :type 'alist)

(defvar ohl--loaded nil
  "A sign used to check if deps loaded")

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
(defun ohl-load ()
  "A activating function"
  (interactive)
  (unless ohl--loaded
    (message "[Org Hugo Lazy] Loading the configurations...")
    (require 'ox)
    (require 'ox-hugo)

    ;; (setq org-hugo-default-static-subdirectory-for-externals "img"
    ;; 	  org-hugo-auto-set-lastmod t
    ;; 	  org-hugo-export-with-toc nil)

    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" . (:raw t)))
   
    ;; 修改 center ，不加 <style> ，在 css 里面调样式
    (setq org-blackfriday-center-block
	  (lambda (_center-block contents _info)
	    "Center-align the text in CONTENTS using CSS."
	    (let* ((class "org-center"))
	      (format "<div class=\"%s\">\n  <div></div>\n\n%s\n</div>" ;See footnote 1
		      class contents))))

    (add-to-list 'org-export-filter-paragraph-functions
		 'eh-org-clean-space-for-md)

    (unless ohl-db--conn
      (ohl-db-load))
    
    (setq ohl--loaded t)))

(defun ohl--gernerate-single (project file-name plist)
  "Export a single org file FILE-NAME to md.
PLIST is communication property-list."
  (let* ((file-short-name (f-relative file-name
				      (plist-get plist :source-directory)))
	 (file-buffer (get-file-buffer file-name))
	 (file-buffer-exists-p file-buffer)
	 (file-time (org-time-convert-to-integer (f-modification-time file-name)))
	 (file-time-in-db (ohl-db-get project file-short-name))
	 (outfile ""))
    (message "[Org Hugo Lazy] Processing: %s" file-name)
    
    (unless file-buffer-exists-p
	(setq file-buffer (find-file file-name)))
    
    (with-current-buffer file-buffer
      
      (if (and file-time-in-db
	       (= file-time file-time-in-db))
	  ;; This file has not been modified.
	  (message "Skipping unmodified file: %s" file-name)
	;; New file or modified file
	(setq outfile (org-hugo-export-wim-to-md t nil))

	
	(when (plist-get plist :gitalk)
	  ;; Try to create new issue
	  (ohl-gitalk--git-add-issue (plist-get plist :repository-directory)
				     (s-trim (nth 1 (s-match "#\\+[tT][iI][tT][lL][eE]:\\([^\n]+\\)\n"
							     (buffer-string))))
				     (downcase (concat (plist-get plist :url)
						       (f-base file-name) "/"))
				     (md5 (f-relative outfile
						      (f-expand "content/"
								(plist-get plist :base-directory)))))))
      
      (unless file-buffer-exists-p
	(kill-buffer file-buffer)))

    (ohl-db-set project file-short-name file-time)
    (message "Done: %s" file-name)))


;;;###autoload
(defun ohl-generate (&optional project plist)
  "Transform all the org files in project to markdown.
PLIST is communication channel.

IF this function is called interactively, then GITALK will \
be forbidden.

`ohl--gernerate-single' is used for each file."
  (interactive)

  (when (interactive-p)
    (ohl-load)
    (setq project (completing-read
		   "Project: "
		   (mapcar #'car
			   ohl-project-plist)
		   nil t)
	  plist (cdr (assoc-string
		      project
		      ohl-project-plist)))
    (plist-put plist :gitalk nil))
  
  (setq org-hugo-base-dir
	(plist-get plist :base-directory)
	
        org-hugo-default-static-subdirectory-for-externals
	(if (plist-member plist :static-relative-directory)
	      (plist-get plist :static-relative-directory)
	    "assets")
	
	org-hugo-auto-set-lastmod
	  (if (plist-member plist :auto-lastmod)
	      (plist-get plist :auto-lastmod)
	    t)
	  
	org-hugo-export-with-toc
	  (if (plist-member plist :with-toc)
	      (plist-get plist :with-toc)
	    nil)
	  
	org-hugo-paired-shortcodes
          (if (plist-member plist :paired-shortcodes)
	      (plist-get plist :paired-shortcodes)
	    ""))
    
  (let ((cur-bf (buffer-name)))
    (--map
     (ohl--gernerate-single project it plist)
     (f-files (plist-get plist :source-directory)
	      (lambda (file) (s-equals-p (f-ext file) "org"))
	      t))
    
    (ohl-db-clear-old project)
    (switch-to-buffer cur-bf)))

;;;###autoload
(defun ohl-publish (&optional project)
  (interactive)
  (ohl-load)
  (let* ((project-name
	 (if project project
	   (completing-read "Project: "
			    (mapcar #'car
				    ohl-project-plist)
			    nil t)))
	 (plist (cdr (assoc-string project-name
				   ohl-project-plist))))
    
    (when (plist-get plist :gitalk)
      (ohl-gitalk--get-issue-list (plist-get plist :repository-directory)))

    ;; Export org to md
    (ohl-generate project-name plist)

    ;; Export md to HTML
    (message (shell-command-to-string (format "cd %s; hugo"
					      (plist-get plist :base-directory))))

    ;; Git commit
    (ohl-git-commit plist)
    ))

;;;###autoload
(defun ohl-publish-all ()
  (interactive)
  (mapcar #'(lambda (project)
	      (ohl-publish (car project)))
	  ohl-project-plist))

;;;###autoload
(defun ohl-new-blog (title tags-string)
  (interactive "MPost Title: \nMTags(seperated by space): \nMCategory(could be empty): \n")
  (insert (format "#+title: %s\n" title)
	  (format-time-string "#+date: [%Y-%m-%d %a]\n")
	  (format "#+hugo_tags: %s\n" tags-string)
	  "#+hugo_draft: true\n\n"))

(defun ohl-new-tutorial (section title)
  (interactive "MSection:\nM Title: \n")
  (insert "#+title: " title "\n"
	  (format-time-string "#+date: [%Y-%m-%d %a]\n")
	  "#+hugo_section: " section "\n"
	  "#+hugo_weight: "))

(provide 'ohl)
;;; ohl.el ends
