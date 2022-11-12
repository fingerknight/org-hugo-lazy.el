;;; ohl.el --- A workflow from org to hugo  -*- lexical-binding: t -*-

;; Author: Fingker Knight <mrdust1880@outlook.com>
;; URL: https://github.com/fingerknight/ohl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28") (ox-hugo "0.12.1") (dash "2.19.1") (f "0.20.0") (s "1.13.1"))
;; Keywords: org-mode hugo

;; TODO
;; - [-] output filename to md5 by using `slug' or `filename'
;; - [ ] next/prev page in special pages
;; - [X] after exporting, don't not close buffers open before
;; - [-] way to sort in certain category.
;; - [X] do not use org-project-plist as communication channel, its copy instead.


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

:md5 - discarded
BOOL nil
use md5 replacing output md file's name.

:md5-include - discarded
LIST[REGEX-STRING] nil
when :md5 is nil, specify what files rename with md5

:md5-exclude - discarded
LIST[REGEX-STRING] nil
when :md5 is t, specify what files do not rename with md5

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

(defun ohl-transform-tab-to-space-in-trim (text backend info)
  "将落在行首或行尾的 \t 转换为 4 个空格"
  (when (or (org-export-derived-backend-p backend 'hugo))
    (let ((regexp "^\\(\t+\\)\\|\\(\t\\)$")
          (string text))
      (setq string
			(replace-regexp-in-string
			 regexp
			 #'(lambda (match) (s-repeat (s-count-matches "\t" match) "    "))
			 string))
      string)))

(defun ohl-load ()
  "A activating function"
  (interactive)
  (unless ohl--loaded
    (message "[Org Hugo Lazy] Loading the configurations...")
    (require 'ox)
    (require 'ox-hugo)

    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" . (:raw t)))
    
    ;; 修改 center ，不加 <style> ，在 css 里面调样式
    (setq org-blackfriday-center-block
		  (lambda (_center-block contents _info)
			"Center-align the text in CONTENTS using CSS."
			(let* ((class "org-center"))
			  (format "<div class=\"%s\">\n%s\n</div>" 
					  class contents))))

    (add-to-list 'org-export-filter-paragraph-functions
				 'eh-org-clean-space-for-md)
	(add-to-list 'org-export-filter-src-block-functions
				 'ohl-transform-tab-to-space-in-trim)
    
    (setq ohl--loaded t)))

(defun ohl--gernerate-single (project filename plist)
  "Export a single org file FILE-NAME to md.
PLIST is communication property-list."
  (let* ((file-short-name (f-relative filename
									  (plist-get plist :source-directory)))
		 (file-time (org-time-convert-to-integer (f-modification-time filename)))
		 (delete-list (plist-get plist :delete-list))
		 (file-time-in-db (ohl-db-get file-short-name))
		 (outfile ""))

	(message "[Org Hugo Lazy] Processing: %s" filename)

	(if (and file-time-in-db
			 (= file-time file-time-in-db))
		;; This file has not been modified.
		(message "Skipping unmodified file: %s" filename)

	  ;; New file or modified file
	  (with-temp-buffer
		(find-file filename)
		(setq outfile (org-hugo-export-wim-to-md t nil))

		(when (plist-get plist :gitalk)
		  ;; Try to create new issue
		  (ohl-gitalk--git-add-issue (plist-get plist :repository-directory)
									 (s-trim (nth 1 (s-match "#\\+[tT][iI][tT][lL][eE]:\\([^\n]+\\)\n"
															 (buffer-string))))
									 (downcase (concat (plist-get plist :url)
													   (f-base filename) "/"))
									 (md5 (f-relative outfile
													  (f-expand "content/"
																(plist-get plist :base-directory))))))
		(unless (or (eql (buffer-name)
						 (plist-get plist :current-buffer))
					(member (buffer-file-name)
							(plist-get plist :buffer-list)))
		  (kill-buffer)))

	  (let ((lst (plist-get plist :update-list)))
		(push (cons (intern file-short-name) file-time) lst)
		(plist-put plist :update-list lst))

	  
	  ;; (when (or (and (plist-get plist :md5)
	  ;; 				 (--every-p (not (s-matches-p it
	  ;; 											  (f-filename filename)))
	  ;; 							(plist-get plist :md5-exclude)))
	  ;; 			(and (not (plist-get plist :md5))
	  ;; 				 (--any-p (s-matches-p it
	  ;; 									   (f-filename filename))
	  ;; 						  (plist-get plist :md5-include))))
	  ;; 	(rename-file outfile
	  ;; 				 (f-expand (concat (md5 file-short-name)
	  ;; 								   ".md")
	  ;; 						   (f-dirname outfile))
	  ;; 				 t))
	  
	  (message "Done: %s" filename))

	;; remove name from the table
	;; since the rest will be deleted from database
	(plist-put plist :delete-list (remove file-short-name delete-list))))


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
		  plist (copy-alist (cdr (assoc-string
								  project
								  ohl-project-plist))))
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

  (plist-put plist :current-buffer (buffer-name))
  (plist-put plist :buffer-list (remove nil (-map #'buffer-file-name (buffer-list))))

  (plist-put plist :update-list nil)
  (plist-put plist :delete-list (ohl-db-load project))
  (--map
   (ohl--gernerate-single project it plist)
   (f-files (plist-get plist :source-directory)
			(lambda (file) (s-equals-p (f-ext file) "org"))
			t))

  (setq dbg plist)
  ;; clean old items
  (ohl-db-delete-files (plist-get plist :delete-list))
  ;; update items
  (ohl-db-update-files (plist-get plist :update-list))
  ;; save json
  (ohl-db-save project))

;;;###autoload
(defun ohl-publish (&optional project wait)
  (interactive)
  (ohl-load)
  (let* ((project-name
		  (if project project
			(completing-read "Project: "
							 (mapcar #'car
									 ohl-project-plist)
							 nil t)))
		 (plist (copy-alist (cdr (assoc-string project-name
											   ohl-project-plist)))))

	(when (plist-get plist :gitalk)
	  (ohl-gitalk--get-issue-list (plist-get plist :repository-directory)))

	;; Export org to md
	(ohl-generate project-name plist)

	;; Export md to HTML
	(message (shell-command-to-string (format "cd %s; hugo"
											  (plist-get plist :base-directory))))
	(message "[Org Hugo Lazy] HTML exported")

	;; Git commit
	(let ((script  (ohl-git-format-script (plist-get plist :repository-directory))))
	  (if wait script
		(async-shell-command script)))
	))

;;;###autoload
(defun ohl-publish-all ()
  "Publish all projects"
  (interactive)
  (async-shell-command
   (--reduce-from
	(concat acc (ohl-publish (car it) t))
	""
	ohl-project-plist))
  (message "[Org Hugo Lazy] All published."))

(provide 'ohl)
;;; ohl.el ends
