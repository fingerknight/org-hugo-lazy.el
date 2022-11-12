;;; ohl-gitalk.el --- Gitalk in Org Hugo Lazy  -*- lexical-binding: t -*-

;; Author: Fingker Knight <mrdust1880@outlook.com>
;; URL: https://github.com/fingerknight/org-hugo-lazy.el
;; Version: 0.0.1

(defvar ohl-gitalk--issue-list nil
  "List records all the ISSUEs.
Each ISSUE is a assoc-list. Key is the md5 of relative path of the post.\
Value is the ordered number of Github Issue")

(defun ohl-gitalk--get-issue-list (dir)
  "Get all comments(issues) from repo"
  (message "[Org Hugo Lazy] Getting Issue List...")
  (let* ((cmd-output (shell-command-to-string
		      (format "cd %s; gh issue list" dir)))
	 (issue-list (mapcar #'(lambda (each)
				 (s-split "\t" each))
			     (cl-subseq (s-lines cmd-output)
					0 -1))))
    (setq ohl-gitalk--issue-list
	  (when issue-list
	    (--map-when
	     (funcall #'(lambda (label-list)
			  (and (member "Gitalk" label-list)
			       (length= label-list 2)))
		      (s-split-words (nth 3 it)))
	     (car (remove "Gitalk" (s-split-words (nth 3 it))))
	    issue-list))))

  (message (if ohl-gitalk--issue-list
			   "Issue list gotten"
			 "Failed to get issue list")))
    
(defun ohl-gitalk--git-add-issue (dir title uri id)
  "Inital a new issue used to comment"
  (when (and ohl-gitalk--issue-list
	     (not (member id ohl-gitalk--issue-list)))
    (let ((cmd-formatter "cd %s; gh label create \"%s\"; gh issue create --title \"%s\" --body \"%s\" --label \"Gitalk,%s\""))
      (message "[Org Hugo lazy] Adding new issue: %s, %s" title id)
      (message (shell-command-to-string (format cmd-formatter
					       dir id title uri id))))))

(provide 'ohl-gitalk)
