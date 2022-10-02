;;; ohl-git.el --- Git commit for Hugo Org Lazy

(defcustom ohl-git-commit-date-format "%Y%m%d%H%M%S. Updated by Org Hugo Lazy."
  "Formatter of git commit using `format-time-strig'"
  :group 'ohl-git
  :type 'string)

(defcustom ohl-git-commit-shell-path "/tmp/orghugolazy.sh"
  "Where to save generated shell script"
  :group 'ohl-git
  :type 'string)

(defcustom ohl-git-script-template
  (concat "#!/bin/bash\n"
	  "cd $1\n"
	  "git add *\n"
	  "git commit -a -m \"$2\"\n"
	  "git push")
  "The script to git commit"
  :group 'ohl-git
  :type 'string)

(defun ohl-git--gen-shell ()
  "Generate git commit shell script."
  (unless (f-exists-p ohl-git-commit-shell-path)
    (with-temp-file ohl-git-commit-shell-path
      (insert ohl-git-script-template))))

(defun ohl-git-commit (plist)
  "Try to generate git sh script, and commit."
  (ohl-git--gen-shell)
  (async-shell-command (format "/bin/bash %s %s \"%s\""
			       ohl-git-commit-shell-path
			       (plist-get plist :repository-directory)
			       (format-time-string ohl-git-commit-date-format))))

(provide 'ohl-git)
;;; ohl-git.el ends
