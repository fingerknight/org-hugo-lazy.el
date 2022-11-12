;;; ohl-git.el --- Git commit for Hugo Org 

(defun ohl-git-format-script (repo-dir)
  (format "cd %s; git add *; git commit -a -m \"%s. Updated by OHL\"; git push; echo \"\n[Org Hugo Lazy] %s pushed.\n\"; "
		  repo-dir
		  (format-time-string "%Y%m%d%H%M%S")
		  repo-dir))

(provide 'ohl-git)
;;; ohl-git.el ends
