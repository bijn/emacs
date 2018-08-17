;; Bijan Sondossi
;; functions.el
;; Functions and stuff.

;; SSH helper functions.
(defun ssh-edit (login file)
  "Edit user@host:file via SSH."
  (let ((ssh-method "/ssh:"))
    ;; https://www.emacswiki.org/emacs/Tramp_on_Windows
    (when (eq system-type 'windows-nt) (setq ((ssh-method "/plink:"))))
    (dired (concat ssh-method login ":" file))))

(defun ssh-edit-file (login file)
  "Interactive ssh-edit wrapper."
  (interactive "sEnter user@host (or identity): \nsFile: ")
  (ssh-edit login file))

(defun ssh (login)
  "Opens an ssh session at the $HOME directory of 'login'."
  (interactive "sEnter user@host (or identity): ")
  (ssh-edit login "~"))

;; Exuberant CTags
(setq bijans/ctags-path "/usr/bin/ctags")
(defun make-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -f TAGS -R %s" bijans/ctags-path
           (directory-file-name dir-name))))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath. Source: \
   http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;; end functions.el
