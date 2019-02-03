;; Bijan Sondossi
;; functions.el
;; Functions and stuff.

;; SSH helper functions ------------------------------------------------

(defun bijans/ssh-edit (login file)
  "Edit user@host:file via SSH."
  (let ((ssh-method "/ssh:"))
    ;; https://www.emacswiki.org/emacs/Tramp_on_Windows
    (when (eq system-type 'windows-nt) (setq ((ssh-method "/plink:"))))
    (dired (concat ssh-method login ":" file))))

(defun bijans/ssh-edit-file (login file)
  "Interactive ssh-edit wrapper."
  (interactive "sEnter user@host (or identity): \nsFile: ")
  (bijans/ssh-edit login file))

(defun bijans/ssh (login)
  "Opens an ssh session at the $HOME directory of 'login'."
  (interactive "sEnter user@host (or identity): ")
  (bijans/ssh-edit login "~"))

;; CTags ---------------------------------------------------------------

(defun bijans/make-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -f TAGS -R %s" bijans/ctags-path
           (directory-file-name dir-name))))

;; Other ---------------------------------------------------------------

(defun bijans/read-lines (filePath)
  "Return a list of lines of a file at filePath. Source: \
   http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
