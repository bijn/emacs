;; Bijan Sondossi
;; defaults.el
;; Default variables and settings.

;; Organize messy ~backup files into one folder.
(let ((backup-dir "~/.emacs.d/backups/"))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
  (when (not (file-accessible-directory-p backup-dir))
    (make-directory backup-dir)))

;; Custom set variables save location.
(setq custom-file "~/.emacs.d/customs.el")
(when (file-exists-p custom-file) (load custom-file))

;; Silently add newline to files.
;; Values: t, nil, or 'query if you want a prompt to add a newline.
(setq require-final-newlne t)

;; Alarm bell, [[https://www.emacswiki.org/emacs/AlarmBell][link]].
;; Disable bell completely.
(setq ring-bell-function 'ignore)

;; Hide welcome buffer on startup.
(setq inhibit-startup-screen t)

;; Hide Buffer list.
(setq inhibit-startup-buffer-menu t)

;; Make emacs window use full space
(setq frame-resize-pixelwise t)

;; Whitespace.
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd" c-basic-offset 2)
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-default-style "bsd" c-basic-offset 4)))

;; Remove GUI stuff
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 0)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Tooltip delay
(setq tooltip-delay 99999)

;; end defaults.el
