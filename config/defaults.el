;; Bijan Sondossi
;; defaults.el
;; Default variables and settings.

;; Constants -----------------------------------------------------------

(defconst bijans/backup-dir
  (concat user-emacs-directory "/backups")
  "Emacs backup directory.")

(defconst bijans/colors-bg "#222430" "Background color.")
(defconst bijans/colors-hi "#44485a" "Highlight color.")

(defconst dracula/bg "#282a36" "Dracula background color.")
(defconst dracula/cl "#44475a" "Dracula current line color.")
(defconst dracula/selection "#44475a" "Dracula selection color.")
(defconst dracula/fg "#f8f8f2" "Dracula foreground color.")
(defconst dracula/comment "#6272a4" "Dracula comment color.")
(defconst dracula/cyan "#8be9fd" "Dracula cyan.")
(defconst dracula/green "#50fa8b" "Dracula green.")
(defconst dracula/orange "#ffb86c" "Dracula orange.")
(defconst dracula/pink "#ff79c6" "Dracula pink.")
(defconst dracula/purple "#bd93f9" "Dracula purple.")
(defconst dracula/red "#ff5555" "Dracula red.")
(defconst dracula/yellow "#f1fa8c" "Dracula yellow.")

;; Variables -----------------------------------------------------------

(defvar bijans/ctags-path "/usr/bin/ctags" "Path to ctags executable.")

(defvar bijans/buffer-map (make-sparse-keymap) "Buffer shortcuts.")
(defvar bijans/code-map   (make-sparse-keymap) "Code shortcuts.")
(defvar bijans/file-map   (make-sparse-keymap) "File shortcuts.")
(defvar bijans/help-map   (make-sparse-keymap) "Help shortucts.")
(defvar bijans/toggle-map (make-sparse-keymap) "Toggle shortcuts.")
(defvar bijans/window-map (make-sparse-keymap) "Window shortcuts.")

;; Functions -----------------------------------------------------------

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

(defun bijans/make-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -f TAGS -R %s" bijans/ctags-path
           (directory-file-name dir-name))))

(defun bijans/read-lines (filePath)
  "Return a list of lines of a file at filePath. Source: \
   http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;; Default bindings ----------------------------------------------------

(define-key bijans/buffer-map "k" 'kill-buffer)
(define-key bijans/buffer-map "l" 'list-buffers)
(define-key bijans/buffer-map "n" 'next-buffer)
(define-key bijans/buffer-map "p" 'previous-buffer)
(define-key bijans/buffer-map "s" 'switch-to-buffer)
(define-key bijans/code-map   "/" 'comment-or-uncomment-region)
(define-key bijans/file-map   "s" 'save-buffer)
(define-key bijans/file-map   "w" 'write-file)
(define-key bijans/help-map   "d" help-map)
(define-key bijans/window-map "0" 'delete-window)
(define-key bijans/window-map "1" 'delete-other-windows)
(define-key bijans/window-map "2" 'split-window-vertically)
(define-key bijans/window-map "3" 'split-window-horizontally)
(define-key bijans/window-map "o" 'other-window)
(define-key bijans/window-map "j" 'windmove-down)
(define-key bijans/window-map "k" 'windmove-up)
(define-key bijans/window-map "h" 'windmove-left)
(define-key bijans/window-map "l" 'windmove-right)
(define-key bijans/toggle-map "l" 'linum-mode)
(define-key bijans/toggle-map "L" 'hl-line-mode)

;; Settings ------------------------------------------------------------

;; Organize messy ~backup files into one folder.
(setq backup-directory-alist `((".*" . ,bijans/backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,bijans/backup-dir t)))
(when (not (file-accessible-directory-p bijans/backup-dir))
  (make-directory bijans/backup-dir))

;; Emacs customs save location.
(setq custom-file "~/.emacs.d/customs.el")
(when (file-exists-p custom-file) (load custom-file))

;; Silently add newline to files.
(setq require-final-newlne t)

;; Alarm bell, https://www.emacswiki.org/emacs/AlarmBell.
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

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Faces ---------------------------------------------------------------

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "Courier New"
                      :height 90
                      :weight 'normal)

  (cond ((eq system-type 'darwin)
         (set-face-attribute 'default nil :height 120)
         (let ((default-family "Andale Mono")
               (iosevka-font "Iosevka Term")
               (iosevka-weight 'light))
           (if (not (null (x-list-fonts iosevka-font)))
               (set-face-attribute 'default nil
                                   :font iosevka-font
                                   :weight iosevka-weight)
             (set-face-attribute 'default nil
                                 :family default-family))))

        ((eq system-type 'gnu/linux)
         (let ((os-info "/etc/os-release")
               (arch-os "arch")
               (arch-family "Adobe Courier")
               (ubuntu-os "ubuntu")
               (ubuntu-family "Monospace")
               (ubuntu-height 100))
           (dolist (line (bijans/read-lines os-info))
             (cond ((string-match-p arch-os line)
                    (set-face-attribute 'default nil
                                        :family arch-family))
                   ((string-match-p ubuntu-os line)
                    (set-face-attribute 'default nil
                                        :family ubuntu-family
                                        :height ubuntu-height))))))))
