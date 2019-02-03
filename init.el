;; Bijan Sondossi
;; init.el
;; Emacs config file

;; Required package stuff ----------------------------------------------

(require 'package)

(defun package-add-archive (name url)
  "Add an archive with the name NAME and url URL."
  (add-to-list 'package-archives (cons name url) t))

;; Package archives
(if (< emacs-major-version 24)
    (let ((elpa-url "https://elpa.gnu.org/packages/"))
      (setq package-archives '("gnu" . elpa-url)))
  (let ((melpa-url "https://melpa.org/packages/")
        (stable-url "https://stable.melpa.org/packages/")
        (org-url "https://orgmode.org/elpa/"))
    (package-add-archive "melpa" melpa-url)
    (package-add-archive "melpa-stable" stable-url)
    (when (not (eq system-type 'windows-nt))
      (package-add-archive "org" org-url))))

(package-initialize)

;; Use package ---------------------------------------------------------

;; Keywords: https://jwiegley.github.io/use-package/keywords/

(unless (require 'use-package nil t)
  (if (eq system-type 'darwin)
      (let ((git-command "git clone")
            (package-url "https://github.com/jwiegley/use-package")
            (package-dir "~/.emacs.d/packages/use-package/"))
        (progn
          (when (not (file-accessible-directory-p package-dir))
            (shell-command (concat git-command " "
                                   package-url " "
                                   package-dir))
            (package-refresh-contents))
          (add-to-list 'load-path package-dir)
          (require 'use-package)))
    (package-refresh-contents)
    (package-install 'use-package)))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Configuration files -------------------------------------------------

(defun bijans/emacs-d-file (filename)
  "Appends a filename to the user-emacs-directory"
  (concat user-emacs-directory filename))

(defvar bijans/config-dir
  (bijans/emacs-d-file "config")
  "Custom configurations directory.")

(defun bijans/load-config (filename)
  "Loads a config file from bijans/config-dir"
  (load (concat bijans/config-dir "/" filename)))

(use-package benchmark-init
  :disabled
  :demand
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (benchmark-init/deactivate)
              (benchmark-init/show-durations-tabulated))))

(bijans/load-config "defaults")
(bijans/load-config "faces")
(bijans/load-config "functions")
(bijans/load-config "packages")
(bijans/load-config "late")
