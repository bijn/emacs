;; Bijan Sondossi
;; file: init.el
;; info: Emacs config file

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

;; Installation
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

;; Use package settings
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(use-package benchmark-init
  :disabled
  :demand
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Other config files --------------------------------------------------

(load "~/.emacs.d/settings/defaults")
(load "~/.emacs.d/settings/bindings")
(load "~/.emacs.d/settings/packages")
(load "~/.emacs.d/settings/functions")
;; should add this to hook
(when (display-graphic-p) (load "~/.emacs.d/settings/faces"))
(load "~/.emacs.d/settings/late")

;; Temporary settings --------------------------------------------------

(use-package markdown-mode :mode ("\\.md\\'" . markdown-mode))
(use-package cmake-mode :demand)
(use-package yaml-mode :mode ("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; end init.el
