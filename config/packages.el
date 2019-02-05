;; Bijan Sondossi
;; package-settings.el
;; Emacs package settings

;; Use-package heplers -------------------------------------------------

(use-package bind-key)

;; Builtins ------------------------------------------------------------

(use-package linum
  :pin manual
  :hook (prog-mode . linum-mode)
  :custom (linum-format "%4d "))

(use-package hideshow
  :pin manual
  :hook (prog-mode . hs-minor-mode))

(use-package prolog
  :pin manual
  :mode ("\\.pl\\'" . prolog-mode))

(use-package sticky-windows
  :pin manual
  :load-path (lambda () (bijans/emacs-d-file "packages/source")))

;; Key binding packages ------------------------------------------------

(use-package which-key
  :demand
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-operator-state-maps t)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package general
  :demand
  :config
  (general-create-definer bijans/leader
    :keymaps '(override)
    :states '(normal visual insert treemacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

;; Org mode ------------------------------------------------------------

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map bijans/org-map ("a" . org-agenda))
  :general (bijans/leader "o" '(:keymap bijans/org-map :which-key "org"))

  :custom
  (org-agenda-tags-column 0)
  (org-hide-leading-stars t)
  (org-image-actual-width nil)
  (org-startup-truncated nil)

  :init
  (defvar bijans/org-map (make-sparse-keymap) "Org shortcuts.")
  (defvar bijans/org-agenda-dir
    (bijans/emacs-d-file "agenda")
    "Org agenda directory")
  (when (file-exists-p bijans/org-agenda-dir)
    (setq org-agenda-files (list bijans/org-agenda-dir)))

  (when (display-graphic-p)
    (setq doc-view-continuous t)
    (setq org-pretty-entities t)
    (setq org-tags-column 0))

  :config
  (when (eq system-type 'darwin)
    (setq org-latex-create-formula-image-program 'imagemagick)
    (use-package cl
      :demand
      :ensure cl-lib)
    (use-package org-drill
      :demand
      :ensure org-plus-contrib
      :pin manual))

  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0))

  (add-hook 'org-mode-hook (lambda () (visual-line-mode))))

;; Evil magic ----------------------------------------------------------

(use-package evil
  :demand
  :after undo-tree

  :bind (:map evil-visual-state-map ("s" . evil-surround-region))
  :bind (:map evil-insert-state-map ("C-/" . evil-force-normal-state))
  :bind (:map bijans/buffer-map ("d" . evil-delete-buffer))

  :general
  (bijans/leader
   "b" '(:keymap bijans/buffer-map :which-key "buffer")
   "c" '(:keymap bijans/code-map :which-key "code")
   "f" '(:keymap bijans/file-map :which-key "files")
   "t" '(:keymap bijans/toggle-map :which-key "toggles")
   "?" '(:keymap help-map :which-key "documentation")
   "p" '(sticky-window-keep-window-visible
         :which-key "pin window"); (unpin w prefix arg)")
   "P" '((lambda ()
           (interactive)
           (set-window-dedicated-p (selected-window) nil))
         :which-key "unpin window")
   "0" '(sticky-window-delete-window
         :which-key "delete window")
   ; ")" '(delete-window :which-key "force delete window")
   "1" '(sticky-window-delete-other-windows
         :which-key "delete other windows")
   "!" '(delete-other-windows :which-key "force delete other windows")
   "2" 'split-window-vertically
   "3" 'split-window-horizontally
   "o" 'other-window
   "h" 'windmove-left
   "j" 'windmove-down
   "k" 'windmove-up
   "l" 'windmove-right
   "ESC" 'keyboard-quit)

  :custom
  (evil-want-C-u-scroll t)
  (evil-emacs-state-modes nil)
  (evil-default-state nil)
  (evil-motion-state-modes nil)
  (evil-toggle-key "")

  :config
  (if (eq emacs-major-version 24)
      (setq evil-normal-state-modes
            (append evil-emacs-state-modes
                    evil-insert-state-modes
                    evil-normal-state-modes
                    evil-motion-state-modes)))

  (add-to-list 'evil-emacs-state-modes 'nav-mode)

  (evil-mode 1))

(use-package evil-surround
  :demand
  :after evil
  :bind (:map bijans/toggle-map ("s" . evil-surround-mode))
  :config (global-evil-surround-mode 1))

(use-package evil-magit
  :after evil magit
  :custom (evil-magit-use-y-for-yank t)
  :config (evil-magit-init))

(use-package org-evil
  :after org evil
  :hook (org-mode . org-evil-mode))

;; Ivy -----------------------------------------------------------------

;; Ivy! Guides here -> http://tinyurl.com/go4bu33
;; http://tinyurl.com/lzl4uf2
;; http://tinyurl.com/hofdfv8

(use-package ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line))

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 5)
  (ivy-count-format "")
  (ivy-re-builders-alist '((t . ivy--regex-plus)))

  :config
  (ivy-mode 1))

(use-package counsel
  :bind (:map bijans/file-map ("f" . counsel-find-file))
  :general (bijans/leader "C-SPC" 'counsel-M-x "SPC" 'counsel-M-x))

(use-package counsel-projectile
  :bind (:map bijans/file-map ("/" . counsel-projectile-find-file))
  :config (counsel-projectile-mode))

;; UI ------------------------------------------------------------------

(use-package dracula-theme
  :demand
  :when (display-graphic-p))

(use-package centered-window
  :bind (:map bijans/toggle-map ("c" . 'centered-window-mode))
  :init (setq cwm-centered-window-width 77))

(use-package column-enforce-mode
  :bind (:map bijans/toggle-map ("m" . column-enforce-mode))
  :hook prog-mode
  :custom (column-enforce-column 72))

(use-package hlinum
  :demand
  :custom (linum-highlight-in-all-buffersp t)
  :config
  (hlinum-activate)
  (set-face-attribute 'linum-highlight-face nil
                      :background dracula/fg
                      :foreground dracula/bg))

(use-package smooth-scrolling
  :config (smooth-scrolling-mode 1))

(use-package spaceline
  :demand
  :when (display-graphic-p)
  :custom (powerline-default-separator nil)
  :config
  (require 'spaceline-config)

  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)
  (spaceline-spacemacs-theme)

  (dolist (face '(mode-line
                  powerline-active2
                  mode-line-inactive
                  powerline-inactive1
                  powerline-inactive2))
    (set-face-attribute face nil
                        :background bijans/colors-bg
                        :foreground dracula/fg
                        :box nil))

  (dolist (face '(spaceline-highlight-face powerline-active1))
    (set-face-attribute face nil
                        :background bijans/colors-hi
                        :foreground dracula/fg)))

(use-package tabbar
  :bind (:map bijans/toggle-map ("t" . tabbar-mode))
  :custom
  (tabbar-buffer-home-button (cons (cons " + " nil) (cons " - " nil)))
  (tabbar-scroll-left-button (cons (cons " < " nil) (cons " - " nil)))
  (tabbar-scroll-right-button (cons (cons " > " nil) (cons " - " nil)))
  (tabbar-use-images nil)
  (tabbar-separator '(0.5))
  :custom-face
  (tabbar-default ((nil (:foreground nil :background "#282a36"))))
  (tabbar-button ((nil (:box nil :foreground "#f8f8f2"))))
  (tabbar-unselected ((nil (:box nil :foreground "#f8f8f2"))))
  (tabbar-modified ((nil (:box nil :foreground "#ff5555"))))
  (tabbar-selected-modified ((nil (:box nil :foreground "#ff5555"))))
  (tabbar-selected ((nil (:box nil :foreground nil :bold t)))))

;; Programming ---------------------------------------------------------

(use-package clang-format
  :bind (:map bijans/code-map ("=" . clang-format-buffer)))

(use-package compile
  :bind (:map bijans/code-map ("m" . compile))
  :bind (:map bijans/code-map ("r" . recompile))
  :bind (:map bijans/code-map ("s" . recompile-silent))
  :custom (compile-command "make")
  :config
  (defun recompile-silent ()
    "Re-compile without changing the window configuration.
       https://www.emacswiki.org/emacs/CompileCommand"
    (interactive)
    (save-window-excursion (recompile))))

(use-package flycheck
  :defer t ; not being deferred by bind keyword
  :bind (:map bijans/toggle-map ("f" . 'flycheck-mode)))

(use-package flycheck-clang-tidy
  :hook (flycheck-mode-hook . flycheck-clang-tidy-setup))

(use-package xcscope
  :when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  :bind
  (:map bijans/code-map ("f" . cscope-find-this-symbol))
  (:map bijans/code-map
        ("i" . (lambda ()
                 (interactive)
                 (cscope-index-files cscope-initial-directory))))
  :config (cscope-setup)
  :custom (cscope-index-recursively t)
  :custom-face
  (cscope-separator-face
   ((nil (:foreground nil :underline nil :overline nil))))
  :hook
  (c++-mode . (lambda ()
                (setq cscope-initial-directory
                      (projectile-project-root))
                (cscope-minor-mode))))

;; File manipulation ---------------------------------------------------

(use-package flx :disabled :demand)

(use-package flx-ido
  :disabled
  :after flx

  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)

  :config
  (ido-mode 1)
  (flx-ido-mode 1))

(use-package treemacs
  :bind (:map bijans/toggle-map ("n" . treemacs))
  :custom (treemacs-no-png-images t))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :bind (:map bijans/toggle-map ("N" . treemacs-projectile)))

(use-package projectile
  :config (projectile-mode))

;; Text manipulation ---------------------------------------------------

(use-package caps-lock
  :bind (:map bijans/toggle-map ("~" . caps-lock-mode)))

(use-package company
  :bind (:map bijans/toggle-map  ("a" . company-mode))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package smartparens
  :bind (:map bijans/toggle-map ("p" . smartparens-mode)))

(use-package subword
  :bind (:map bijans/toggle-map ("w" . subword-mode)))

(use-package undo-tree
  :demand
  :general (bijans/leader "u" 'undo-tree-visualize))

(use-package yasnippet
  :ensure yasnippet-snippets
  :hook (prog-mode . yas-minor-mode-on)
  :bind (:map bijans/toggle-map ("y" . 'yas-minor-mode))
  :config
  (let ((snippet-dir "~/.emacs.d/snippets"))
    (when (not (file-accessible-directory-p snippet-dir))
      (make-directory snippet-dir))
    (setq yas-snippet-dirs snippet-dir)))

;; Other major modes ---------------------------------------------------

(use-package bats-mode
  :mode ("\\.bats\\'" . bats-mode))

(use-package cmake-mode
  :mode (("\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package docker
  :disabled
  :ensure docker-tramp
  :ensure dockerfile-mode)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package scratch
  :commands scratch)

(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode) ("\\.mli\\'" . tuareg-mode))
  :interpreter "ocaml")

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; Other minor modes ---------------------------------------------------

(use-package magit
  :general (bijans/leader "g" 'magit-status)
  :custom (magit-completing-read-function 'ivy-completing-read))

(use-package auto-package-update
  :commands auto-package-update-maybe)
