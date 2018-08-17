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

;; Org mode ------------------------------------------------------------

(use-package which-key
  :demand
  :init
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 1)
  (setq which-key-max-description-length 30)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps nil)
  :custom-face
  (which-key-command-description-face ((nil (:foreground "#ffffff"))))
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; A better Evil leader.
(use-package general
  :demand
  :custom
  (general-default-prefix "SPC")
  (general-default-non-normal-prefix "C-SPC")
  (general-default-global-prefix "C-SPC")
  (general-default-keymaps '(evil-normal-state-map
                             evil-insert-state-map
                             evil-visual-state-map)))

;; Org mode stuff here so we can load settings.org
(use-package org
  :ensure cl-lib
  ;; :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)

  :init
  (defvar bijans/org-map (make-sparse-keymap) "Org shortcuts.")
  (when (display-graphic-p)
    (setq doc-view-continuous t)
    (setq org-pretty-entities t)
    (setq org-tags-column 0))

  :bind (:map bijans/org-map ("a" . org-agenda))
  :general ("o" '(:keymap bijans/org-map :which-key "org"))

  :custom
  (org-startup-truncated nil)
  (org-hide-leading-stars t)
  (org-image-actual-width nil)

  :config
  (when (eq system-type 'darwin)
    (setq org-latex-create-formula-image-program 'imagemagick))

  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0))

  ;; (use-package cl :demand :ensure t :pin manual)
  ;; (require 'org-drill)
  ;; (setq org-drill-cram-hours 0)

  (add-hook 'org-mode-hook (lambda () (visual-line-mode)))
  (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))
  (add-to-list 'org-emphasis-alist '("!" (:overline t))))

;; Some default minor modes.
(use-package subword
  :bind (:map bijans/toggle-map ("w" . subword-mode)))

(use-package undo-tree
  :demand
  :bind (:map bijans/toggle-map ("u" . undo-tree-visualize)))

(use-package compile
  :bind (:map bijans/code-map ("m" . compile)
         :map bijans/code-map ("r" . recompile)
         :map bijans/code-map ("s" . recompile-silent))
  :custom (compile-command "make")
  :config
  (defun recompile-silent ()
    "Re-compile without changing the window configuration.
       https://www.emacswiki.org/emacs/CompileCommand"
    (interactive)
    (save-window-excursion (recompile))))

(use-package caps-lock
  :bind (:map bijans/toggle-map ("~" . caps-lock-mode)))

(use-package scratch :commands scratch)
(use-package dracula-theme :demand :if (display-graphic-p))

;; Source tree minor mode.
(use-package neotree
  :bind (:map bijans/toggle-map ("n" . neotree-toggle))
  :config (neotree-mode) (neotree-hide))

;; Mark column 73
(use-package column-enforce-mode
  :bind (:map bijans/toggle-map ("m" . column-enforce-mode))
  :hook prog-mode
  :custom (column-enforce-column 72))

;; Evil magic!
(use-package evil
  :demand
  :bind (:map evil-visual-state-map ("s" . evil-surround-region)
         :map evil-insert-state-map ("C-/" . evil-force-normal-state)
         :map bijans/buffer-map ("d" . evil-delete-buffer)
         :map evil-normal-state-map ("g h" . left-char)
                                    ("g l" . right-char))
  :general ("b" '(:keymap bijans/buffer-map :which-key "buffer")
            "c" '(:keymap bijans/code-map :which-key "code")
            "f" '(:keymap bijans/file-map :which-key "files")
            "h" '(:keymap bijans/help-map :which-key "help")
            "t" '(:keymap bijans/toggle-map :which-key "toggles")
            "w" '(:keymap bijans/window-map :which-key "window")
            "ESC" 'keyboard-quit)

  :custom
  (evil-want-C-u-scroll t)
  (evil-emacs-state-modes nil)
  (evil-default-state nil)
  (evil-motion-state-modes nil)
  (evil-toggle-key "")
  (evil-want-integration nil)

  :config
  (if (eq emacs-major-version 24)
      (setq evil-normal-state-modes
            (append evil-emacs-state-modes
                    evil-insert-state-modes
                    evil-normal-state-modes
                    evil-motion-state-modes))
    (use-package evil-collection
      :demand
      :after evil
      :config (evil-collection-init)))
  (use-package evil-surround
    :demand
    :bind (:map bijans/toggle-map ("s" . evil-surround-mode))
    :config (global-evil-surround-mode 1))
  (use-package avy :demand)
  (use-package evil-easymotion
    :demand
    :after avy
    :general
    ("j" '(:keymap evilem-map :which-key "easymotion")
     "k" '(:keymap evilem-map :which-key "easymotion")))
  (use-package org-evil :hook (org-mode . org-evil-mode))
  (evil-mode 1))

;; Git minor modes.
(use-package magit
  :general ("g" 'magit-status)
  :custom (magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init)
  :custom (evil-magit-use-y-for-yank t))

;; Ivy! Guides here -> http://tinyurl.com/go4bu33
;; http://tinyurl.com/lzl4uf2
;; http://tinyurl.com/hofdfv8

(use-package flx :demand)
(use-package flx-ido
  :after flx
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  :config
  (ido-mode 1)
  (flx-ido-mode 1))

(use-package projectile
  :demand
  :config (projectile-mode))

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-next-line)
              ("C-e" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-y" . ivy-previous-line))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 5)
  (ivy-count-format "");"(%d/%d) ")
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1))

(use-package swiper :disabled
  :custom (ivy-use-selectable-prompt 1)
  :bind (:map evil-normal-state-map ("/" . swiper)))

(use-package counsel
  :bind (:map bijans/file-map ("f" . counsel-find-file))
  :general ("C-SPC" 'counsel-M-x "SPC" 'counsel-M-x))

(use-package counsel-projectile
  :demand
  :bind (:map bijans/file-map ("/" . counsel-projectile-find-file))
  :config (counsel-projectile-mode))

(use-package company
  :demand
  :bind (:map bijans/toggle-map  ("a" . company-mode)
         :map company-active-map ("C-n" . company-select-next)
                                 ("C-p" . company-select-previous))
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Settings and package configurations for my mode line.
;; `spaceline-major-mode-p` toggles segments (list on GH page).
(use-package spaceline
  :demand
  :if (display-graphic-p)
  :custom (powerline-default-separator nil)
  :config
  (require 'spaceline-config)
  (let ((bg "#282a36") (lg "#44485a") (fg "#f8f8f2"))
    (dolist (face '(mode-line
                    powerline-active2
                    mode-line-inactive
                    powerline-inactive1
                    powerline-inactive2))
      (set-face-attribute face nil :background bg :foreground fg))
    (dolist (face '(spaceline-highlight-face powerline-active1))
      (set-face-attribute face nil :background lg :foreground fg)))
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)
  (spaceline-spacemacs-theme))

(use-package hlinum
  :demand
  :custom (linum-highlight-in-all-buffersp t)
  :custom-face (linum-highlight-face ((nil (:background "#f8f8f2"
                                            :foreground "#282a36"))))
  :config (hlinum-activate))

(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :interpreter "ocaml")

(use-package smartparens
  :bind (:map bijans/toggle-map ("p" . smartparens-mode)))

(use-package flycheck
  :bind (:map bijans/toggle-map ("f" . 'flycheck-mode)))

(use-package yasnippet
  :ensure yasnippet-snippets
  :hook (prog-mode . yas-minor-mode-on)
  :bind (:map bijans/toggle-map ("y" . 'yas-minor-mode))
  :config
  (let ((snippet-dir "~/.emacs.d/snippets"))
    (when (not (file-accessible-directory-p snippet-dir))
      (make-directory snippet-dir))
    (setq yas-snippet-dirs snippet-dir)))

(use-package centered-window
  :bind (:map bijans/toggle-map ("c" . 'centered-window-mode))
  :init (setq cwm-centered-window-width 77))

(use-package auto-package-update :commands auto-package-update-maybe)

(use-package smooth-scrolling :config (smooth-scrolling-mode 1))

;; end package-settings.el
