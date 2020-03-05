;; Bijan Sondossi
;; package-settings.el
;; Emacs package settings

;; Use-package heplers -------------------------------------------------

(use-package bind-key)

;; Builtins ------------------------------------------------------------

;; want to add projectile based configs. project local hooks?
;; - for buffer local hooks see `LOCAL' parameter in `add-hook'
(use-package json
  :pin manual
  :functions bijans/read-json-config
  :config
  (defun bijans/set-key (key value &rest args)
    (let* ((symbol (intern-soft key))
           (allocator (plist-get args :allocator))
           (hook (plist-get args :hook)))
      (if allocator
          (allocator symbol value)
        (if hook
            (let ((does-contain (member symbol (eval hook))))
              (when (string= "off" value)
                (when does-contain
                  (remove-hook hook `,symbol)))
              (when (string= "on" value)
                (when (not does-contain)
                  (add-hook hook `,symbol))))
          (progn
            (set symbol value))))))

  (defun bijans/set-hash (map hash &rest args)
    (apply 'bijans/set-key hash (gethash hash map) args))

  (defun bijans/read-json-config (&optional directory)
    (let* ((project-root (if directory
                             directory
                           user-emacs-directory))
           (json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (config-file (concat project-root "config.json"))
           (config-map
            (if (file-exists-p config-file)
                (json-read-file config-file)
              nil)))
      (when config-map
        (dolist (key (map-keys config-map))
          (let ((rules (gethash key bijans/json-rules)))
            (if rules
                (apply #'bijans/set-hash
                       (loop for arg in (append `(,config-map ,key) rules)
                             collect arg))
              (bijans/set-hash config-map key))))))))

(use-package linum
  :pin manual
  :hook (prog-mode . linum-mode)
  :bind (:map bijans/toggle-map ("l" . linum-mode))
  :bind (:map bijans/toggle-map ("L" . global-linum-mode))
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

(use-package smalltalk-mode
  :pin manual
  :load-path (lambda () (bijans/emacs-d-file "packages/source"))
  :mode ("\\.st\\'" . smalltalk-mode))

;; Key binding packages ------------------------------------------------

(use-package which-key
  :demand
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-operator-state-maps t)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (which-key-add-key-based-replacements
    "SPC g i" "cscope index project"
    "SPC g r" "reload config"))

(use-package hydra
  :config
  (defhydra bijans/hydra-scale-up (:hint "")
    ("+" text-scale-increase)
    ("-" bijans/hydra-scale-down/body :exit t)
    ("h" evil-window-increase-width)
    ("v" evil-window-increase-height)
    ("q" nil))
  (defhydra bijans/hydra-scale-down (:hint "")
    ("+" bijans/hydra-scale-up/body :exit t)
    ("-" text-scale-decrease)
    ("h" evil-window-decrease-width)
    ("v" evil-window-decrease-height)
    ("q" nil)))

(use-package general
  :demand
  :config
  (general-create-definer bijans/leader
    :keymaps '(override)
    :states '(normal visual insert treemacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (bijans/bind-general-hydra
   bijans/leader
   bijans/hydra-nav
   ("!" delete-other-windows "force delete other windows")
   ("0" delete-window "delete window")
   ("1" sticky-window-delete-other-windows "delete other windows")
   ("2" split-window-vertically "vertical split")
   ("3" split-window-horizontally "horizontal split")
   ("7" ace-swap-window "swap windows")
   ("8" (lambda ()
          (interactive)
          (when (bound-and-true-p centered-window-mode)
            (centered-window-mode 0))
          (winner-undo))
    "undo window change and disable centered window")
   ("9" winner-redo "redo window change")
   ("B" list-buffers "list buffers")
   ("b" switch-to-buffer "switch buffer")
   ("d" kill-buffer "close buffer")
   ("h" windmove-left "window move left")
   ("j" windmove-down "window move down")
   ("k" windmove-up "window move up")
   ("l" windmove-right "window move right")
   ("n" next-buffer "next buffer")
   ("o" other-window "move to next window")
   ("x" (lambda ()
          (interactive)
          (if (window-dedicated-p)
              (set-window-dedicated-p (selected-window) nil)
            (set-window-dedicated-p (selected-window) t))
          (force-mode-line-update)) "toggle pin window")
   ("p" previous-buffer "previous buffer"))

  (bijans/leader
    "?" '(:keymap help-map :which-key "documentation")
    "+" '(bijans/hydra-scale-up/body :which-key "scale up mode")
    "-" '(bijans/hydra-scale-down/body :which-key "scale down mode")
    "`" '(eshell :which-key "shell")
    "W" '(write-file :which-key "save as")
    "c" '(bijans/comment-or-uncomment :which-key "toggle comment")
    "g" '(:keymap bijans/extras-map :which-key "additional shortcuts")
    "m" '(:keymap bijans/code-map :which-key "additional shortcuts")
    "r" '(bijans/ssh :which-key "ssh")
    "t" '(:keymap bijans/toggle-map :which-key "toggles")
    "w" '(save-buffer :which-key "save")
    "ESC" '(keyboard-quit :which-key "cancel")
    "C-g" '(keyboard-quit "cancel"))

  (define-key bijans/extras-map "w" 'bijans/hydra-nav/body))

(use-package multi-term
  :when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  :general
  (bijans/leader
    "`" '((lambda (directory)
            (interactive
             (list (read-directory-name "Directory: "
                                        default-directory)))
            (let ((tmp-buffer (get-buffer-create "*tmp*")))
              (switch-to-buffer-other-window tmp-buffer)
              (cd directory)
              (multi-term)))
          :which-key "terminal"))
  :custom
  (multi-term-program (when (eq system-type 'darwin) "/bin/bash")))

;; Org mode ------------------------------------------------------------

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general (bijans/leader "a" 'org-agenda)

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
    (use-package cl :ensure cl-lib)
    (use-package org-drill :ensure org-plus-contrib :pin manual))

  (use-package pamparam)

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

  :general (bijans/leader "D" 'evil-delete-buffer)

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

  (evil-define-operator evil-comment (beg end type register)
    (interactive "<R><x>")
    (comment-or-uncomment-region beg end))
  (bijans/leader "c" 'evil-comment)

  (add-hook
   'term-mode-hook
   (lambda ()
     (evil-normal-state)
     (add-hook 'evil-insert-state-entry-hook 'term-char-mode nil t)
     (add-hook 'evil-insert-state-exit-hook 'term-line-mode nil t)))

  (evil-define-key 'normal term-mode-map
    (kbd "<return>") 'term-send-input)

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
  :demand
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

(use-package swiper
  :general (bijans/leader "/" 'counsel-git-grep "C-/" 'swiper))

(use-package counsel
  :demand
  :general
  (bijans/leader
    "C-SPC" 'counsel-M-x
    "SPC" 'counsel-M-x
    "F" 'counsel-find-file))

(use-package counsel-projectile
  :after counsel
  :general (bijans/leader "f" 'counsel-projectile-find-file)
  :config (counsel-projectile-mode))

;; UI ------------------------------------------------------------------

(use-package dracula-theme
  :demand
  :when (display-graphic-p))

(use-package centered-window
  :bind (:map bijans/toggle-map ("c" . 'centered-window-mode))
  :init (setq cwm-centered-window-width 77))

(use-package column-enforce-mode
  :bind
  (:map bijans/toggle-map
        ("m" . column-enforce-mode)
        ("M" . (lambda ()
                 (interactive)
                 (if (not (bound-and-true-p column-enforce-mode))
                     (progn
                       (when (eq nil (member 'column-enforce-mode
                                             prog-mode-hook))
                         (add-hook 'prog-mode-hook
                                   'column-enforce-mode))
                       (column-enforce-mode 1))
                   (global-column-enforce-mode 0)
                   (remove-hook 'prog-mode-hook
                                'column-enforce-mode)))))
  :hook prog-mode
  :custom (column-enforce-column 72))

(use-package hlinum
  :demand
  :bind (:map bijans/toggle-map ("h" . hl-line-mode))
  :custom (linum-highlight-in-all-buffersp t)
  :config
  (hlinum-activate)
  (set-face-attribute 'linum-highlight-face nil
                      :background dracula/fg
                      :foreground dracula/bg))

(use-package smooth-scrolling
  :config (smooth-scrolling-mode 1))

(use-package spaceline
  :disabled
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
  :bind (:map bijans/toggle-map ("b" . tabbar-mode))
  :custom
  (tabbar-buffer-home-button (cons (cons " - " nil) (cons " + " nil)))
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
  :after evil
  :config
  (evil-define-operator evil-clang-format-region (beg end type register)
    (interactive "<R><x>")
    (clang-format-region beg end))
  (bijans/leader "=" 'clang-format-buffer)
  (evil-define-key
    'normal c++-mode-map "=" 'evil-clang-format-region))

(use-package compile
  :after projectile
  :defines compile-directory bijans/compile-registers

  :functions
  bijans/compile
  bijans/quick-compile
  bijans/quick-compile-record
  bijans/recompile
  bijans/recompile-silent

  :bind
  (:map bijans/code-map ("@" . bijans/quick-compile))
  (:map bijans/code-map ("q" . bijans/quick-compile-record))
  (:map bijans/code-map ("m" . bijans/compile))
  (:map bijans/code-map ("r" . bijans/recompile))
  (:map bijans/code-map ("s" . bijans/recompile-silent))

  :general
  (bijans/leader "@" 'bijans/quick-compile)
  (bijans/leader "q" 'bijans/quick-compile-record)

  :custom
  (compile-command "cmake --build .")
  (compilation-scroll-output 'first-error)

  :config
  (defcustom compile-directory "" "Compilation directory.")

  (defcustom bijans/quick-compile-map
    (make-hash-table) "Compilation shortcuts.")

  (add-hook 'compilation-start-hook
            (lambda (process)
              (setq bijans/build-status "building")))
  (add-to-list 'compilation-finish-functions
               (lambda (process status)
                 (setq bijans/build-status
                       (let ((chomped-status (string-trim status)))
                         (message "%s" chomped-status)
                         (if (string= chomped-status "finished")
                             "success"
                           "failure")))))

  (defun bijans/set-compile-parameters (&optional command directory)
    (interactive)
    (when (or (not compile-directory)
              (= (length compile-directory) 0))
        (setq compile-directory ".")
        (when (projectile-project-root)
          (let* ((root-dir (projectile-project-root))
                 (cmake-file (concat root-dir "CMakeLists.txt"))
                 (build-dir (concat root-dir "build")))
            (when (file-exists-p cmake-file)
              (setq compile-directory build-dir)))))
    (list (read-string "Command: " compile-command)
          (read-directory-name "Directory: "
                               compile-directory)))

  (defun bijans/quick-compile (key)
    (interactive (list (read-event)))
    (let ((command-pair (gethash key bijans/quick-compile-map)))
      (if command-pair
          (bijans/compile (car command-pair) (cadr command-pair))
        (bijans/quick-compile-record key))))

  (defun bijans/quick-compile-record (key)
    (interactive (list (read-event)))
    (let ((param-list (bijans/set-compile-parameters)))
      (puthash key param-list bijans/quick-compile-map)
      (bijans/compile (car param-list) (cadr param-list))))

  (defun bijans/compile (&optional command directory)
    "Runs COMMAND in DIRECTORY. If directory is empty, will attempt
to find and create a CMake build directory or will use the current
directory"
    (interactive (bijans/set-compile-parameters))
    (setq compile-command command)
    (setq compile-directory directory)
    (let ((curdir default-directory))
      (when (not (file-accessible-directory-p compile-directory))
        (when (yes-or-no-p (format "%s does not exist. Create? "
                                   compile-directory))
          (make-directory compile-directory t)))
      (cd compile-directory)
      (compile compile-command)
      (cd curdir)))

  (defun bijans/recompile ()
    "Calls bijans/compile without prompting."
    (interactive)
    (bijans/compile compile-command compile-directory))

  (defun bijans/recompile-silent ()
    "Re-compile without changing the window configuration.
https://www.emacswiki.org/emacs/CompileCommand"
    (interactive)
    (save-window-excursion (bijans/recompile))))

(use-package flycheck
  :defer t ; not being deferred by bind keyword
  :bind (:map bijans/toggle-map ("f" . 'flycheck-mode)))

(use-package flycheck-clang-tidy
  :hook (flycheck-mode-hook . flycheck-clang-tidy-setup))

;; look into more at http://cedet.sourceforge.net/
(use-package semantic
  :disabled
  :hook (prog-mode . semantic-mode)
  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-ede-mode 1))

(use-package auto-complete
  :hook (prog-mode . auto-complete-mode)
  :config
  (ac-config-default)
  (add-hook 'c-mode-common-hook
            '(lambda () (add-to-list 'ac-sources 'ac-source-semantic))))

(use-package xcscope
  :when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  :defines cscope-initial-directory
  :functions cscope-index-files
  :bind
  (:map bijans/extras-map
        ("i" . (lambda ()
                 (interactive)
                 (cscope-index-files (projectile-project-root))))
        ("/" . cscope-find-this-symbol))
  :config (cscope-setup)
  :custom (cscope-index-recursively t)
  :custom-face
  (cscope-separator-face
   ((nil (:foreground nil :underline nil :overline nil))))
  :init (add-hook 'c-mode-common-hook 'cscope-minor-mode))

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
  :after ivy
  :bind (:map bijans/toggle-map ("t" . treemacs))
  :custom (treemacs-no-png-images t))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :bind (:map bijans/toggle-map ("N" . treemacs-projectile)))

(use-package projectile
  :demand
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

(use-package iedit
  :general (bijans/leader "i" 'iedit-mode))

(use-package evil-iedit-state
  :after evil iedit
  :hook iedit-mode)

;; Other major modes ---------------------------------------------------

(use-package bats-mode
  :mode ("\\.bats\\'" . bats-mode))

(use-package cmake-mode
  :mode (("CMakeLists.txt" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package docker
  :mode (("Dockerfile" . dockerfile-mode)
         ("\\.docker\\'" . dockerfile-mode))
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

(use-package glsl-mode
  :mode (("\\.fs\\'" . glsl-mode) ("\\.vs\\'" . glsl-mode)))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))

;; Other minor modes ---------------------------------------------------

(use-package magit
  :bind (:map bijans/extras-map ("g" . magit-status))
  :custom (magit-completing-read-function 'ivy-completing-read))

(use-package auto-package-update
  :commands auto-package-update-maybe)

(use-package ace-window
  :custom (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :general (bijans/leader "o" 'ace-window))

(use-package vagrant-tramp :functions 'vagrant-tramp-term)
