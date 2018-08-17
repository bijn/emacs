;; Bijan Sondossi
;; bindings.el
;; Keymaps and bindings.

;; Default keymaps.
(defvar bijans/buffer-map (make-sparse-keymap) "Buffer shortcuts.")
(defvar bijans/code-map   (make-sparse-keymap) "Code shortcuts.")
(defvar bijans/file-map   (make-sparse-keymap) "File shortcuts.")
(defvar bijans/help-map   (make-sparse-keymap) "Help shortucts.")
(defvar bijans/toggle-map (make-sparse-keymap) "Toggle shortcuts.")
(defvar bijans/window-map (make-sparse-keymap) "Window shortcuts.")

;; Default keymap binds.
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

;; end bindings.el
