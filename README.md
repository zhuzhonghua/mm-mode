# mm-mode
a minor mode inspired by meow

using key bindings below plus the commands rectangle-mark-mode (C-x SPC) and string-rectangle (C-x r t) can satisfy most of my daily needs

key bindings


```
(defun mm-define-keys ()
  (define-key mm-keymap (kbd "j") 'mm-next)
  (define-key mm-keymap (kbd "k") 'mm-prev)

  (define-key mm-keymap (kbd "h") 'backward-char)
  (define-key mm-keymap (kbd "l") 'forward-char)

  (define-key mm-keymap (kbd "0") 'mm-beginning-of-line)
  (define-key mm-keymap (kbd "9") 'delete-window)
  (define-key mm-keymap (kbd "C-8") 'counsel-imenu)
  (define-key mm-keymap (kbd "7") 'ivy-switch-buffer)
  (define-key mm-keymap (kbd "6") 'mm-find-in-directory)
  (define-key mm-keymap (kbd "3") 'split-window-right)
  (define-key mm-keymap (kbd "2") 'split-window-below)
  (define-key mm-keymap (kbd "1") 'delete-other-windows)

  (define-key mm-keymap (kbd "o") 'mm-open-below)
  (define-key mm-keymap (kbd "b") 'backward-word)
  (define-key mm-keymap (kbd "c") 'mm-ready-change)
  ;; cw -> delete next word and exit mm-mode
  (define-key mm-keymap (kbd "d") 'mm-ready-delete)
  ;; dw -> delete next word
  (define-key mm-keymap (kbd "e") 'mm-end-of-line)
  (define-key mm-keymap (kbd "f") 'scroll-up-command)
  (define-key mm-keymap (kbd "i") 'mm-insert)
  (define-key mm-keymap (kbd "O") 'mm-open-above)
  (define-key mm-keymap (kbd "p") 'mm-yank)
  (define-key mm-keymap (kbd "r") 'mm-replace)
  (define-key mm-keymap (kbd "u") 'mm-undo)
  (define-key mm-keymap (kbd "v") 'mm-mark)
  (define-key mm-keymap (kbd "V") 'mm-mark-line)
  (define-key mm-keymap (kbd "w") 'mm-forward-word)
  (define-key mm-keymap (kbd "x") 'mm-delete)
  (define-key mm-keymap (kbd "y") 'mm-save)
  (define-key mm-keymap (kbd "C-<tab>") 'mm-swith-to-other-buffer)
  (define-key mm-keymap (kbd "<C-left>") 'tab-bar-switch-to-prev-tab)
  (define-key mm-keymap (kbd "<C-right>") 'tab-bar-switch-to-next-tab)
  (define-key mm-keymap (kbd "(") 'backward-sexp)
  (define-key mm-keymap (kbd ")") 'forward-sexp)
  (define-key mm-keymap (kbd "{") 'backward-sexp)
  (define-key mm-keymap (kbd "}") 'forward-sexp)
  (define-key mm-keymap (kbd "DEL") 'mm-clear-before-space)
  (define-key mm-keymap (kbd "<") 'beginning-of-buffer)
  (define-key mm-keymap (kbd ">") 'end-of-buffer)
  (define-key mm-keymap (kbd "-") 'mm-switch-other-window)
  (define-key mm-keymap [escape] 'keyboard-quit)
  (define-key mm-keymap (kbd "<escape>") 'keyboard-quit)
  )
```
