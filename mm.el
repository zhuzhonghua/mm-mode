(require 'counsel)

(defun mm-swith-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun mm-beginning-of-line ()
  (interactive)
  (move-beginning-of-line nil)
  (while (or (eq ?  (char-after (point)))
						 (eq ?\t (char-after (point))))
		(forward-char))
  )

(defun mm-end-of-line ()
  (interactive)
  (move-end-of-line nil)
  (mm-check-change))

(defun mm-find-in-directory ()
	(interactive)
	(counsel-rg (thing-at-point 'symbol' 'no-properties)
              (file-name-directory (buffer-file-name))
              nil
              "Grep In Current Directory:"))

(defun mm-clear-before-space ()
  (interactive)
  (if (or (eq ?  (char-before (point)))
					(eq ?\t (char-before (point))))
      (while (or (eq ?  (char-before (point)))
					       (eq ?\t (char-before (point))))
        (backward-delete-char 1 nil))
		(backward-delete-char 1 nil)))

(defvar meow--kbd-exchange-point-and-mark "C-x C-x"
  "KBD macro for command `exchange-point-and-mark'.")

(region-beginning) (region-end)

(defun mm-prev ()
  (interactive)
  (when (and (region-active-p)
             (eq last-command 'mm-mark-line)
             (< (region-beginning) (region-end)))
    (mm-call-kbd "C-x C-x"))

  (previous-line)
  )

(defun mm-next ()
  (interactive)
  (next-line)
  )

(defun mm-open-below ()
  (interactive)
  (mm-insert)
  (goto-char (line-end-position))
  (mm-call-kbd "RET"))

(defun mm-open-above ()
  (interactive)
  (mm-insert)
  (goto-char (line-beginning-position))
  (save-excursion
    (newline)))

(defun mm-call-kbd (ki)
  (let ((ret (key-binding (read-kbd-macro ki))))
    (setq this-command ret)
    (call-interactively ret)))

(defun mm-replace ()
  (interactive)
  (delete-char 1)
  (mm-insert))

(defun mm-insert ()
  (interactive)
  (mm-mode -1)
  (setq cursor-type 'bar))

(defun mm-yank ()
  (interactive)
  (mm-call-kbd "C-y"))

(defun mm-undo ()
  (interactive)
  (mm-call-kbd "C-/"))

(defun mm-mark ()
  (interactive)
  (mm-call-kbd "C-@"))

(defun mm-mark-line ()
  (interactive)
  (goto-char (line-end-position))
  (save-excursion
    (set-mark (line-beginning-position))))

(defun mm-save ()
  (interactive)
  (if (region-active-p)
      (mm-call-kbd "M-w")
    (when (eq last-command 'mm-save)
      (save-mark-and-excursion
        (mm-mark-line)
        (mm-call-kbd "M-w"))))
  )

(defun mm-switch-other-window ()
	(interactive)
	(other-window 1))

(defun mm-delete ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1)))

(defun mm-cancel ()
  (interactive)
  (mm-call-kbd "C-g"))

(defvar mm-keymap (make-sparse-keymap))

;;;###autoload
(define-minor-mode mm-mode
  "mm minor mode"
  :init-value nil
  ;;:interactive nil
  ;;:global nil
  :lighter " MM"
  :keymap mm-keymap
  (if mm-mode
      (mm--enable)
    (mm--disable)))

(defun mm--enable ()
  (message "enable mm-mode %S" last-command)
  (when (and (eq last-command 'self-insert-command)
             (or (equal ?\) (char-before (point)))
                 (equal ?\} (char-before (point)))
                 (equal ?\" (char-before (point))))
             )
    (backward-char))
  (add-hook 'minibuffer-setup-hook #'mm--minibuffer-setup)
  )

(defun mm--disable ()
  (message "disable mm-mode")
  )

(defun mm-minibuffer-quit ()
  "Keyboard escape quit in minibuffer."
  (interactive)
  (if (minibufferp)
      (if (fboundp 'minibuffer-keyboard-quit)
          (call-interactively #'minibuffer-keyboard-quit)
        (call-interactively #'abort-recursive-edit))
    (call-interactively #'keyboard-quit)))

(defun mm--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'mm-minibuffer-quit)
  )

(defun mm-check-change ()
  (cond
   ((eq last-command 'mm-ready-change)
    (progn (mm-delete)
           (mm-insert)))
   ((eq last-command 'mm-ready-delete)
    (progn (mm-delete)))))

(defun mm-ready-delete ()
  (interactive)
  (cond
   ((region-active-p)
    (mm-delete))
   (t
    (set-mark-command nil)
    (when (eq last-command 'mm-ready-delete)
      (mm-mark-line)
      (mm-delete)))))

(defun mm-ready-change ()
  (interactive)
  (set-mark-command nil))

(defun mm-forward-word ()
  (interactive)
  (forward-word)
  (mm-check-change))

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
  (define-key mm-keymap (kbd "d") 'mm-ready-delete)
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

(mm-define-keys)

(global-set-key [escape] 'init-mm-mode)

(defun init-mm-mode ()
  (interactive)
  (setq cursor-type 'box)
  (mm-mode 1))

(provide 'mm)
