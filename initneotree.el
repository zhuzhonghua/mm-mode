(require 'neotree)

(setq neo-smart-open t)

(defvar tab-bar-mode-state-on-neotree nil)

(defun neotree-tab-toggle ()
	(interactive)
	(let ((old-tab-bar-mode tab-bar-mode))

		(when tab-bar-mode
      (tab-bar-new-tab -1)
			(tab-bar-mode -1))

		(neotree-toggle)

		(when (and (not (neo-global--window-exists-p))
							 tab-bar-mode-state-on-neotree)
			(tab-bar-mode 1))

		(setq tab-bar-mode-state-on-neotree old-tab-bar-mode)))


(global-set-key [f12] 'neotree-tab-toggle)
(global-set-key [f11] 'tab-bar-mode)

(global-set-key (kbd "<C-left>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "<C-right>") 'tab-bar-switch-to-next-tab)

(provide 'initneotree)
