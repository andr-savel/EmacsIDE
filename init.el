    ; Package installer
    ;     to list packagess print: M-x list-packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
    ;     check and install packages
(defvar required-packages '(undo-tree
                            ))

(defvar is-pack-list-refreshed 0)
(dolist (package required-packages)
    (unless (package-installed-p package)
        (if (eq is-pack-list-refreshed 0)
            (progn
                (package-refresh-contents)
                (setq is-pack-list-refreshed 1)))
        (package-install package)))

    ; cua-mode
(cua-mode t)

    ; switch-on undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-M-z") 'redo)
(global-set-key (kbd "C-y") 'redo)

    ; start/end of file
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)

    ; mark whole buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)

    ; Search with Custom Highlight (sch)
;TODO: set pointer to start of word when search-forward
;TODO: select found text 
; definitions
(setq sch-global-regexp "")
(defvar-local sch-local-regexp "")

; initial search
(defun sch-search ()
    (interactive)
    (setq sch-global-regexp (read-regexp "Search"))
    (sch-forward)
)

; search forward
(defun sch-forward-core ()
    (interactive)
    (when (and (not (re-search-forward sch-local-regexp nil t)) (re-search-backward sch-local-regexp nil t))
        (beginning-of-buffer)
        (re-search-forward sch-local-regexp nil nil)
        (message "Searching from beginning"))
)
(defun sch-forward ()
    (interactive)
    (when (not (string-equal sch-local-regexp sch-global-regexp))
        (message "llll")
        (if (not (string-equal sch-local-regexp ""))
            (unhighlight-regexp sch-local-regexp))
        (setq sch-local-regexp sch-global-regexp)
        (highlight-regexp sch-local-regexp))
    (sch-forward-core)
)

; search backward
(defun sch-backward-core ()
    (interactive)
    (when (and (not (re-search-backward sch-local-regexp nil t)) (re-search-forward sch-local-regexp nil t))
        (end-of-buffer)
        (re-search-backward sch-local-regexp nil nil)
        (message "Searching from end"))
)
(defun sch-backward ()
    (interactive)
    (when (not (string-equal sch-local-regexp sch-global-regexp))
        (message "llll")
        (if (not (string-equal sch-local-regexp ""))
            (unhighlight-regexp sch-local-regexp))
        (setq sch-local-regexp sch-global-regexp)
        (highlight-regexp sch-local-regexp))
    (sch-backward-core)
)

; key bindings
(global-set-key (kbd "C-f") 'sch-search)
(global-set-key (kbd "<f3>") 'sch-forward)
(global-set-key (kbd "M-<f3>") 'sch-backward)

    ; set comfortable scrolling mode
(setq scroll-step 1)
(setq scroll-conservatively 10000)

    ; remove useless windows on start
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

    ; show pair for (){}[] TODO: {} does not work
(show-paren-mode t)

    ; wrap lines
(setq word-wrap t)
(global-visual-line-mode t)

    ; highlight current line
;(global-hl-line-mode 1)


    ;; keys
;; C-h b) all key bindings
;; C-x o) next window

;; to check the following search tools: swiper, helm swoop, swoop
