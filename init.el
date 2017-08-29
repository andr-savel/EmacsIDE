;;;;;;;;;;;;;;;;;;;;;;;;;; Package installer
; to list packagess print: M-x list-packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; check and install packages
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

;;;;;;;;;;;;;;;;;;;;;;;;;; cua-mode
(cua-mode t)
(setq cua-keep-region-after-copy t)
(put 'dired-previous-line 'CUA 'move)
(put 'dired-next-line 'CUA 'move)

;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-enable-undo-in-region nil)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-M-z") 'redo)
(global-set-key (kbd "C-y") 'redo)

;;;;;;;;;;;;;;;;;;;;;;;;;; search with Custom Highlight (sch)
; case sensitivity
(setq sch-global-case-insensitivity t)
(defvar-local sch-local-case-insensitivity sch-global-case-insensitivity)
(defadvice re-search-forward (before sch-advice-search-forward activate) (setq case-fold-search sch-local-case-insensitivity))
(defadvice re-search-backward (before sch-advice-search-backward activate) (setq case-fold-search sch-local-case-insensitivity))

(defun sch-change-case-sensitivity ()
    (interactive)
    (if sch-global-case-insensitivity
        (progn
            (setq sch-global-case-insensitivity nil)
            (message "case-sensitive search")) ; TODO: integrate search case sensitivity into emacs mode line
        (setq sch-global-case-insensitivity t)
        (message "case-INsensitive search")))

(global-set-key (kbd "M-f") 'sch-change-case-sensitivity)

; search core
(setq sch-global-regexp "")
(defvar-local sch-local-regexp sch-global-regexp)
(defvar-local sch-local-search-status nil)

(defun sch-deselect-search-result ()
    (if sch-local-search-status
        (progn
            (setq mark-active nil)
            (setq sch-local-search-status nil))))

(defun sch-x-direction-with-selection (search-direction match-side regexp)
    (sch-deselect-search-result)
    (setq sch-local-search-status (funcall search-direction regexp nil t))
    (if sch-local-search-status
        (set-mark (funcall match-side 0)))
    sch-local-search-status)

(defun sch-x-direction-core (search-direction1 search-direction2 goto-side side-name)
    (when (and (not (funcall search-direction1 sch-local-regexp)) (funcall search-direction2 sch-local-regexp nil t))
        (funcall goto-side)
        (funcall search-direction1 sch-local-regexp)
        (message "Searching from the %s" side-name)))

(defun sch-x-direction (direction1 direction2 goto-side side-name)
    (when (or (not (string-equal sch-local-regexp sch-global-regexp)) (not (equal sch-local-case-insensitivity sch-global-case-insensitivity)))
        (if (not (string-equal sch-local-regexp ""))
            (unhighlight-regexp sch-local-regexp))
        (setq sch-local-regexp sch-global-regexp)
        (setq sch-local-case-insensitivity sch-global-case-insensitivity)
        (highlight-regexp sch-local-regexp))
    (sch-x-direction-core direction1 direction2 goto-side side-name))

; manipulations with search result's selection
(add-hook 'pre-command-hook 'sch-deselect-search-result-before)
(add-hook 'pre-command-hook 'sch-deselect-search-result-after)
(defun sch-deselect-search-result-before ()
    (if (and sch-local-search-status (and (use-region-p) (eq (get this-command 'CUA) 'move)))
        (sch-deselect-search-result)))

(defun sch-deselect-search-result-after ()
    (if (and sch-local-search-status (not (use-region-p)))
        (sch-deselect-search-result)))

; search in direction
(defun sch-forward-search-with-selection (regexp)
    (sch-x-direction-with-selection 're-search-forward 'match-beginning regexp))

(defun sch-backward-search-with-selection (regexp)
    (sch-x-direction-with-selection 're-search-backward 'match-end regexp))

(defun sch-forward ()
    (interactive)
    (sch-x-direction 'sch-forward-search-with-selection 're-search-backward 'beginning-of-buffer "beginning"))

(defun sch-backward ()
    (interactive)
    (sch-x-direction 'sch-backward-search-with-selection 're-search-forward 'end-of-buffer "end"))

; initial search
(defun sch-search ()
    (interactive)
    (setq sch-global-regexp (read-regexp "Search"))
    (sch-forward)
)

; key bindings
(global-set-key (kbd "C-f") 'sch-search)
(global-set-key (kbd "<f3>") 'sch-forward)
(global-set-key (kbd "M-<f3>") 'sch-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;; set comfortable scrolling mode
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;; remove useless windows on start
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;; show pair for parentheses
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;;;;;;;;;;;;;;;;;;;;;;;;;; wrap lines
(setq word-wrap t)
(global-visual-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;; common key bindings
;; C-h
;;     b) all key bindings
;;     k) description for appropriate key
;;     f) description for function
;;     v) description for variable
;;     w) key bindings for specified command
;; C-x
;;     C-c) close emacs
;;     C-w) write changes to another file
;;     o) next window
(global-set-key (kbd "M-q") 'keyboard-escape-quit)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
;(global-hl-line-mode 1) ; highlight current line

(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;; file actions
(defun save-all-buffers-without-questions ()
    (interactive)
    (save-some-buffers t))
(global-set-key (kbd "C-s") 'save-all-buffers-without-questions)

;;;;;;;;;;;;;;;;;;;;;;;;;; other
(setq column-number-mode t)

;; TODO:
;; 1) make minibuffer interactive (not only in icomplete-mode? but always (on file opening))
;; 2) tune makr and global mark ring
;; 3) correct identation for C++ buffer, python buffer; remove trailing white spaces on "save buffer" action
;; 4) integrate 'query-replace (conditional replace)
;; 5) rebind find-file-another-window (C-x 4 f)
;; 6) to intoroduce sch-search minor mode

;; to check the following search tools: swiper, helm swoop, swoop
