;;;;;;;;;;;;;;;;;;;;;;;;;; Package installer
; to list packagess print: M-x list-packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; check and install packages
(defvar required-packages '(undo-tree
                            ivy
                            ivy-rich
                            ivy-rtags
                            company))

(defvar is-pack-list-refreshed 0)
(dolist (package required-packages)
    (unless (package-installed-p package)
        (if (eq is-pack-list-refreshed 0)
            (progn
                (package-refresh-contents)
                (setq is-pack-list-refreshed 1)))
        (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;; rtags
; common
(setq rtags-autostart-diagnostics t)
(setq rtags-display-result-backend 'ivy)

; code navigation
(setq eide-rtags-root (concat (getenv "EIDE_INSTALLATION_PATH") "/rtags"))

(setq rtags-path (concat eide-rtags-root "/bin"))
(load (concat eide-rtags-root "/src/rtags"))
(setq rtags-enable-unsaved-reparsing t)
;(customize-set-variable 'rtags-periodic-reparse-timeout 0.5)
(setq rtags-jump-to-first-match nil)

; EIDE_PROJECT_ROOT directory should contain:
;     - project source tree
;     - compile_commands.json file with appropriate compilations commans
; Then EIDIE starts 'rdm' and after 'rc' to index project sources.
(setq eide-project-root (getenv "EIDE_PROJECT_ROOT"))
(setq eide-rtags-index (concat eide-project-root "/eide_rtags_index"))
(setq rtags-process-flags (concat (concat "--progress --data-dir " eide-rtags-index) " -j10"))
(defun eide-rtags-parse-project ()
    (rtags-start-process-unless-running)
    (sleep-for 3) ; TODO: start eide-rtags-parse-project asynchronously; if 'eide_rtags_index' dir exists then return from this function immediately
    (start-process-shell-command "RTags" "*rc*" (concat "rc -J " eide-project-root)))
(eide-rtags-parse-project)

(define-key rtags-mode-map (kbd "M-c") nil)
(define-key c-mode-base-map (kbd "<f2>") 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "C-u") 'rtags-find-references-at-point)
(define-key c-mode-base-map (kbd "C-S-r") 'rtags-rename-symbol)
(define-key c-mode-base-map (kbd "C-M-v") 'rtags-find-virtuals-at-point)

; code indexer progrees
(add-to-list 'global-mode-string '(:eval (rtags-modeline)))
(add-hook 'rtags-diagnostics-hook (function force-mode-line-update))

; code completion
(require 'company)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(push 'company-files company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-SPC>") (function company-complete))
(setq completion-ignore-case t) ; TODO: repair case insensitivity for completion
;TODO: setup completion for file names

;;;;;;;;;;;;;;;;;;;;;;;;;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-wrap t)
(global-set-key (kbd "C-<next>") 'ivy-switch-buffer)
(define-key ivy-minibuffer-map (kbd "C-<next>") 'ivy-next-line)
(global-set-key (kbd "C-<prior>") 'ivy-switch-buffer)
(define-key ivy-minibuffer-map (kbd "C-<prior>") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "M-<up>") 'ivy-previous-history-element)
(define-key ivy-minibuffer-map (kbd "M-<down>") 'ivy-next-history-element)

(require 'ivy-rich)
(ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
(setq ivy-rich-abbreviate-paths t)
(defadvice ivy-rich-switch-buffer-format (around eide-ivy-rich-switch-buffer-format (columns) activate)
    (setq columns `(,(nth 2 columns) ,(nth 0 columns) ,(nth 5 columns)))  ; use only 'indicator', 'buf-name', 'path'
    ad-do-it)
;;TODO: to select appropriate buffer when 'crtl' key is up (possibly can be implemented with xdotool and other unix tools)

;;;;;;;;;;;;;;;;;;;;;;;;;; cua-mode
(cua-mode t)
(setq cua-keep-region-after-copy t)
(put 'dired-previous-line 'CUA 'move)
(put 'dired-next-line 'CUA 'move)
(put 'next-completion 'CUA 'move)
(put 'previous-completion 'CUA 'move)
(put 'close-current-buffer 'CUA 'move)

;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-enable-undo-in-region nil)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-M-z") 'redo)
(global-set-key (kbd "C-y") 'redo)

;;;;;;;;;;;;;;;;;;;;;;;;;; minibuffer
(setq text-in-current-region "")
(defun store-text-in-current-region ()
    (if (use-region-p)
        (setq text-in-current-region (buffer-substring (region-beginning) (region-end)))))
(defun insert-region-content-into-munibuffer ()
    (if (not (string-equal text-in-current-region ""))
        (insert text-in-current-region)))
(add-hook 'minibuffer-setup-hook 'insert-region-content-into-munibuffer)

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
    (store-text-in-current-region)
    (setq sch-global-regexp (read-regexp "Search"))
    (setq text-in-current-region "")
    (sch-forward)
)

; key bindings
(global-set-key (kbd "C-f") 'sch-search)
(global-set-key (kbd "<f3>") 'sch-forward)
(global-set-key (kbd "S-<f3>") 'sch-backward)

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
(defun forward-or-backward-sexp (&optional arg)
    (interactive "^p")
    (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))))
(global-set-key (kbd "C-p") 'forward-or-backward-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;; indentations
; C++
(setq c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;; syntacs actions
; LISP
(define-key emacs-lisp-mode-map (kbd "C-SPC") 'lisp-complete-symbol)
(defun find-lisp-func-def ()
    (interactive)
    (find-function (function-called-at-point)))
(define-key emacs-lisp-mode-map (kbd "<f2>") 'find-lisp-func-def)

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
;; C-
;;     j) exec LISP expression in *scratch* buffer
;;        also use 'M-x ielm' to interpret LISP.
(defun my-keyboard-escape-quit ()
; Redefinition of keyboard-escape-quit emacs original function.
; 'delete-other-windows' call removed.
; 'close-current-buffer' is used for this purposes.
    (interactive)
    (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
            (deactivate-mark))
        ((> (minibuffer-depth) 0)
            (abort-recursive-edit))
        (current-prefix-arg
            nil)
        ((> (recursion-depth) 0)
            (exit-recursive-edit))
        (buffer-quit-function
            (funcall buffer-quit-function))
        ((string-match "^ \\*" (buffer-name (current-buffer)))
            (bury-buffer))))
(global-set-key (kbd "M-q") 'my-keyboard-escape-quit)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
;(global-hl-line-mode 1) ; highlight current line

(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;; file actions
(defun save-all-buffers-without-questions ()
    (interactive)
    (save-some-buffers t))
(global-set-key (kbd "C-s") 'save-all-buffers-without-questions)

;;;;;;;;;;;;;;;;;;;;;;;;;; other
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;; buffers
;(setq enable-recursive-minibuffers t)
(defun close-current-buffer ()
    (interactive)
    (setq is-special-buffer (or (string-equal (buffer-name) "*rdm*") (string-equal (buffer-name) "*RTags Diagnostics*")))
    (if (and (= (length (get-buffer-window-list nil)) 1) (not is-special-buffer))
        (kill-buffer nil))
    (if (not (one-window-p t))
        (delete-window)))
(global-set-key (kbd "M-c") 'close-current-buffer)
(global-set-key (kbd "C-o") 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;; windows
(global-set-key (kbd "C-w") 'next-multiframe-window)
(defun switch-to-buffer-in-window ()
    (interactive)
    (switch-to-buffer-other-window (current-buffer)))

;(define-key input-decode-map [?\C-m] [C-m])
;(global-set-key (kbd "<C-m>") 'switch-to-buffer-in-window) ;; multiply buffer
(global-set-key (kbd "M-m") 'switch-to-buffer-in-window) ;; multiply buffer

;; TODO:
;; 2) tune makr and global mark ring
;; 3) correct identation for C++ buffer, python buffer; remove trailing white spaces on "save buffer" action
;; 4) integrate 'query-replace (conditional replace)
;; 5) rebind find-file-another-window (C-x 4 f)
;; 6) to intoroduce sch-search minor mode
;; 7) try LazyLock mode
;; 8) tune correct indentations for python and correct auto-format for C++ (if/else block, etc.)
;; 9) set C++ comment for region
;; 10) to use counsel-rg to integrate ripgrep into emacs
;; 11) to remove 'auto-save' and 'backup' files when save files in emacs.
;;     also don't create 'auto-save' file when 'save changes into file' is ignored and 'save changes in buffer to somewhere' is ignored too.
;; 12) 'projectile' project environment for emacs

;; to check the following search tools: swiper, helm swoop, swoop
