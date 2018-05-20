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
                            company
                            company-rtags
                            sr-speedbar))

(defvar is-pack-list-refreshed 0)
(dolist (package required-packages)
    (unless (package-installed-p package)
        (if (eq is-pack-list-refreshed 0)
            (progn
                (package-refresh-contents)
                (setq is-pack-list-refreshed 1)))
        (package-install package)))

;(setq linum-format "%d │$")
;(setq linum-format "%4d \u2502 ")

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
; TODO: <enter> on symbol references buffer's elements should move cursor to selected file buffer (instead of pointing to references buffer). Maybe it is better to use Helm instead of Ivy here.

; code indexer progrees
(add-to-list 'global-mode-string '(:eval (rtags-modeline)))
(add-hook 'rtags-diagnostics-hook (function force-mode-line-update))

; code completion
(require 'company)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-SPC>") (function company-complete))
(setq completion-ignore-case t) ; TODO: repair case insensitivity for completion
;TODO: setup completion for file names
;TODO: close completion window when press escape
;TODO: decrease completion witdow appearance timetout

;;;;;;;;;;;;;;;;;;;;;;;;;; debugger
(require 'gud)
(setq gdb-many-windows t)

(defun eide-gud-core (func)
    (select-window (get-buffer-window gud-comint-buffer)) ; gdb current line cursor is shown correctly in code window when gud-* commands executed from gdb window
    (funcall func 1))

(define-key gud-minor-mode-map (kbd "<f10>") (lambda () (interactive) (eide-gud-core 'gud-next)))
(define-key gud-minor-mode-map (kbd "<f11>") (lambda () (interactive) (eide-gud-core 'gud-step)))
(define-key gud-minor-mode-map (kbd "<S-f11>") (lambda () (interactive) (eide-gud-core 'gud-finish)))
(define-key gud-minor-mode-map (kbd "<f5>") (lambda ()
    (interactive)
    (if gdb-active-process
        (eide-gud-core 'gud-cont)
        (eide-gud-core 'gud-run))))

(define-key gud-minor-mode-map (kbd "<S-f5>") (lambda ()
    (interactive)
    (gud-basic-call "save breakpoints ~/breakpoints.gdb") ; TODO: to save breakpoints into project directory
    (gud-basic-call "quit")
    (write-region "source ~/breakpoints.gdb" nil "~/commands.gdb" nil) ; TODO: to save gdb commands into project directory
    (while (get-buffer-process gud-comint-buffer)
        (sleep-for 0.1))
    (kill-buffer gud-comint-buffer)
    (delete-window)))

(global-set-key (kbd "<f5>") (lambda ()
    (interactive)
    (gdb (concat "gdb -i=mi --command=~/commands.gdb" " a.out")))) ; TODO: to provide real name of eide project binary path; take commands.gdb from prject root

; TODO: show all local vars in speedbar

; set/remove breakpoint by keyboard
(defun eide-has-breakpoint-at-line ()
    (interactive)
    (catch 'has-bp
        (dolist (b gdb-breakpoints-list)
            (setq filePath (bindat-get-field b 'fullname))
            (setq lineNo (string-to-number (bindat-get-field b 'line)))
            (when (and (string-equal filePath (buffer-file-name))
                       (eq lineNo (line-number-at-pos)))
                (throw 'has-bp b)))
        (throw 'has-bp nil)))

(define-key gud-minor-mode-map (kbd "M-b") (lambda ()
    (interactive)
    (if (not (eide-has-breakpoint-at-line))
        (gud-break nil)
        (gud-remove nil))))


(define-key gud-minor-mode-map (kbd "C-M-b") (lambda ()
    (interactive)
    (setq breakpoint (eide-has-breakpoint-at-line))
    (if breakpoint
        (gud-basic-call
            (concat
                (if (equal "y" (bindat-get-field breakpoint 'enabled))
                    "-break-disable "
                    "-break-enable ")
                (bindat-get-field breakpoint 'number))))))

; set windows custom layout
; gdb-restore-windows -- restore 'many-windows' layout
(defadvice gdb-setup-windows (after activate)
  (eide-gdb-setup-windows)
)

(defun eide-gdb-setup-windows ()
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (gdb-get-buffer-create 'gdb-stack-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) nil)
    (delete-other-windows)

    (let ((win-code (selected-window))
          (win-stack (split-window nil ( / ( * (window-height) 3) 4)))
          (win-locals (split-window-right ( / ( * (window-width) 5) 8))))
            ; set code buffer
        (set-window-buffer
            win-code
            (if gud-last-last-frame 
                (gud-find-file (car gud-last-last-frame))
                (if gdb-main-file
                      (gud-find-file gdb-main-file)
                    ;; Put buffer list in window if we
                    ;; can't find a source file.
                    (list-buffers-noselect))))
            ; set stack buffer
        (select-window win-stack)
        (gdb-set-window-buffer (gdb-stack-buffer-name))
            ; set breakpoints buffer
        (let ((win-breakpoints (split-window-right)))
            (gdb-set-window-buffer (if gdb-show-threads-by-default
                                       (gdb-threads-buffer-name)
                                     (gdb-breakpoints-buffer-name))
             nil win-breakpoints))

        (select-window win-locals)
        (let ((win-gdb-cmd (split-window nil ( / ( * (window-height) 2) 3)))
              (win-io (split-window-below)))
            ; set locals buffer
            (gdb-set-window-buffer (gdb-locals-buffer-name) nil win-locals)
            ; set gdb-cmd buffer
            (select-window win-gdb-cmd)
            (switch-to-buffer gud-comint-buffer)
            (set-window-dedicated-p (selected-window) t)
            ; set io buffer
            (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win-io)

            (select-window win-gdb-cmd))))

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
(put 'eide-close-current-buffer 'CUA 'move)

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
(xterm-mouse-mode 1) ; to paste selected text region from another application press "shift + middle_mouse_button"

;;;;;;;;;;;;;;;;;;;;;;;;;; buffers
;(setq enable-recursive-minibuffers t)
(defun eide-close-current-buffer ()
    (interactive)
    (setq is-special-buffer (or (string-equal (buffer-name) "*rdm*") (string-equal (buffer-name) "*RTags Diagnostics*")))
    (if (or (and gud-minor-mode (not (window-dedicated-p)))
         (and (not gud-minor-mode) (= (length (get-buffer-window-list nil)) 1) (not is-special-buffer)))
        (kill-buffer nil))
    (if (not (or (one-window-p t) gud-minor-mode))
        (delete-window))
)
(global-set-key (kbd "M-c") 'eide-close-current-buffer)
(global-set-key (kbd "C-o") 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;; windows
(global-set-key (kbd "C-w") 'next-multiframe-window)
(defun switch-to-buffer-in-window ()
    (interactive)
    (switch-to-buffer-other-window (current-buffer)))

;(define-key input-decode-map [?\C-m] [C-m])
;(global-set-key (kbd "<C-m>") 'switch-to-buffer-in-window) ;; multiply buffer
(global-set-key (kbd "M-m") (lambda () ; multiply buffer
    (interactive)
    (if (not gud-minor-mode)
        (switch-to-buffer-in-window))))

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
