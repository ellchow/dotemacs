;;;; Elliot's .emacs
(defvar ELISPDIR (format "%s/elisp" (getenv "HOME")))
(setq load-path (cons ELISPDIR load-path))
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory ELISPDIR)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;; display
(require 'color-theme)
(load-file (format "%s/color-theme-echow.el" ELISPDIR))
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-echow)))

(when window-system (setq initial-frame-alist (x-parse-geometry "85x50+0+0")))

(set-default-font "-bitstream-Courier 10 Pitch-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(blink-cursor-mode 1)

(global-hl-line-mode 1)

(menu-bar-mode 1)
(tool-bar-mode 0)

(transient-mark-mode t)

(setq backup-inhibited t)

(setq backward-delete-char-untabify-method 'hungry)

(setq column-number-mode t)
(setq line-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 1)

(delete-selection-mode t)

(setq query-replace-highlight t)

(setq search-highlight t)

(show-paren-mode 1)

(set-display-table-slot standard-display-table 'wrap ?\\)

(setq inhibit-startup-message t)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;;;;;;;;;;;;;;; behaviors and functions

(require 'redo)

;;;; copy paste
(setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defun copy-line ()
      (interactive)
      (kill-ring-save (line-beginning-position)
                      (line-end-position))
      (message "line copied")
)

;;;; tramp
(setq password-cache-expiry 72000)
(setq tramp-default-method "ssh")

;;;; indentation
(defun indent-entire-buffer ()
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'indent-region)
)

;;;; hiding
(defun hs-load-hide-block ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-block)
)
(defun hs-load-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all)
)
(defun hs-load-show-block ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-block)
)
(defun hs-load-show-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all)
)

(defun julia-indent-newline-indent ()
  (interactive)

  (defun end-section()
    (insert "\nend")
    (indent-region (line-beginning-position) (line-end-position))
    (previous-line)
    (end-of-line)
    )

  (defun run ()

    (if (string-match "^\s*\\(end\\|else\\|elseif\\)\s*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (indent-region (line-beginning-position) (line-end-position)))

    (if (string-match "^\s*\\(if\\|module\\|function\\|immutable\\|type\\|else\\|for\\|while\\|begin\\|let\\)\s.*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (end-section))

    (newline-and-indent)
    )

  (if (eq (point) (line-end-position))
    (run)
    (newline-and-indent))
)

;;;;  from https://github.com/mattharrison/point-stack/blob/master/point-stack.el
(defvar point-stack-stack nil)
(defvar point-stack-forward-stack nil)
(defun point-stack-push ()
  "Push current buffer, point, and scroll position onto stack."
  (interactive)
  (point-stack-store 'point-stack-stack)
  (setq point-stack-forward-stack nil) ; new step resets forward history
  (message "Location marked."))
(defun point-stack-pop ()
  "Push current location onto forward stack, move to previous location."
  (interactive)
  (if (null point-stack-stack)
      (message "Stack is empty.")
    (point-stack-store 'point-stack-forward-stack)
    (point-stack-go (car point-stack-stack))
    (setq point-stack-stack (cdr point-stack-stack))))
(defun point-stack-forward-stack-pop ()
  "Push current location onto stack, pop and move to location from forward stack."
  (interactive)
  (if (null point-stack-forward-stack)
      (message "forward Stack is empty.")
    (point-stack-store 'point-stack-stack)
    (point-stack-go (car point-stack-forward-stack))
    (setq point-stack-forward-stack (cdr point-stack-forward-stack))))
(defun point-stack-store (stack)
  (let ((loc (car (symbol-value stack))))
    ;; don't push the same location twice
    (unless (and (eq (current-buffer) (car loc))
                 (eq (point) (cadr loc)))
      (add-to-list stack (list (current-buffer) (point) (window-start))))))
(defun point-stack-go (loc)
  (switch-to-buffer (car loc))
  (set-window-start nil (caddr loc))
  (goto-char (cadr loc)))
(provide 'point-stack)


;;;; window resize

(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

;;;; formatting
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

(defun sort-lines-ignore-case ()
  (interactive)
  (setq sort-fold-case t)
  (call-interactively 'sort-lines)
  (setq sort-fold-case nil)
)


;;;; tab complete

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))
(setq hippie-expand-ignore-buffers
      (append hippie-expand-ignore-buffers '("TAGS")))

(defun clever-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

;;;; tab width
;; (defun tab-2 ()
;;   (interactive)
;;   (save-excursion
;;     (setq tab-width 2)
;;     (untabify (point-min) (point-max))))

;; (defun tab-4 ()
;;   (interactive)
;;   (save-excursion
;;     (setq tab-width 4)
;;     (untabify (point-min) (point-max))))

;; (defun tab-8 ()
;;   (interactive)
;;   (save-excursion
;;     (setq tab-width 8)
;;     (untabify (point-min) (point-max))))

;;;; comments
(defun comment-line ()
  (interactive)
  (set-mark (line-beginning-position))
  (end-of-line)
  (comment-region (region-beginning) (region-end))
  (set-mark nil)
)

(defun uncomment-line ()
  (interactive)
  (delete-horizontal-space)
  (set-mark (line-beginning-position))
  (end-of-line)
  (uncomment-region (region-beginning) (region-end))
  (call-interactively 'indent-region)
  (set-mark nil)
)

;; (defun indent-line ()
;;   (interactive)
;;   (delete-horizontal-space)
;;   (set-mark (line-beginning-position))
;;   (end-of-line)
;;   (call-interactively 'indent-region)
;;   (set-mark nil)
;; )

;;;; saving
(defun my-save ()
  (interactive)

  (when (not (string-match "\[.\]\\(tsv\\|md\\)" (buffer-name))) (untabify (point-min) (point-max)))

  (when (not (string-match "\[.\]\\(tsv\\)" (buffer-name))) (delete-trailing-whitespace))

  (save-buffer)
  )

;;;; eshell
(defun my-clear-eshell ()
  (interactive)
  (if (eq (current-buffer) (get-buffer "*eshell*"))
      (my-clear-eshell-helper)
      (message "Not in eshell buffer"))
)

(defun my-clear-eshell-helper ()
  (setq x default-directory)
  (kill-buffer "*eshell*")
  (find-file x)
  (setq y (current-buffer))
  (eshell)
  (kill-buffer y)
  )

(defun my-start-or-clear-eshell ()
  (interactive)
  (setq x (get-buffer "*eshell*"))
  (setq y (current-buffer))
  (eshell)
  (if (eq y x)
      (my-clear-eshell)
  )
)

;;;; scratch buffer
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defvar ywb-scratch-buffer "*scratch*")
(defun switch2scratch (arg)
  (interactive "P")
  (when arg
    (setq ywb-scratch-buffer (read-buffer "Set scratch to: " (buffer-name))))
  (let ((buf (get-buffer ywb-scratch-buffer)))
    (if (null buf)
        (progn
          (or arg
              (setq ywb-scratch-buffer (if (y-or-n-p "The buffer does not exist! Create *scratch*? ")
                                           "*scratch*"
                                         (read-buffer "Set scratch to: " (buffer-name)))))
          (switch-to-buffer ywb-scratch-buffer)
          (lisp-interaction-mode))
      (switch-to-buffer ywb-scratch-buffer))))

;;;; parentheses
(require'autopair)
(autopair-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;; tools

;;;; git
(add-to-list 'load-path (format "%s/egg" ELISPDIR))
(require 'egg)
;; (setq egg-auto-update t)
(setq egg-switch-to-buffer t)
;; (require 'git-blame)

;;;; ido
(setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

;;;; abbrev
(setq default-abbrev-mode t)


;;;;;;;;;;;;;;;;;;;;;;;; modes

;;;; java

(defun my-java-init ()
  (define-mode-abbrev "psvm" "public static void main (String[] args)")
)
(add-hook 'java-mode-hook 'my-java-init)


;;;; simple note
(require 'simplenote)
(setq simplenote-email "zzzxqq@gmail.com")
(setq simplenote-password nil)
(simplenote-setup)


;;;; json
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avdl$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avpr$" . json-mode))

(require 'python-mode)
(setq tab-width 2)
(setq-default py-indent-offset 2)
(define-key py-mode-map [backspace] 'py-electric-backspace)
(setq py-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.yaml$" . python-mode))

;;;; Haskell
(add-to-list 'load-path (format "%s/haskell-mode" ELISPDIR))
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
; use only one indentation mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;;;; Javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)
;; (setq js-indent-level 2)

;;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;;;; Scala  https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)
(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;;;; clojure mode
(require 'clojure-mode)

(require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;; sql
(eval-after-load "sql"
  '(load-library "sql-indent"))

;;;; ESS (R)
(require 'ess-site)
(ess-toggle-underscore nil)
(define-key ess-mode-map "\t" 'clever-hippie-tab)
(setq ess-indent-level 2)


;;;;; ESS (Julia)
(add-hook 'julia-mode-hook '(lambda () (local-set-key (kbd "RET") `julia-indent-newline-indent)))

;;;; Pig
;; (require 'pig-mode)

;;;; Markdown Mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdownfiles" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(require 'markdown-mode)

;;;; Textile Mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))


;;;; Protobuf Mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))




;;;;;;;;;;;;;; libraries

;;;; bookmarking
(setq bm-restore-repository-on-load t)
(require 'bm)
(setq-default bm-buffer-persistence t)
(add-hook' after-init-hook 'bm-repository-load)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'kill-emacs-hook (lambda ()
                             (bm-buffer-save-all)
                             (bm-repository-save)))
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(setq bm-cycle-all-buffers 1)


;;;;;;;;;;;; key bindings
(setq mac-command-modifier 'meta)

(global-set-key "\C-c\C-k" 'copy-line)

(global-set-key "\C-c,L" 'sort-lines)
(global-set-key "\C-c,l" 'sort-lines-ignore-case)

(global-set-key "\C-c\C-n"  'bm-next)
(global-set-key "\C-c\C-p"  'bm-previous)
(global-set-key "\C-c\C-b" 'bm-toggle)

(global-set-key [f7]  'my-start-or-clear-eshell)
(global-set-key [f3]  'point-stack-push)
(global-set-key [f4]  'point-stack-pop)
(global-set-key [f5]  'point-stack-forward-stack-pop)

(global-set-key "\C-cg"  'egg-status)
(global-set-key "\C-cl"  'egg-log)
;; (global-set-key "\C-cb"  'git-blame-mode)

(global-set-key "\C-ch" 'hs-load-hide-block)
(global-set-key "\C-cH" 'hs-load-hide-all)
(global-set-key "\C-cs" 'hs-load-show-block)
(global-set-key "\C-cS" 'hs-load-show-all)

(global-set-key "\C-r" 'redo)
(global-set-key "\C-x\C-s"  'my-save)

(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-c,s" 'switch2scratch)

(global-set-key "\M-\\"     'uncomment-line)
(global-set-key "\C-\\" 'comment-line)
(global-set-key "\C-cc"     'comment-region)
(global-set-key "\C-cv"      'uncomment-region)

(global-set-key "\C-c+" 'resize-window)

(global-set-key "\t" 'clever-hippie-tab)

;;;;

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(add-hook 'find-file-hook
  (lambda ()
    (show-paren-mode 1)
    (autopair-mode 1)
    )
  )
(put 'downcase-region 'disabled nil)
