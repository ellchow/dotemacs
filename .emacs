;;;; Elliot's .emacs

(defvar ELISPDIR (format "%s/elisp" (getenv "HOME")))
(setq load-path (cons ELISPDIR load-path))
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory ELISPDIR)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))



;;;; COLOR THEME
(defun set-color-theme ()
  (require 'color-theme)
  (load-file (format "%s/color-theme-gruber-darker.el" ELISPDIR))
  (load-file (format "%s/color-theme-subdued.el" ELISPDIR))
  (load-file (format "%s/color-theme-echow.el" ELISPDIR))
  ;; (color-theme-gruber-darker)
  ;; (color-theme-subdued)
  (color-theme-echow))
;; (if (not(eq (display-graphic-p) nil))
    (set-color-theme)
  ;; )


;;;; Basic Settings

(setq mac-command-modifier 'meta)

; cols x rows (character) +x+y (pixel)
(when window-system
  (setq initial-frame-alist (x-parse-geometry "85x50+0+0")))
; Font
(set-default-font "-bitstream-Courier 10 Pitch-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")


  ;; turn on the mouse wheel
(mouse-wheel-mode t)

  ;; blink the cursor
(blink-cursor-mode 1)

  ;; highlight line-mode
(global-hl-line-mode 1)

;; i don't like menus...
(menu-bar-mode 0)
;;(tool-bar-mode 0)

;; personally i like transient-mark-mode
(transient-mark-mode t)

(setq backup-inhibited t)

(setq backward-delete-char-untabify-method 'hungry)
(setq column-number-mode t)
;; (setq confirm-before-kill-emacs nil)
(setq line-number-mode t)
;;(global-linum-mode 1)

;; type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; turn of beeping sound
;setq visible-bell t)
(setq ring-bell-function 'ignore)

;;;; column marker
(require 'column-marker)
(defun column-marker-2-80 () (interactive) (column-marker-2 80))

;; scroll 1 at a time at bottom of page
;;(setq scroll-step 1)
(setq scroll-conservatively 1)
;;(scroll-bar-mode 1)

;; replace highlighted text with what I type rather than just
;; inserting at a point
(delete-selection-mode t)

;; resize the mini-buffer when necessary
;(setq resize-minibuffer-mode t)

;; highlight during searching
(setq query-replace-highlight t)

;; highlight incremental search
(setq search-highlight t)

;; (setq require-final-newline nil)

(require 'redo)

(show-paren-mode 1)
;; (require 'highlight-parentheses)

;; line wrap
;; (setq global-visual-line-mode 1)
(set-display-table-slot standard-display-table 'wrap ?\\)

(setq inhibit-startup-message t)


;; tramp/ssh
(setq password-cache-expiry 72000)
(setq tramp-default-method "ssh")

;; fix copy/paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;;; HELPERS

(defun string-replace (string from to)
  (mapconcat 'identity (split-string string from) to))

(defun hungry-delete ()
  "Delete character and consecutive whitespace before point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t")
    (if (/= (point) here)
        (delete-region (point) here)
      (delete-backward-char 1))))

(defun hungry-delete-ws ()
  "Delete consecutive whitespace before point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t\n")
    (if (/= (point) here)
        (delete-region (point) here))))

(defun hungry-delete-forward ()
  "Delete character and consecutive whitespace after point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point))
      (delete-backward-char 1))))

(defun hungry-delete-forward-ws ()
  "Delete consecutive whitespace after point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point)))))

(defvar point-stack nil)

(defun point-stack-push ()
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

;;;; Bookmarks
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




(defun indent-entire-buffer ()
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'indent-region)
)

(defun eval-buffer2 ()
        (interactive)
        (eval-buffer)
        (eval-buffer)
)

(defun mvn-dir ()
  (interactive)
  (locate-dominating-file (buffer-file-name) "pom.xml")
)

(defun exec-mvn-cmd (cmd)
  (interactive)
  (shell-command (format "cd %s && mvn %s" (mvn-dir) cmd))
)

(defun exec-mvn-compile ()
  (interactive)
  (exec-mvn-cmd "compile")
)

(defun open-pom (x)
  (interactive)
  (find-file (format "%s/pom.xml" (mvn-dir)))
)

(defun open-current-pom()
  (interactive)
  (open-pom (buffer-file-name))
)

(defun build-sbt-dir ()
  (interactive)
  (locate-dominating-file (buffer-file-name) "build.sbt")
)

(defun open-build-sbt (x)
  (interactive)
  (find-file (format "%s/build.sbt" (build-sbt-dir)))
)

(defun open-current-build-sbt()
  (interactive)
  (open-build-sbt (buffer-file-name))
)

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

(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;;;; HIPPIE EXPANSION

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

(defun tab-2 ()
  (interactive)
  (save-excursion
    (setq tab-width 2)
    (untabify (point-min) (point-max))))

(defun tab-4 ()
  (interactive)
  (save-excursion
    (setq tab-width 4)
    (untabify (point-min) (point-max))))

(defun tab-8 ()
  (interactive)
  (save-excursion
    (setq tab-width 8)
    (untabify (point-min) (point-max))))

;;(defalias 'print 'ps-print-buffer-with-faces)

;; GLOBAL KEY BINDING

(defvar mode-list
  '(emacs-lisp-mode java-mode lisp-interaction-mode lisp-mode makefile-mode
                    perl-mode python-mode sgml-mode shell-mode shell-script-mode tetris-mode
                    c-mode-common text-mode fundamental-mode sql-mode sql-interactive-mode
                    generic-mode gud-mode bat-generic-mode properties-generic-mode p4-buffer-mode
                    nxml-mode)
  "List of all the modes that these key bindings should apply to.")

(defvar the-cc-modes '(c-mode c++-mode objc-mode csharp-mode java-mode idl-mode pike-mode)
  "List of the modes which are 'subclasses' of cc-mode")

(defun global-set-key-override (keys func &optional mode)
  (if (null mode)
      (global-set-key keys func))
  (if (null mode)
      (global-set-key keys func))
  (global-set-key-override0 keys func mode))

(defun global-set-key-override0 (keys func &optional mode)
  (let* ((the-mode (if (null mode) 'global-mode mode))
         (bindings (get 'global-key-overrides the-mode))
         (binding (assoc keys bindings)))
    (if (or (null bindings) (null binding))
        (setq bindings (cons (cons keys func) bindings))
      (setcdr binding func))
    (put 'global-key-overrides the-mode bindings))
  t)

(defun global-bindings-override-hook ()
  "Function that's called for the various major modes to override bindings."
  (message (format "Applying bindings for %s" major-mode))

  ;; first map global bindings
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides 'global-mode))
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides major-mode))

  ;; check to see if the major-mode is a subclass of the cc-modes, and
  ;; if so, invoke the binding overrides defined for c-mode-common
  (when (memq major-mode the-cc-modes)
    ;;(message "Applying common bindings for %s" major-mode)
    (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
          (get 'global-key-overrides 'c-common-mode))))

;; Add our hook to all the defined hooks in 'mode-list'.
(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  'global-bindings-override-hook))
      mode-list)
(add-hook 'find-file-hooks 'global-bindings-override-hook)



;; DABBREV

(require 'dabbrev)
(define-abbrev-table 'java-mode-abbrev-table
  '(("psvm" "public static void main(String[] args)" nil 1)
    ("sopl" "System.out.println" nil 1)
    ("sop" "System.out.print" nil 1)
    ("sof" "System.out.flush" nil 1)
    ("sep" "System.err.print" nil 1)
    ("sepl" "System.err.println" nil 1)
    ("ctm" "System.currentTimeMillis" nil 1)
    ("sac" "System.arraycopy" nil 1)
    ("ija" "import java.awt.*")
    ("iji" "import java.io.*")
    ("ijn" "import java.net.*")
    ("ijt" "import java.text.*")
    ("iju" "import java.util.*")
    ("/*" "/*
*
*/")
    ("cch" "/**
 *
 *
 * @author
 * @version     %\111%, %\107%
 */" java-class-comment-hook 0)
    ("mch" "/**
 *
 */" nil 0)
    ))


;;;;

(defun switch-to-buffer-nocreate (buffer)
  "Switch to a buffer but don't create one if it doesn't exist."
  (interactive "bSwitch to buffer ")
  (switch-to-buffer buffer))

;; hack to clear comint buffer when I use certain commands
(defun my-comint-filter (x)
  (when (string-match "\\(startup\\|units\\)\n" x)
    (kill-region (point-min) (point-max))
    (insert (format "(buffer cleared by my-comint-filter)\n> %s" x))))
(add-hook 'shell-mode-hook '(lambda () (add-hook 'comint-input-filter-functions 'my-comint-filter nil t)))

;; overridden to modify compilation-search-path
(defun compilation-find-file (marker filename dir &rest formats)
  "Find a buffer for file FILENAME.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
current directory, which is passed in DIR.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask the user."
  (or formats (setq formats '("%s")))
  (save-excursion
    (let ((dirs compilation-search-path)
          buffer thisdir fmts name)
      (if (file-name-absolute-p filename)
          ;; The file name is absolute.  Use its explicit directory as
          ;; the first in the search path, and strip it from FILENAME.
          (setq filename (abbreviate-file-name (expand-file-name filename))
                dirs (cons (file-name-directory filename) dirs)
                filename (file-name-nondirectory filename)))
      ;; Now search the path.
      (while (and dirs (null buffer))
        (setq thisdir (or (car dirs) dir)
              fmts formats)
        ;; For each directory, try each format string.
        (while (and fmts (null buffer))
          (setq name (expand-file-name (format (car fmts) filename) thisdir)
                buffer (and (file-exists-p name)
                            (find-file-noselect name))
                fmts (cdr fmts)))
        (setq dirs (cdr dirs)))
      (or buffer
          ;; The file doesn't exist.
          ;; Ask the user where to find it.
          ;; If he hits C-g, then the next time he does
          ;; next-error, he'll skip past it.
          (let* ((pop-up-windows t)
                 (w (display-buffer (marker-buffer marker))))
            (set-window-point w marker)
            (set-window-start w marker)
            (let ((name (expand-file-name
                         (read-file-name
                          (format "Find this error in: (default %s) "
                                  filename)
                          dir filename t))))
              (if (file-directory-p name)
                  (setq name (expand-file-name filename name)))

              ;; amd
              (setq compilation-search-path
                    (cons (file-name-directory name) compilation-search-path))

              (setq buffer (and (file-exists-p name)
                                (find-file name))))))
      ;; Make intangible overlays tangible.
      (mapcar (function (lambda (ov)
                          (when (overlay-get ov 'intangible)
                            (overlay-put ov 'intangible nil))))
              (overlays-in (point-min) (point-max)))
      buffer)))

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

(defun copy-line()
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position))
)

(defun my-save ()
  (interactive)

  (when (not (string-match "\[.\]\\(tsv\\|md\\)" (buffer-name))) (untabify (point-min) (point-max)))

  (when (not (string-match "\[.\]\\(tsv\\)" (buffer-name))) (delete-trailing-whitespace))

  (save-buffer)
  )

;; (defun my-clear-shell ()
;;    (interactive)
;;    (let ((old-max comint-buffer-maximum-size))
;;      (setq comint-buffer-maximum-size 0)
;;      (comint-truncate-buffer)
;;      (setq comint-buffer-maximum-size old-max)))

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

;;;; AUTO RECREATE SCRATCH BUFFER
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

;;;; AUTOPAIR
(require'autopair)
(autopair-global-mode 1)

;;;; Git
(add-to-list 'load-path (format "%s/egg" ELISPDIR))
(require 'egg)
(require 'git-blame)
(defun exec-git-pull ()
        (interactive)
  (shell-command "git pull")
)
(defun exec-git-pull-push ()
        (interactive)
  (shell-command "git pull && git push")
)

;;;; MODES

; IDO

(setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

;; tab width
;; (setq c-basic-offset 4)

; JSON

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

; Python
(require 'python-mode)
(setq tab-width 2)
(setq-default py-indent-offset 2)
(define-key py-mode-map [backspace] 'py-electric-backspace)
(setq py-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.yaml$" . python-mode))

; Haskell
;; (load (format "%s/haskell-mode/haskell-site-file.el" ELISPDIR))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; ; use only one indentation mode
;; ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)



; Javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)

;; ; CEDET
;; (load-file (format "%s/cedet-1.0/common/cedet.el" ELISPDIR))
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu


;; ; ECB
;; (add-to-list 'load-path (format "%s/ecb-2.40" ELISPDIR))
;; (setq ecb-tip-of-the-day nil)
;; (require 'ecb)
;; (require 'ecb-autoloads)

; Scala
(add-to-list 'load-path (format "%s/scala" ELISPDIR))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(require 'scala-mode-auto)


(add-to-list 'load-path "~/elisp/scalaz")
(require 'scalaz-unicode-input-method)
;; (add-hook 'scala-mode-hook
  ;; (lambda () (set-input-method "scalaz-unicode")))

;; (add-to-list 'load-path (format "%s/ensime_2.9.2-0.9.8.1/elisp/" ELISPDIR))
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


; SQL
(eval-after-load "sql"
  '(load-library "sql-indent"))


; ESS
(require 'ess-site)
(ess-toggle-underscore nil)

;; (require 'pig-mode)

; Markdown Mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdownfiles" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(require 'markdown-mode)

;; Textile Mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;;; KEY BINDINGS

(global-set-key [f4]  'toggle-input-method)
;; (global-set-key [f6]  'point-stack-push)
;; (global-set-key [C-f6]  'point-stack-pop)
(global-set-key [C-f6]  'bm-next)
(global-set-key [M-f6]  'bm-previous)
(global-set-key [f6] 'bm-toggle)
(global-set-key [f7]  'my-start-or-clear-eshell)
;; (global-set-key [f3]  'exec-mvn-compile)
;; (global-set-key [f3]  'exec-mvn-compile)
(global-set-key [f8] 'column-marker-2-80)
(global-set-key [C-f9]  'egg-log)
(global-set-key [C-S-f9]  'exec-git-pull-push)
;; (global-set-key [C-M-f9]  'exec-git-pull-push)
(global-set-key [M-f9]  'egg-checkout-ref)
(global-set-key [M-S-f9]  'egg-start-new-branch)


(global-set-key "\C-ch" 'hs-load-hide-block)
(global-set-key "\C-cH" 'hs-load-hide-all)
(global-set-key "\C-cs" 'hs-load-show-block)
(global-set-key "\C-cS" 'hs-load-show-all)
(global-set-key [C-backspace]   'backward-kill-word)
(global-set-key "\C-r" 'redo)
(global-set-key "\C-c\C-g" 'goto-line)

;; Buffer management
(global-set-key "\C-x\C-s"  'my-save)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-c,s" 'switch2scratch)

(global-set-key "\M-\\"     'uncomment-line)
(global-set-key "\C-\\" 'comment-line)
(global-set-key [f5]      'comment-region)
(global-set-key [C-f5]      'uncomment-region)

(define-key minibuffer-local-map "\t" 'hippie-expand)

;;; mistakes
(global-set-key "\C-xf"     'find-file)
(global-set-key "\C-x\C-f"  'find-file)
(global-set-key "\C-xs"     'my-save)

;; use TAB key for completion everywhere
(global-set-key-override0 "\t" 'clever-hippie-tab)


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
    ;;(highlight-parentheses-mode 0)
    ;; (hs-minor-mode 1)
    (autopair-mode 1)
    ))
