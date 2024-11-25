;;; ruff-check.el --- Ruff check Python source     -*- lexical-binding: t; -*-

(require 'reformatter)

(defcustom ruff-check-command "ruff"
  "Ruff command to use for formatting."
  :type 'string
  :group 'ruff-check)



(defvar ruff-check--base-args '("check")
  "Base arguments to pass to black.")

(defcustom ruff-check-extra-args '("--fix")
  "Extra arguments to pass to black."
  :group 'ruff-check
  :type '(repeat string))
(defconst ruff-check--config-file "pyproject.toml")
(defconst ruff-check--config-file-marker-regex (rx bol "[tool.ruff]"))

(defun ruff-check-on-save-mode-enable-dwim ()
  "Enable ‘ruff-check-on-save-mode’ if this project is using Black.

The heuristic used looks for ‘[tool.ruff]’ in a ‘pyproject.toml’ file."
  (interactive)
  (when (ruff-check--buffer-in-blackened-project-p)
    (ruff-check-on-save-mode)))

(defun ruff-check--buffer-in-blackened-project-p ()
  "Check whether the current buffer resides in a project that is using Black."
  (-when-let* ((file-name (buffer-file-name))
               (project-directory (locate-dominating-file file-name ruff-check--config-file))
               (config-file (concat project-directory ruff-check--config-file))
               (config-file-contains-marker
                (with-temp-buffer
                  (insert-file-contents-literally config-file)
                  (re-search-forward ruff-check--config-file-marker-regex nil t 1))))
    t))


(defun ruff-check--make-args ()
  (let ((lst (append
             ruff-check--base-args
             (when-let (project-directory (file-truename (locate-dominating-file (buffer-file-name) ruff-check--config-file)))
               (list "--config" (concat project-directory ruff-check--config-file)))
             (list "--stdin-filename" (or (buffer-file-name) input-file))
             ruff-check-extra-args
             )))
    (message "ruff check args for %s: %s" (buffer-file-name) lst)
    lst))

(reformatter-define ruff-check
  :program ruff-check-command
  :args (ruff-check--make-args)
  :lighter " RuffFmt"
  :group 'ruff-check)

(provide 'ruff-check)
;;; ruff-check.el ends here
