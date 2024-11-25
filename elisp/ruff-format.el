;;; ruff-format.el --- Ruff format Python source     -*- lexical-binding: t; -*-

(require 'reformatter)

(defcustom ruff-format-command "ruff"
  "Ruff command to use for formatting."
  :type 'string
  :group 'ruff-format)



(defvar ruff-format--base-args '("format")
  "Base arguments to pass to black.")

(defcustom ruff-format-extra-args nil
  "Extra arguments to pass to black."
  :group 'ruff-format
  :type '(repeat string))
(defconst ruff-format--config-file "pyproject.toml")
(defconst ruff-format--config-file-marker-regex (rx bol "[tool.ruff]"))

(defun ruff-format-on-save-mode-enable-dwim ()
  "Enable ‘ruff-format-on-save-mode’ if this project is using Black.

The heuristic used looks for ‘[tool.ruff]’ in a ‘pyproject.toml’ file."
  (interactive)
  (when (ruff-format--buffer-in-blackened-project-p)
    (ruff-format-on-save-mode)))

(defun ruff-format--buffer-in-blackened-project-p ()
  "Check whether the current buffer resides in a project that is using Black."
  (-when-let* ((file-name (buffer-file-name))
               (project-directory (locate-dominating-file file-name ruff-format--config-file))
               (config-file (concat project-directory ruff-format--config-file))
               (config-file-contains-marker
                (with-temp-buffer
                  (insert-file-contents-literally config-file)
                  (re-search-forward ruff-format--config-file-marker-regex nil t 1))))
    t))


(defun ruff-format--make-args ()
  (let ((lst (append
             ruff-format--base-args
             (when-let (project-directory (file-truename (locate-dominating-file (buffer-file-name) ruff-format--config-file)))
               (list "--config" (concat project-directory ruff-format--config-file)))
             (list "--stdin-filename" (or (buffer-file-name) input-file))
             ruff-format-extra-args
             )))
    (message "ruff format args for %s: %s" (buffer-file-name) lst)
    lst))

(reformatter-define ruff-format
  :program ruff-format-command
  :args (ruff-format--make-args)
  :lighter " RuffFmt"
  :group 'ruff-format)

(provide 'ruff-format)
;;; ruff-format.el ends here
