;;; scalafmter.el --- (automatically) format scala buffers using SCALAFMT.

;; Author: Elliot Chow

;; Installation:
;;
;; Add scalafmter.el to your load-path.
;;
;; To automatically format all Scala buffers before saving, add the function
;; scalafmt-mode to scala-mode-hook:
;;
;; (add-hook 'scala-mode-hook 'scalafmt-mode)
;;
;;; Code:

(require 'cl-lib)

(defun find-closest-parent-dir-containing-file (leaf-path filename)
  (interactive)
  (if (and leaf-path (not (equal leaf-path "")) (not (equal leaf-path "/")))
      (let* ((parent (file-name-directory (directory-file-name leaf-path)))
             (candidate (concat parent filename)))
        (if (file-exists-p candidate)
            candidate
          (find-closest-parent-dir-containing-file parent filename)))
    )
  )

(defcustom scalafmter-executable "scalafmt"
  "Executable used to start scalafmt."
  :type 'string
  :group 'scalafmter)

(defun scalafmter-call-bin (input-buffer output-buffer)
  "Call process scalafmt on INPUT-BUFFER saving the output to OUTPUT-BUFFER.

Return the exit code.
format."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max) scalafmter-executable nil output-buffer nil "--stdin" (buffer-file-name))
    ))

(defun get-buffer-string (buffer)
  "Return the contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun scalafmter-buffer ()
  "Try to scalafmter the current region.

If scalafmt exits with an error, the output will be shown in a help-window."
  (interactive "r")
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (buffer-windows (get-buffer-window-list original-buffer nil t))
         (original-window-pos (mapcar 'window-start buffer-windows))
         (tmpbuf (generate-new-buffer (format "*scalafmter-%s*" (buffer-name original-buffer))))
         (exit-code (scalafmter-call-bin original-buffer tmpbuf)))
    (deactivate-mark)
    ;; There are three exit-codes defined for SCALAFMT:
    ;; 0: Exit with success (change or no change on scalafmt >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((eq exit-code 0)
           (with-current-buffer tmpbuf
             (if (< (point-min) (point-max))(copy-to-buffer original-buffer (point-min) (point-max)))))
          ((or (eq exit-code 1) (eq exit-code 2))
           (error "Scalafmt failed, see %s buffer for details" (buffer-name tmpbuf))))
    ;; Clean up tmpbuf
    (mapc 'kill-buffer (seq-filter (lambda (x) (string-prefix-p "*scalafmter-" (buffer-name x))) (buffer-list)))
    ;; restore window to similar state
    (goto-char original-point)
    (cl-mapcar 'set-window-start buffer-windows original-window-pos)))

;;;###autoload
(define-minor-mode scalafmt-mode
  "Automatically run SCALAFMT before saving."
  :lighter " SCALAFMT"
  (if scalafmt-mode
      (add-hook 'before-save-hook 'scalafmter-buffer nil t)
    (remove-hook 'before-save-hook 'scalafmter-buffer t)))

(provide 'scalafmter)

;;; scalafmter.el ends here
