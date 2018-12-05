;;; macos --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Interact with OSX clipboard
(defun copy-from-osx ()
  "Paste from clipboard."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy TEXT to clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Mac stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier nil)
(setq mac-emulate-three-button-mouse t)

;; Mac 'ls' command doesn't support '--dired'
(setq dired-use-ls-dired nil)

;;; macos.el ends here
