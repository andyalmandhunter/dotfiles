;;; flycheck --- Andy Almand-Hunter
;;; Commentary:

;;; Code:
;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
(global-set-key "\C-cj" 'flycheck-mode)

;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun prettier-eslint ()
  "Format the current file with Prettier followed by ESLint --fix."
  (interactive)
  (if (executable-find "prettier")
      (call-process "prettier" nil "*Prettier Errors*" nil "--write" buffer-file-name)
    (message "Prettier not found."))
  (if (executable-find "eslint")
      (progn (call-process "eslint" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
             (call-process "eslint" nil "*ESLint Errors*" nil "--fix" buffer-file-name))
    (message "ESLint not found."))
  (revert-buffer t t t))

;;; flycheck.el ends here
