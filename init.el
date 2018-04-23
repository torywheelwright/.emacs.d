;;; package --- Summary: tory's emacs config
;;; Commentary:
;;; Code:
(require 'package)
(setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/")
	  ("marmalade" . "https://marmalade-repo.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)
(defvar package-packages '(bracketed-paste
			   dockerfile-mode
			   editorconfig
			   flycheck
			   helm
			   jedi
			   json-mode
			   monokai-theme
			   rainbow-delimiters
			   rust-mode
			   undo-tree
			   yaml-mode))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; ui stuff
(setq inhibit-startup-screen t)
(line-number-mode 0)
(column-number-mode 0)
(bracketed-paste-enable)
(setq mode-line-format
      (list '(vc-mode vc-mode)
	    ":"
	    mode-line-buffer-identification
	    " "
	    mode-line-position
	    " "
	    mode-line-modes))
(load-theme 'monokai t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(setq linum-format "%3d| ")
(global-linum-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)
(add-hook 'server-switch-hook
  (lambda nil
    (let ((server-buf (current-buffer)))
      (bury-buffer)
      (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

;; helm
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p 1)
(setq helm-buffers-fuzzy-matching 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-find-files)

;; editor stuff
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(defun rename-current-buffer-file ()
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(defun next-code-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
	 (string-match-p "^\*" (buffer-name))
	 (not (equal bread-crumb (buffer-name))))
      (next-buffer))))
(global-set-key [remap next-buffer] 'next-code-buffer)
(defun previous-code-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
	 (string-match-p "^\*" (buffer-name))
	 (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))
(global-set-key [remap previous-buffer] 'previous-code-buffer)
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "C-x g") 'gdb)
(global-set-key (kbd "<f5>") 'compile-go)
(global-set-key (kbd "C-t") 'ansi-term)
(global-set-key [mouse-8] 'previous-buffer)
  (global-set-key [mouse-9] 'next-buffer)
(setq shift-select-mode nil)
(windmove-default-keybindings)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode)
(add-hook 'after-change-major-mode-hook
	  '(lambda () (when (eq major-mode 'rust-mode)
			(setq whitespace-line-column 99))))
(add-hook 'after-change-major-mode-hook
	  '(lambda () (when (eq major-mode 'c-mode)
			(setq whitespace-line-column 80))))
(add-hook 'after-change-major-mode-hook
	  '(lambda () (when (eq major-mode 'c++-mode)
			(setq whitespace-line-column 80))))
(add-hook 'after-change-major-mode-hook
	  '(lambda () (when (eq major-mode 'python-mode)
			(setq whitespace-line-column 79))))
(add-hook 'after-change-major-mode-hook
	  '(lambda () (when (eq major-mode 'java-mode)
			(setq whitespace-line-column 100))))
(editorconfig-mode 1)
(delete-selection-mode)
(global-undo-tree-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace 1)))

;; c/++ stuff
(add-hook 'c++-mode-hook
    (lambda () (setq flycheck-clang-language-standard "c++14")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-set-offset 'innamespace 0) ;; Don't indent namespaces
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument.
http://stackoverflow.com/a/23553882"
  (setq ad-return-value
    (if (and (equal major-mode 'c++-mode)
          (ignore-errors
            (save-excursion
              (goto-char (c-langelem-pos langelem))
              ;; Detect "[...](" or "[...]{". preceded by "," or "(",
              ;;   and with unclosed brace.
              (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
      0                             ; no additional indent
      ad-do-it)))                   ; default behavior)

;; python stuff
(setq flycheck-python-pylint3-executable "python3")
(setq flycheck-python-flake8-executable "python3")
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode undo-tree rust-mode rainbow-delimiters monokai-theme json-mode elpy editorconfig dockerfile-mode bracketed-paste))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
