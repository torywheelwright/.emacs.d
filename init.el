;;; package --- Summary: tory wheelwright's emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use `package` to install `use-package`, which will install everything else

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configurations

(use-package alchemist
  :ensure t)

(use-package auto-package-update
  :ensure t
  :config (setq auto-package-update-delete-old-versions t)
          (setq auto-package-update-hide-results t)
          (auto-package-update-maybe))

(use-package bracketed-paste
  :ensure t
  :config (bracketed-paste-enable))

(use-package cc-mode
  :config (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; .h default to c++-mode
          (c-set-offset 'innamespace 0)                          ; don't indent namespaces
          (c-set-offset 'inlambda 0))                            ; no extra indent for lambda

(use-package company
  :ensure   t
  :diminish company-mode
  :config   (global-company-mode)
  :custom   ((company-idle-delay 0)
              (company-tooltip-align-annotations t)
              (company-c-headers-path-user
                (split-string
                  (shell-command-to-string "~/.emacs.d/user-headers-paths.sh")
                  "\n"
                  t))
              (company-c-headers-path-system
                (split-string
                  (shell-command-to-string "~/.emacs.d/system-headers-paths.sh")
                  "\n"
                  t))))

(use-package company-c-headers
  :ensure t)

(use-package diminish
  :ensure t)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :config (flycheck-add-next-checker 'python-flake8 'python-pylint)
          (global-flycheck-mode)
  :custom ((flycheck-clang-args (quote ("-std=c++17")))
            (flycheck-python-pylint-executable "python3")
            (flycheck-python-flake8-executable "python3")
            (flycheck-checker 'python-flake8)))

(use-package flycheck-credo
  :init   (add-hook 'flycheck-mode-hook #'flycheck-credo-setup)
  :custom (flycheck-elixir-credo-strict t))

(use-package flycheck-mix
  :ensure t
  :commands (flycheck-mix-setup))

(use-package helm
  :ensure t
  :config (helm-mode 1)
          (helm-autoresize-mode 1)
  :custom (helm-split-window-inside-p 1)
  :bind   (("M-x" . helm-M-x)
            ("M-y" . helm-show-kill-ring)
            ("C-x b" . helm-mini)
            ("C-x C-f" . helm-find-files)
            ("C-x f" . helm-find-files)))

(use-package jedi
  :ensure t
  :hook   (python-mode . 'jedi:setup)
  :custom (jedi:complete-on-dot t))

(use-package linum
  :config (global-linum-mode t)
  :custom (linum-format "%3d| "))

(use-package rainbow-delimiters
  :ensure t
  :hook   (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :ensure   t
  :diminish undo-tree-mode
  :config   (global-undo-tree-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :config   (global-whitespace-mode)
  :custom   (whitespace-style '(face lines-tail))
  :hook     ((prog-mode .    (lambda () (setq show-trailing-whitespace 1)))
              (c-mode .      (lambda () (setq whitespace-line-column 80)))
              (c++-mode .    (lambda () (setq whitespace-line-column 80)))
              (elixir-mode . (lambda () (setq whitespace-line-column 80)))
              (java-mode .   (lambda () (setq whitespace-line-column 100)))
              (python-mode . (lambda () (setq whitespace-line-column 79)))
              (rust-mode .   (lambda () (setq whitespace-line-column 99)))))

(use-package windmove
  :config (windmove-default-keybindings)
  :custom (shift-select-mode nil))

;; non-bundled language support
(use-package cython-mode     :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package elixir-mode     :ensure t)
(use-package json-mode       :ensure t)
(use-package rust-mode       :ensure t)
(use-package yaml-mode       :ensure t)

;;; ui stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
(load-theme 'indoors t)
;; don't show startup screen in gui mode
(setq inhibit-startup-screen t)
;; show column numbers, but no row numbers
(column-number-mode 1)
(line-number-mode 0)
;; make "yes/no" questions respond to "y/n"
(defalias 'yes-or-no-p 'y-or-n-p)
;; hide tool bar
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
;; hide menu bar
(menu-bar-mode 0)
;; create a new frame for every emacsclient, and clean them up on disconnect
;; https://www.emacswiki.org/emacs/EmacsClient#toc14
(add-hook 'server-switch-hook
  (lambda nil
    (let ((server-buf (current-buffer)))
      (bury-buffer)
      (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;;; improve some of the standard keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make C-a move alternately, to the beginning or end of leading whitespace
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

;; make C-x C-r rename the file being visited by the current buffer
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

;; make M-DEL delete the previous word
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; make C-x-{<left>/<right>} only visit code buffers
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

;;; change a few of the standard key bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unbind M-t (constantly hitting it by accident)
(global-unset-key (kbd "M-t"))
;; bind C-t to launch a term
(global-set-key (kbd "C-t") 'ansi-term)
;; bind C-x g to gdb
(global-set-key (kbd "C-x g") 'gdb)
;; bind F5 to compile
(global-set-key (kbd "<f5>") 'compile-go)
;; bind mouse forward/back buttons to switch buffers
(global-set-key [mouse-8] 'previous-buffer)
(global-set-key [mouse-9] 'next-buffer)
;; if a region is selected when delete is pressed, it gets deleted
(delete-selection-mode)

;;; init.el ends here
