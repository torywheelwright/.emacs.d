;;; node-modules-path.el --- Make `node_modules\.bin` available in `exec-path`. -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; When working in javascript, you typically want to use the local
;;; versions of tools like eslint, installed in a `node_modules/.bin` folder
;;; somewhere up the directory hierarchy from the file that the buffer in
;;; question is visiting. This package provides functions for prepending to
;;; the path each `node_modules/.bin` (nearest first), and for clearing these
;;; entries.
;;; Code:

(defvar node-modules-path--original-exec-path nil)

(make-local-variable 'node-modules-path--original-exec-path)
(make-local-variable 'exec-path)

(defun node-modules-path--find-bin-dirs (file)
    "Upward search from FILE for any `node_modules/.bin` folders and return as a list, nearest first."
    (let ((found-dir (locate-dominating-file file "node_modules/.bin")))
        (if found-dir
            (cons
                (expand-file-name "node_modules/.bin" found-dir)
                (node-modules-path--find-bin-dirs (expand-file-name (concat found-dir ".."))))
            nil)))
;; (message (node-modules-path--find-bin-dirs "~/tulip/tulip/environments/connector-host/src/connector-host/src/connectorAPI.js"))
;; (message (locate-dominating-file "~/tulip/tulip/environments/connector-host/src/connector-host/src/connectorAPI.js" "node_modules/.bin"))
;; ;;; ~/tulip/tulip/environments/connector-host/src/connector-host/
;; (message (expand-file-name "node_modules/.bin" "~/tulip/tulip/environments/connector-host/src/connector-host/"))
;; ;;; /Users/tory/tulip/tulip/environments/connector-host/src/connector-host/node_modules/.bin

;; (message (locate-dominating-file "~/tulip/tulip/environments/connector-host/src/" "node_modules/.bin"))

;; (message (expand-file-name "~/tulip/tulip/environments/connector-host/src/connector-host/.."))

;;;###autoload
(defun node-modules-path ()
    "Add node executables to the focused buffer's `exec-path`."
    (interactive)
    (unless node-modules-path--original-exec-path
        (setq node-modules-path--original-exec-path exec-path))
    (setq exec-path (append
                        (node-modules-path--find-bin-dirs (buffer-file-name))
                        node-modules-path--original-exec-path)))

(provide 'node-modules-path)

;;; node-modules-path.el ends here
