;;; ob-tsnode.el --- org-babel functions for typescript evaluation

;; Copyright (C) 2015 KURASHIKI Satoru

;; Author: Tom Klaver
;; Keywords: literate programming, reproducible research, typescript
;; Package-Version: 0.0.1
;; Homepage: https://github.com/tommos0
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (org "8.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Exec typescript in org-babel code blocks.

;;; Requirements:
;; You need to install node.js and typescript to use this extension.

;;; Code:
(require 'ob)
;;(require 'ob-ref)
;;(require 'ob-comint)
;;(require 'ob-eval)

;;(require 'typescript)

(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))

;; ;; optionally declare default header arguments for this language
;; (defvar org-babel-default-header-args:tsnode '((:cmdline . "--noImplicitAny")))

;; (defun org-babel-variable-assignments:tsnode (params)
;;   "Return list of typescript statements assigning the block's variables."
;;   (mapcar (lambda (pair) (format "let %s=%s;"
;;                                  (car pair) (org-babel-typescript-var-to-typescript (cdr pair))))
;;           (org-babel--get-vars params)))

;; (defun org-babel-typescript-var-to-typescript (var)
;;   "Convert an elisp var into a string of typescript source code
;; specifying a var of the same value."
;;   (if (listp var)
;;       (concat "[" (mapconcat #'org-babel-typescript-var-to-typescript var ", ") "]")
;;     (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (let* ((temporary-file-directory (file-name-directory buffer-file-name))
         (org-babel-temporary-directory (file-name-directory buffer-file-name))
         (tmp-src-file (org-babel-temp-file "ts-src-" ".ts")))

    (with-temp-file tmp-src-file (insert body))
    (let ((result (org-babel-eval (format "ts-node --transpile-only %s"
                            (org-babel-process-file-name tmp-src-file))
                    "")))
    (delete-file tmp-src-file)
    (print result))))

(provide 'ob-tsnode)

;;; ob-tsnode.el ends here
