;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tom Klaver"
      user-mail-address "tomklav@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Hack" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; add several elements at once (may create double entries,
;; but that would not hurt)

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))

(push '("\\.tsx\\'" . web-mode) auto-mode-alist)

(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'lsp-deferred)

(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)

(after! eshell
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode))

(add-hook 'vterm-mode-hook
          (lambda () (display-line-numbers-mode t)))

(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

(push (expand-file-name "~/.yarn/bin") exec-path)

(after! quickrun
        (push '("typescript/ts-node" . ((:command . "ts-node")
                                        (:exec . ("%c --transpile-only -r tsconfig-paths/register %s"))
                                        (:tempfile . nil)))
                quickrun--language-alist)

        (quickrun-set-default "typescript" "typescript/ts-node"))

(defun run-npm-command ()
  (interactive)
  (let* (
         (default-directory (file-name-as-directory (locate-dominating-file default-directory
                                                                            "package.json")))
         (package-json-file (concat default-directory "package.json"))
         (package-json (json-read-file package-json-file))
         (scripts (mapcar 'car (cdr (assq 'scripts package-json))))
         (script (completing-read "Select script: " scripts))
         (buffer-name (concat "npm " script)))
    (async-shell-command (concat "yarn " script)
                         (get-buffer-create buffer-name))))

(defun org-babel-edit-prep:typescript (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))


(require 'ob-tsnode)
(require 'private)

(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))

(defun org-babel-execute:typescript (body params)
  (let* ((temporary-file-directory (file-name-directory buffer-file-name))
         (org-babel-temporary-directory (file-name-directory buffer-file-name))
         (tmp-src-file (org-babel-temp-file "ts-src-" ".ts")))

    (with-temp-file tmp-src-file (insert body))
    (let ((result (org-babel-eval (format "ts-node --transpile-only %s"
                            (org-babel-process-file-name tmp-src-file))
                    "")))
    (delete-file tmp-src-file)
    (print result))))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps '("ahold" . "github"))
  (advice-add 'browse-at-remote--format-region-url-as-github
            :around
            (lambda (orig-fun repo-url &rest args)
              (let ((repo-url (string-replace "https://ahold" "https://github.com" repo-url)))
                (push repo-url args)
                (apply orig-fun args))))
)

(setenv "KUBECONFIG" "/home/tomk/.kube/ah-tst.yaml")

;; (require 'dap-node)

(defun dap-tsnode-current ()
       (interactive)
        (dap-debug
        (list :name "TS Index"
                :type "node"
                :request "launch"
                :args (buffer-file-name)
                :runtimeArgs ["--nolazy" "-r" "ts-node/register"]
                :sourceMaps t
                :cwd default-directory
                :protocol "inspector")))

(defun dap-attach()
       (interactive)
        (dap-debug
        (list :name "TS Index"
                :type "node"
                :request "attach"
                :port 9229
                :program "__ignored"
                ;:runtimeArgs ["--nolazy" "-r" "ts-node/register"]
                :sourceMaps t
                :cwd default-directory
                :protocol "inspector"
                :skipFiles [ "<node_internals>/**" ]
 )))

(scroll-bar-mode t)
(setq scroll-bar-adjust-thumb-portion nil)
(setq window-divider-default-right-width 12)
(global-undo-tree-mode)
(setq vterm-buffer-name-string "vterm %s")

(defun my-remap-hl-line ()
  "Remap hl-line face."
  (face-remap-add-relative 'hl-line `(:background ,(face-background 'default))))

(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook #'my-remap-hl-line))

(after! tramp
  (push
   (cons
    "k8s"
    '((tramp-login-program "kubectl")
      (tramp-login-args (("exec") ("-it") ("%h") ("--") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods))
