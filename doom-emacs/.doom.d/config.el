;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
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

(load (expand-file-name "./private.el"))

(push '("\\.tsx\\'" . web-mode) auto-mode-alist)

(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'lsp-deferred)

(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
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