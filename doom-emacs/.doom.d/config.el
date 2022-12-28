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

(setq doom-font (font-spec :family "Source Code Pro" :size 14 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

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
;; (remove-hook 'typescript-mode-hook #'prettier-js-mode)
;; (remove-hook 'web-mode-hook #'prettier-js-mode)
(add-hook 'graphql-mode-hook #'lsp-deferred)

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

  (push '("typescript/deno" . ((:command . "deno")
                               (:exec . ("%c run --allow-all %s"))
                               (:tempfile . nil)))
        quickrun--language-alist)

  ;; (quickrun-set-default "typescript" "typescript/ts-node")
  (quickrun-set-default "typescript" "typescript/deno")
  )

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
(setenv "EDITOR" "emacsclient")
(setenv "DENO_INSTALL" "/home/tomk/.deno")

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

;; (defun my-remap-hl-line ()
;;   "Remap hl-line face."
;;   (face-remap-add-relative 'hl-line `(:background ,(face-background 'default))))

;; (with-eval-after-load 'treemacs
;;   (add-hook 'treemacs-mode-hook #'my-remap-hl-line))

(after! tramp
  (push
   (cons
    "k8s"
    '((tramp-login-program "kubectl")
      (tramp-login-args (("exec")
                         ("-it")
                         ("%h")
                         ("--")
                         ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i")
                               ("-c"))))
   tramp-methods))

(after! eshell (setq eshell-history-size 1024))

(after! magit (map! :mode magit-status-mode
                    "M-RET" #'magit-diff-visit-file-other-window))

(after! quickrun (add-hook 'quickrun--mode-hook (lambda ()
                                                  (display-line-numbers-mode t)
                                                  (setq hide-mode-line-mode nil))))

(defconst headphone-bluetooth-profile-a2dp "a2dp_sink")
(defconst headphone-bluetooth-profile-hfp "handsfree_head_unit")

(defun switch-headphone-current-profile ()
  "returns (set-card-profile <card_id> <profile>)"
  (split-string
   (shell-command-to-string "pacmd dump | grep bluez | grep profile")))

(defun switch-headphone-profile ()
  (interactive)
  (let* ((current (switch-headphone-current-profile))
         (card_id (cadr current))
         (profile (caddr current))
         (next_profile (if (string= profile headphone-bluetooth-profile-a2dp)
                           headphone-bluetooth-profile-hfp
                           headphone-bluetooth-profile-a2dp)))
    (shell-command (format "pacmd set-card-profile %s %s"
                           card_id
                           next_profile))
    (message "Switched to %s" next_profile)))

(add-hook 'after-init-hook (lambda () (auto-dim-other-buffers-mode t)))

(setenv "PATH" (concat (getenv "PATH") ":/home/tomk/.nvm/versions/node/v16.13.0/bin:/home/tomk/.yarn/bin"))
(push "/home/tomk/.nvm/versions/node/v16.13.0/bin" exec-path)
(push "/home/tomk/.yarn/bin" exec-path)

(after! forge (add-to-list 'forge-alist '("ahold"
                                          "api.github.com"
                                          "github.com"
                                          forge-github-repository)))

(setq auth-sources '("~/.authinfo"))

;; (transient-define-infix forge-forge.remote ()
;;   "Change the local value of the `forge.remote' Git variable."
;;   :class 'magit--git-variable:choices
;;   :variable "forge.remote"
;;   :choices #'magit-list-remotes
;;   :default "origin")

(after! eshell
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))

(defun open-in-vscode ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (number-to-string (line-number-at-pos)))
         (col (number-to-string (+ 1 (current-column))))
         (filestr (concat file ":" line ":" col)))
    (start-process "code" nil "/usr/bin/code" "--goto" filestr)))

(setq evil-escape-key-sequence "qt")

(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

(add-to-list 'org-src-lang-modes (cons "tsx" 'typescript))

(add-hook! +dap-running-session-mode
  (set-window-buffer nil (current-buffer)))

(advice-add 'hide-mode-line-mode :around (lambda (orig &optional args) nil))

(defun company-copilot-accept ()
  "Accept copilot suggestion if company is not active"
  (interactive)
  (unless (company--active-p)
    (copilot-accept-completion)))

(map! :after copilot
      :map copilot-mode-map
      :i "TAB" #'company-copilot-accept
      :leader
      (:prefix ("l" . "Copilot")
       :desc "Clear" "c"           #'copilot-clear-overlay
       :desc "Complete" "l"        #'copilot-complete
       :desc "Accept" "RET"        #'copilot-accept-completion
       :desc "Accept (word)" "TAB" #'copilot-accept-completion-by-word
       :desc "Next" "n"            #'copilot-next-completion
       :desc "Previous" "p"        #'copilot-previous-completion))

(add-hook! prog-mode 'copilot-mode)

;; set modeline background face
(set-face-background 'mode-line "#2e3440")

(defun clone-buffer-fundamental ()
  "Clone the current buffer into a new buffer with fundamental mode."
  (interactive)
  (let ((content (buffer-string)))
    (with-current-buffer (generate-new-buffer (concat "Copy of " (buffer-name)))
      (insert content)
      (switch-to-buffer (current-buffer)))))

(setq lsp-headerline-breadcrumb-icons-enable nil)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
