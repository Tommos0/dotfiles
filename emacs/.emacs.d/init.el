(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode +1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(set-face-attribute 'default nil 
		    :font "Hack" 
		    :height 120)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package) 
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package 
  monokai-theme)

(use-package 
  darcula-theme)

(use-package 
  zenburn-theme) 

(use-package 
  evil 
  :init (setq evil-want-integration 1) 
  (setq evil-want-keybinding nil) 
  (setq evil-want-C-u-scroll 1) 
  :config (evil-mode 1))

(use-package 
  evil-collection 
  :after evil 
  :config (evil-collection-init))

(use-package 
  swiper)

(use-package 
  command-log-mode)

(use-package 
  ivy 

  :diminish 
  :bind (("C-s" . swiper) :map ivy-minibuffer-map ("TAB" . ivy-alt-done) 
	 ("C-l" . ivy-alt-done) 
	 ("C-j" . ivy-next-line) 
	 ("C-k" . ivy-previous-line) 
	 :map ivy-switch-buffer-map ("C-k" . ivy-previous-line) 
	 ("C-j" . ivy-next-line) 
	 ("C-l" . ivy-done) 
	 ("C-d" . ivy-switch-buffer-kill) 
	 :map ivy-reverse-i-search-map ("C-k" . ivy-previous-line) 
	 ("C-d" . ivy-reverse-i-search-kill)) 
  :config (ivy-mode 1))

(use-package 
  ivy-rich 
  :init (ivy-rich-mode 1))

(use-package 
  counsel 
  :bind (("M-x" . counsel-M-x) 
	 ("C-x b" . counsel-ibuffer) 
	 ("C-x C-f" . counsel-find-file) 
	 :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history)) 
  :config (setq ivy-initial-inputs-alist nil))

;; Always focus help-window
(setq help-window-select t)

(use-package 
  elisp-format 
;  :bind (("C-l" . elisp-format-buffer)) 
  :custom (elisp-format-indent-comment nil))

(use-package 
  doom-themes)

(use-package 
  doom-modeline 
  :ensure t 
  :init (doom-modeline-mode 1) 
  :custom ((doom-modeline-height 15)))

(load-theme 'zenburn t)

(global-command-log-mode 1)
;;(clm/toggle-command-log-buffer)

; Shows column number in modeline.
(column-number-mode)

(show-paren-mode 1)

(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook

;;eshell-mode-hook)
;;(add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; disabled line wrapping
(setq-default truncate-lines t)

;; Colored matching of parenthesis
(use-package rainbow-delimiters 
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package 
which-key 
:init (which-key-mode) 
:diminish which-key-mode 
:config (setq which-key-idle-delay 1))

(use-package 
helpful 
:custom (counsel-describe-function-function #'helpful-callable) 
(counsel-describe-variable-function #'helpful-variable) 
:bind ([remap describe-function] . counsel-describe-function) 
([remap describe-command] . helpful-command) 
([remap describe-variable] . counsel-describe-variable) 
([remap describe-key] . helpful-key))

(use-package magit)
(use-package projectile
  :diminish projectile-mode
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("/p" "~/git")))

(use-package tide)
(use-package elixir-mode)
(use-package alchemist)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "ff195a1c80d74fdfece0993e5eb763be25163ab8981175899d19c3023dc90e23" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "6fa878330926f7b021c214be11f15d3dc36b2f5969d8111fde27ba00ac016bbd" "22c8110ceeade36347426926ae4adac25067743f49354fd04d9d49c051cb5149" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" default))
 '(package-selected-packages
   '(spacemacs-themes spacemacs-theme zenburn-theme darcula-theme prettier docker docker-compose-mode yaml-mode yasnippet-snippets yasnippet rustic cargo-mode lsp-mode csv-mode hydra exwm sly flycheck-rust rust-mode slime-volleyball markdown-mode markdown evil-commentary company-box vshell company-quickhelp vterm alchemist elixir-mode company tide projectile magit elisp-format doom-themes helpful rainbow-delimiters doom-modeline evil-collections use-package swiper monokai-theme evil-collection command-log-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(use-package company)
(use-package web-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (local-set-key (kbd "C-<return>") 'tide-fix) 
  (local-set-key (kbd "M-<return>") 'tide-project-errors) 
  (auto-save-mode -1)
  (auto-save-visited-mode +1)
  (setq auto-save-visited-interval 1)
)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(setq rcirc-server-alist '(
	("irc.libera.chat" :channels ("#rcirc"))
    ))

(use-package vterm)

(defun testje () (interactive) (save-buffer) (alchemist-iex-compile-this-buffer))

(add-hook 'elixir-mode-hook
  (lambda ()
    (local-set-key (kbd "<C-return>") (lambda () (interactive) (testje)))))


(use-package evil-commentary)

(global-set-key (kbd "C-/") 'evil-commentary-line) 

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package markdown-mode)


(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package rust-mode)
(use-package flycheck-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package sly)


(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))


(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-replace t)
  "https://www.google.com"

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  ;; (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
)

(setenv "_JAVA_AWT_WM_NONREPARENTING" "1")
(add-hook 'after-init-hook 'global-company-mode)

(use-package hydra)

(defun kwin-replace () (call-process "kwin_x11" nil 0 nil "--replace"))

(defhydra hydra-windows (global-map "s-w")
  "windows"
  ("0" (exwm-workspace-switch-create 0) "WS0")
  ("1" (exwm-workspace-switch-create 1) "WS1")
  ("2" (exwm-workspace-switch-create 2) "WS2")
  ("3" (exwm-workspace-switch-create 3) "WS3")
  ("4" (exwm-workspace-switch-create 4) "WS4")
  ("5" (exwm-workspace-switch-create 5) "WS5")
  ("6" (exwm-workspace-switch-create 6) "WS6")
  ("7" (exwm-workspace-switch-create 7) "WS7")
  ("8" (exwm-workspace-switch-create 8) "WS8")
  ("9" (exwm-workspace-switch-create 9) "WS9")
  (")" (exwm-workspace-move-window 0) "M0")
  ("!" (exwm-workspace-move-window 1) "M1")
  ("@" (exwm-workspace-move-window 2) "M2")
  ("#" (exwm-workspace-move-window 3) "M3")
  ("$" (exwm-workspace-move-window 4) "M4")
  ("%" (exwm-workspace-move-window 5) "M5")
  ("^" (exwm-workspace-move-window 6) "M6")
  ("&" (exwm-workspace-move-window 7) "M7")
  ("*" (exwm-workspace-move-window 8) "M8")
  ("(" (exwm-workspace-move-window 9) "M9")

  ("h" (evil-window-left 1) "h")
  ("j" (evil-window-down 1) "j")
  ("k" (evil-window-up 1) "k")
  ("l" (evil-window-right 1) "l")

  ("H" (evil-window-move-far-left) "H")
  ("J" (evil-window-move-very-bottom) "J")
  ("K" (evil-window-move-very-top) "K")
  ("L" (evil-window-move-far-right) "L")

  ("s" (evil-window-split) "s")
  ("v" (evil-window-vsplit) "v")

  ("-" (evil-window-decrease-height +1) "-")
  ("=" (evil-window-increase-height +1) "=")
  ("<" (evil-window-decrease-width +1) "<")
  (">" (evil-window-increase-width +1) ">")

  ("o" (delete-other-windows) "o")
  ("c" (evil-window-delete) "c")
  ("x" (kill-current-buffer) "x")
  ("b" (ibuffer) "b" :exit t)
  ("r" (counsel-linux-app) "r" :exit t)

  ("C-x" (kwin-replace) "C-x" :exit t)
  ("C-i" (exwm-init) "C-i" :exit t)
  ("ESC" (ignore) "ESC" :exit t)

)

(push ?\s-w exwm-input-prefix-keys)

(use-package csv-mode)

(use-package lsp-mode)
(use-package cargo-mode)
(use-package rustic)
(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package yaml-mode
  :ensure t)
(use-package docker-compose-mode
  :ensure t)
(use-package docker
  :ensure t)
(use-package tramp
  :ensure t)
