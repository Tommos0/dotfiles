(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-hl-line-mode t)
 '(lsp-eslint-validate '("svelte" "graphql"))
 '(package-selected-packages
   '(keycast counsel-ffdata kubernetes-tramp js-comint plantuml-mode graphviz-dot-mode smudge tree-sitter-langs tree-sitter))
 '(warning-suppress-log-types
   '((doom-init-ui-hook)
     (doom-init-ui-hook)
     (vterm-mode-hook)
     (doom-switch-buffer-hook)
     (iedit)))
 '(warning-suppress-types
   '((doom-init-ui-hook)
     (vterm-mode-hook)
     (doom-switch-buffer-hook)
     (iedit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#222228"))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(hl-line ((t (:extend t :background "gray28"))))
 '(line-number ((t (:inherit default :foreground "gray" :strike-through nil :underline nil :slant normal :weight normal))))
 '(magit-section-highlight ((t (:inherit hl-line :background "gray27"))))
 '(solaire-default-face ((t (:inherit default :background "#282c34"))))
 '(solaire-hl-line-face ((t (:inherit hl-line :extend t :background "gray28")))))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(map! :leader
      :desc "Toggle maximize buffer"
      "w f" #'toggle-maximize-buffer)

;; (defhydra indium-debug-hydra ()
;;   "indium debug"
;;       ("SPC" indium-debugger-step-over)
;;       ("i"   indium-debugger-step-into)
;;       ("o"   indium-debugger-step-out)
;;       ("c"   indium-debugger-resume)
;;       ("l"   indium-debugger-locals)
;;       ("s"   indium-debugger-stack-frames)
;;       ("q"   indium-debugger-resume)
;;       ("h"   indium-debugger-here)
;;       ("e"   indium-debugger-evaluate)
;;       ("n"   indium-debugger-next-frame)
;;       ("p"   indium-debugger-previous-frame))

;; (map! :localleader
;;       "m" #'indium-debug-hydra/body)

(map! :desc "Comment line"
      "C-/" #'evil-commentary-line)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun profile-test ()
  (interactive)
  (doom/toggle-profiler)
  (find-file "/home/tomk/git/ah/ah-graphql/graphql-server/src/helpers/error/format-error-output.ts")
  (run-at-time 2.0 nil #'doom/toggle-profiler))


(map! :desc "Jest test"
      :leader "c T" (lambda (arg) (interactive "P") (save-buffer) (jest-ts-run-test-at-point arg)))

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(after! eshell
  (setq eshell-save-history-on-exit nil)
  (add-hook 'eshell-pre-command-hook #'eshell-append-history))
(put 'erase-buffer 'disabled nil)

(defun dired-jump-and-close ()
  (interactive)
  (let ((old-buffer (current-buffer)))
    (dired-jump)
    (kill-buffer old-buffer)))

(map! :leader
      :desc "Close buffer and window"
      "b q" #'kill-buffer-and-window)


(map! :desc "Dired jump and close current buffer"
      :leader "o =" #'dired-jump-and-close)

(map! :after company
      :map company-active-map
      "RET" nil
      "<return>" nil
      "TAB" #'company-complete-selection
      "<tab>" #'company-complete-selection)
