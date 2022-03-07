(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(js-comint plantuml-mode graphviz-dot-mode smudge tree-sitter-langs tree-sitter))
 '(warning-suppress-log-types
   '((vterm-mode-hook)
     (doom-switch-buffer-hook)
     (iedit)
     (lsp-mode)
     (lsp-mode)
     (lsp-mode)))
 '(warning-suppress-types
   '((vterm-mode-hook)
     (doom-switch-buffer-hook)
     (iedit)
     (lsp-mode)
     (lsp-mode)
     (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(map! :leader
      :desc "Close buffer and window"
      "b q" (lambda ()
              (interactive)
              (kill-current-buffer)
              (delete-window)))

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

