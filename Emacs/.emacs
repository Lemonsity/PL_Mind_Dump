;; Init Package Source
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ========= Setup use-package =========
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; When eval a use-package form,
;; Then it guarantee download the package
(setq use-package-always-ensure t)

;; Enable to use diminish
;; Add the line [:diminish] to a [use-package]
;; call to not show the mode in the mode-line
(use-package diminish :ensure t)

;; Not necessary, but now we can show keystroke
(use-package command-log-mode)


;; ========= Ivy / Counsel Completion =========
;; Ivy completion setup
(use-package ivy
  :diminish
  :bind (
	 ("C-s" . swiper))
  :config
  (ivy-mode 1))

;; counsel will give us better completion when:
;; - Finding files
;; - Switching Buffer
(use-package counsel
  :diminish
  :bind (
	 ("M-x" . counsel-M-x)) ;; Default to counsel's interactive command
  :config
  (counsel-mode 1))
	 
	  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-line-numbers 'relative)
 '(package-selected-packages
   '(diminish counsel ivy command-log-mode company-coq racket-mode company rust-mode lsp-mode proof-general)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ========== Font Setup ==========
;; default to mononoki
(set-face-attribute 'default nil
                    :family "mononoki"
                    :height 120
                    :weight 'normal
                    :width  'normal)

;; ========== Languages Setup ==========

;; lsp-mode Setup
(require 'lsp-mode)

;; ---------- Haskell Setup ----------
(require 'lsp-haskell)
(require 'haskell-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; ---------- Coq Setup ----------

;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

;; ---------- Rust Setup ----------
;; Auto start rust-mode
(require 'rust-mode)
;; lsp-mode Rust integration
(add-hook 'rust-mode-hook #'lsp)


;; Racket Setup
(setq auto-mode-alist
   (append
     '(("\\.rkt\\'" . racket-mode))
     auto-mode-alist))
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
       
;; Agda Setup
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

