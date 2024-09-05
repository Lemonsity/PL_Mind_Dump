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
  :bind (("C-s" . swiper)) ;; Swiper gives list of completion suggestion
  :config
  (ivy-mode 1))

;; counsel will give us better completion when:
;; - Finding files
;; - Switching Buffer
(use-package counsel
  :diminish
  :bind (
	 ("M-x" . 'counsel-M-x)) ;; Default to counsel's interactive command
  :config
  (counsel-mode 1))

;; ========= Flymake =========
;; Flymake comes with a backend called ['flymake-proc-legacy-flymake]
;; However this is deprecated.
;; TODO: find a way to perma disable it
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
	 
	  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(dired-create-destination-dirs 'ask)
 '(display-line-numbers 'relative)
 '(flymake-mode-line-lighter "FM")
 '(package-selected-packages
   '(company-coq-mode diminish counsel ivy command-log-mode company-coq racket-mode company rust-mode lsp-mode proof-general)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ========== Font Setup ==========

;; Set up on agda mode
;; https://www.emacswiki.org/emacs/FacesPerBuffer#toc3
(defun agda-buffer-face-mode ()
   "Set font to a variable width (proportional) fonts in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "mononoki"
					 :height 120
					 :width normal
					 :weight normal))
   (buffer-face-mode))
(add-hook 'agda2-mode-hook 'agda-buffer-face-mode)

;; default to mononoki
;; (set-face-attribute 'default nil
;;                     :family "mononoki"
;;                     :height 120
;;                     :weight 'normal
;;                     :width  'normal)


;; ========== Languages Setup ==========

;; lsp-mode Setup
;; (require 'lsp-mode)
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (haskell-mode . lsp-deferred)
	 (haskell-literate-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands (lsp lsp-deferred))

;; ---------- Haskell Setup ----------
(use-package lsp-haskell)
(use-package haskell-mode)
;; (require 'lsp-haskell)
;; (require 'haskell-mode)
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;; ---------- Coq Setup ----------

;; Load company-coq when opening Coq files
(use-package company-coq
  :hook ((coq-mode . company-coq-mode)))
;; (add-hook 'coq-mode-hook #'company-coq-mode)

;; ---------- Rust Setup ----------
(use-package rust-mode)
;; Auto start rust-mode
;; (require 'rust-mode)
;; lsp-mode Rust integration
;; (add-hook 'rust-mode-hook #'lsp)


;; ---------- Racket Setup ----------
(use-package racket-mode
  :init (setq auto-mode-alist
	      (append
	       '(("\\.rkt\\'" . racket-mode))
	       auto-mode-alist))
  :hook ((racket-mode . racket-unicode-input-method-enable)
	 (racket-repl-mode . racket-unicode-input-method-enable)))

;; (setq auto-mode-alist
;;    (append
;;      '(("\\.rkt\\'" . racket-mode))
;;      auto-mode-alist))
;; (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

;; ---------- Agda Setup ----------
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; ========== Productivity Setup ==========
(add-hook 'org-mode-hook 'visual-line-mode)
