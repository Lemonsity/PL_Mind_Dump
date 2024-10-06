;; ====================== Startup Setup ======================
; (tool-bar-mode -1)
; (menu-bar-mode -1)
(setq visible-bell t)

;; ====================== Init Package Source ======================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ====================== Setup use-package ======================
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

;; ====================== Ivy / Counsel Completion ======================
;; Ivy completion setup
(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)) ;; Swiper gives list of completion suggestion
  :config
  (ivy-mode 1))

;; counsel will give us better completion when:
;; - Finding files
;; - Switching Buffer
(use-package counsel
  :diminish
  :bind
  (("M-x" . 'counsel-M-x)) ;; Default to counsel's interactive command
  :config
  (counsel-mode 1))

;; ====================== Flymake ======================
;; Flymake comes with a backend called ['flymake-proc-legacy-flymake]
;; However this is deprecated.
;; TODO: find a way to perma disable it
;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;; =====================================================
;; Flymake is so garbage, use Flycheck instead
;; =====================================================
	  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist nil)
 '(column-number-mode t)
 '(dired-create-destination-dirs 'ask)
 '(display-line-numbers t)
 '(org-agenda-files
   '())
 '(package-selected-packages
   '(flycheck visual-fill-column visual-line org-indent org-tree-slide auctex company-coq-mode diminish counsel ivy command-log-mode company-coq racket-mode company rust-mode lsp-mode proof-general))
 '(proof-splash-enable nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ====================== Font Setup ======================

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

(set-face-attribute 'default nil
		    :family "Iosevka"
		    :width 'expanded
		    :overline nil)

;; ====================== Languages Setup ======================

;; ---------------------- Multi-language Tools ----------------------
;; Flycheck Syntax Checking
(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  )

;; lsp-mode Setup
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp)
   (rust-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)) ;; if you want which-key integration
  :commands
  (lsp lsp-deferred))

(use-package company
  :diminish
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))

;; ---------------------- Haskell Setup ----------------------
(use-package lsp-haskell)

(use-package haskell-mode
  :hook
  ((haskell-mode . interactive-haskell-mode)))

;; (require 'lsp-haskell)
;; (require 'haskell-mode)
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;; ---------------------- Coq Setup ----------------------

;; Load company-coq when opening Coq files
(use-package company-coq
  :diminish
  :hook
  ((coq-mode . company-coq-mode)))
;; (add-hook 'coq-mode-hook #'company-coq-mode)

;; ---------------------- Rust Setup ----------------------
(use-package rust-mode)
;; Auto start rust-mode
;; (require 'rust-mode)
;; lsp-mode Rust integration
;; (add-hook 'rust-mode-hook #'lsp)


;; ---------------------- Racket Setup ----------------------
(use-package racket-mode
  :init
  (setq auto-mode-alist
	(append
	 '(("\\.rkt\\'" . racket-mode))
	 auto-mode-alist))
  :hook
  ((racket-mode . racket-unicode-input-method-enable)
   (racket-repl-mode . racket-unicode-input-method-enable)))

;; (setq auto-mode-alist
;;    (append
;;      '(("\\.rkt\\'" . racket-mode))
;;      auto-mode-alist))
;; (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

;; ---------------------- Agda Setup ----------------------
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; ---------------------- TeX Setup ----------------------
(use-package tex
  :ensure auctex
  :hook
  ((LaTeX-mode . visual-line-mode))
  :config (setq TeX-auto-save t
		TeX-parse-self t))

;; (use-package latex-preview-pane
;;   :diminish
;;   :hook ((LaTeX-mode . latex-preview-pane-mode)))

;; ====================== Productivity Setup ======================

;; ---------------------- Org Setup ----------------------
(defun lemon/org-mode-setup ()
  (visual-line-mode 1)
  (org-indent-mode)
  )

(defun lemon/org-font-setup ()
  (set-face-attribute 'org-code nil
		      :foreground "black"
		      :background "LightGray")
  (set-face-attribute 'org-block nil
		      :foreground "black"
		      :background "LightGray")
  (set-face-attribute 'org-meta-line nil
		      ;; :background "#EAEAFF"
		      :foreground "#008ED1")
  (set-face-attribute 'org-table nil
		      :background "#d6e4fc")
  (set-face-attribute 'org-quote nil
		      :foreground "black"
		      :background "AntiqueWhite1")
  (set-face-attribute 'org-block-begin-line nil ;; <--- end line inherit this
		      :inherit 'default
		      :foreground "Gray")
  (set-face-attribute 'org-drawer nil
		      :foreground "Gray")
		      
  (setq org-fontify-quote-and-verse-blocks t))

(defun lemon/org-appearance-setup ()
  (setq org-adapt-indentation t)
  (setq org-hide-leading-stars t)
  (setq org-pretty-entities t)
  (setq org-fontify-emphasized-text t)
  (setq org-hide-emphasis-markers t)
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (setq org-highlight-latex-and-related '(latex))
  (setq org-tags-column -100)
  )

(defun lemon/org-todo-setup ()
  ;; Add keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IDEA(i)" "PROG(p)" "READ(r)" "CHECK(c)" "|" "DONE(d)")
	  (sequence "|" "BACKLOG(b)" "CANCELED(x)")))
  ;; Customize keywords
  (setq org-todo-keyword-faces
	'(("IDEA" :inherit 'org-todo :foreground "gold2")
	  ("PROG" :inherit 'org-todo :foreground "gold2")
	  ("READ" :inherit 'org-todo :foreground "gold2")
	  ("CHECK" :inherit 'org-todo :foreground "gold2")
	  ("BACKLOG" :inherit 'org-todo :foreground "SteelBlue")
	  ("CANCELED" :inherit 'org-todo :foreground "CadetBlue")))
  ;; Log time on DONE
  (setq org-log-done 'time)
  )


(defun lemon/org-agenda-setup ()
  (setq org-agenda-span 14))

(use-package org
  :hook
  ((org-mode . lemon/org-mode-setup))
  :config
  (lemon/org-appearance-setup)
  (lemon/org-font-setup)
  (lemon/org-todo-setup)
  (lemon/org-agenda-setup))
  
;; Use Org mode as slide show
(use-package org-tree-slide
  :custom (org-image-actual-width nil))

(defun lemon/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . lemon/org-mode-visual-fill))

;; ====================== Misc ======================

;; This activate a shortcut in Dired mode
;; You can now press [a] to replace current buffer
;; by the directory hovering over
(put 'dired-find-alternate-file 'disabled nil)

