;;; init --- Start Up File
;;; Commentary:
;;; Code:

;;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;; Themes folder path and theme file
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'material t)

;;; Set Fonts
(set-face-attribute 'default nil
		    :family "Source Code Pro" :height 110)
(set-face-attribute 'variable-pitch nil
		    :family "Fira Sans" :height 120 :weight 'regular)

;;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode 1)
(global-hl-line-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;;; UTF-8 everywhere, forever
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; flashes the cursor's line when you scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package linum-relative
   :ensure t
   :config
   (linum-relative-mode)
   (setq linum-relative-current-symbol "")
   )

;;; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;;; Set up load path
(add-to-list 'load-path settings-dir)

;;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Don't write lock-files, I'm the only one here
(setq create-lockfiles nil)

;;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

;;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-candidate-number-list 150
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	helm-echo-input-in-header-line t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 20)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  :config
  (helm-mode 1))

;; Helm-swoop
(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'helm-swoop-without-pre-input)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-j") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-j") 'helm-next-line)

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

  ;; When doing evil-search, hand the word over to helm-swoop
  (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;;face for line numbers
  (setq helm-swoop-use-line-number-face t)
  )

;;; Hydra
(use-package hydra
  :ensure hydra
  :init
  (global-set-key
   (kbd "C-x t")
   (defhydra hydra-git-gutter (:body-pre (git-gutter+-mode 1)
					 :hint nil)
     "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
     ("j" git-gutter+-next-hunk)
     ("k" git-gutter+-previous-hunk)
     ("h" (progn (goto-char (point-min))
		 (git-gutter+-next-hunk 1)))
     ("l" (progn (goto-char (point-min))
		 (git-gutter+-previous-hunk 1)))
     ("s" git-gutter+-stage-hunk)
     ("r" git-gutter+-revert-hunk)
     ("p" git-gutter+-popup-hunk)
     ("R" git-gutter+-set-start-revision)
     ("q" nil :color blue)
     ("Q" (progn (git-gutter+-mode -1)
		 ;; git-gutter-fringe doesn't seem to
		 ;; clear the markup right away
		 (sit-for 0.1)
		 (git-gutter+-clear))
      :color blue))))

;;; Smartparens
(use-package smartparens
  :ensure t
  :init
  ;;(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  ;;(add-hook 'js2-mode-hook #'smartparens-mode)
  ;;(add-hook 'org-mode-hook #'smartparens-mode)
  ;;(add-hook 'web-mode-hook #'smartparens-mode)
  (smartparens-global-mode t))

;;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


(global-set-key (kbd "<f5>") 'revert-buffer)

;;; Try package
(use-package try
  :ensure t)

;;; Undo Tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;; OrgMode setup
(use-package org
  :ensure t
  :pin org)

(setenv "BROWSER" "chromium-browser")

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()(org-bullets-mode 1)
			     (buffer-face-mode))))

(custom-set-variables
 '(org-directory "~/Org")
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(org-confirm-babel-evaluate nil)
 '(org-src-fontify-natively t)
 )

(setq org-file-apps
          (append '(
                    ("\\.pdf\\'" . "zathura %s")
                    ("\\.x?html?\\'" . "/usr/bin/chromium-browser %s")
                    ) org-file-apps ))

(global-set-key "\C-ca" 'org-agenda)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-custom-commands
          '(("c" "Simple agenda view"
             ((agenda "")
              (alltodo "")))))

(global-set-key (kbd "C-c c") 'org-capture)

;;; Custom org defuns for capture
(defadvice org-capture-finalize 
        (after delete-capture-frame activate)  
      "Advise capture-finalize to close the frame"  
      (if (equal "capture" (frame-parameter nil 'name))  
          (delete-frame)))

    (defadvice org-capture-destroy 
        (after delete-capture-frame activate)  
      "Advise capture-destroy to close the frame"  
      (if (equal "capture" (frame-parameter nil 'name))  
          (delete-frame)))

;; Reveal.js
(use-package ox-reveal
  :ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))

;;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
	   :states '(normal visual insert emacs)
	   :prefix "SPC"
	   :non-normal-prefix "M-SPC"
	   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
	   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
	   "SPC" '(helm-M-x :which-key "M-x")
	   "<f1>" '(helm-apropos :which-key "à propos")
	   "bb" '(helm-mini :which-key "helm mini")
	   "fed" '(myinit :which-key "init file")
	   "fer" '(myinitR :which-key "init file")
	   "ff" '(helm-find-files :which-key "find files")
	   "fr" '(helm-recentf :which-key "recent files")
	   "fs" '(save-buffer :which-key "save file")
	   "fS" '(save-some-buffers :which-key "save all")
	   "xw" '(write-file :which-key "write file name")
	   "ry" '(helm-show-kill-ring :which-key "kill ring")
	   "cl" '(comment-or-uncomment-region-or-line :which-key "comment line or region")
	   ;; Projectile
	   "pf"  '(helm-projectile-find-file :which-key "find files")
	   "pF"  '(helm-projectile-find-file-dwim :which-key "find files dwim")
	   "pd"  '(helm-projectile-find-dir :which-key "find directory")
	   "pp"  '(helm-projectile-switch-project :which-key "switch project")
	   "pb"  '(helm-projectile-switch-to-buffer :which-key "switch to buffer")
	   "ph"  '(helm-projectile :which-key "helm-projectile")
	   "pr"  '(helm-projectile-recentf :which-key "recent files")
	   "pg"  '(helm-projectile-grep :which-key "grep")
	   "ps"  '(helm-multi-swoop-projectile :which-key "multi-swoop")

	   ;; Buffers
	   "bb"  '(helm-buffers-list :which-key "buffers list")
	   ;; Window
	   "wl"  '(windmove-right :which-key "move right")
	   "wh"  '(windmove-left :which-key "move left")
	   "wk"  '(windmove-up :which-key "move up")
	   "wj"  '(windmove-down :which-key "move bottom")
	   "w/"  '(split-window-right :which-key "split right")
	   "w-"  '(split-window-below :which-key "split bottom")
	   "wo"  '(other-window :which-key "other window")
	   "wx"  '(delete-window :which-key "delete window")
	   "qz"  '(delete-frame :which-key "delete frame")
	   "qq"  '(kill-emacs :which-key "quit")
	   ;; Orgmode
	   "os"  '(helm-multi-swoop-org :which-key "quit")
	   ;; Magit
	   "gs"  '(magit-status : whick-key "git status")
	   ;; Others
	   "at"  '(urxvt :which-key "open terminal")
	   ))
(use-package pdf-tools
  :ensure t)
(use-package org-pdfview
  :ensure t)

(require 'pdf-tools)
(require 'org-pdfview)

;;; Load init file
(defun myinit ()
  "Go to init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;; Reload init file

(defun myinitR ()
  "Reload to init file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;; mark and edit all copies of the marked region simultaniously.
(use-package iedit
  :ensure t)

;;; if you're windened, narrow to the region, if you're narrowed, widen
;;; bound to C-x n
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed,  it widens.  Otherwise,  it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning) (region-end)))
	((derived-mode-p 'org-mode)
	 ;; `org-edit-src-code' is not a real narrowing command.
	 ;; Remove this first conditional if you don't want it.
	 (cond ((ignore-errors (org-edit-src-code))
		(delete-other-windows))
	       ((org-at-block-p)
		(org-narrow-to-block))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;;; Helm Projectile
(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on))

;;; All The Icons
(use-package all-the-icons :ensure t)

;;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))

;;; Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (company-tng-configure-default)
  (add-to-list 'company-backends 'company-tern)
					; (add-to-list 'company-backends 'ac-js2-company)
  (global-company-mode))

(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)))

(use-package fringe-helper
  :ensure t)

(use-package git-gutter-fringe+
  :ensure t
  :init
  (global-git-gutter+-mode +1))

(use-package git-timemachine
  :ensure t)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("org" (name . "^.*org$"))

	       ("web" (or (mode . web-mode) (mode . js2-mode)))
	       ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
	       ("mu4e" (name . "\*mu4e\*"))
	       ("programming" (or
			       (mode . python-mode)
			       (mode . c++-mode)))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))
	       ))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
					;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

;;; Emmet
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Supports ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; JavaScript
(use-package js2-mode
  :ensure t
  :ensure ac-js2
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    ))

(use-package js2-refactor
  :ensure t
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-m")
    ;; eg. extract function with `C-c C-m ef`.
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package company-tern
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package tern
  :ensure tern
  :ensure tern-auto-complete
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    ))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

;;; web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (setq web-mode-engines-alist
	'(("php"    . "\\.phtml\\'")
	  ("blade"  . "\\.blade\\.")
	  ("twig"   . "\\.twig\\.")))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)) ; this fixes the quote problem I mentioned

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust indent."
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Live dev setup  ;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (use-package simple-httpd
;;   :ensure t
;;   :config
;;   (setq httpd-root "~/LocalDev/")
;;   (httpd-start))

;; (use-package skewer-mode
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'skewer-mode)
;;   (add-hook 'css-mode-hook 'skewer-css-mode)
;;   (add-hook 'html-mode-hook 'skewer-html-mode)
;;   (add-hook 'web-mode-hook 'skewer-html-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (linum-relative org-pdfview pdf-tools iedit magit hungry-delete beacon all-the-icons projectile general which-key helm evil-escape evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Iosevka"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 1.5))))
 '(font-lock-constant-face ((t (:foreground "#C792EA"))))
 '(font-lock-keyword-face ((t (:foreground "#2BA3FF" :slant italic))))
 '(font-lock-preprocessor-face ((t (:inherit bold :foreground "#2BA3FF" :slant italic :weight normal))))
 '(font-lock-string-face ((t (:foreground "#C3E88D"))))
 '(font-lock-type-face ((t (:foreground "#FFCB6B"))))
 '(font-lock-variable-name-face ((t (:foreground "#FF5370"))))
 '(helm-rg-active-arg-face ((t (:foreground "LightGreen"))))
 '(helm-rg-file-match-face ((t (:foreground "LightGreen" :underline t))))
 '(helm-rg-preview-line-highlight ((t (:background "LightGreen" :foreground "black"))))
 '(linum ((t (:inherit (shadow default) :background "#212121" :foreground "#586e75"))))
 '(mode-line ((t (:background "#191919" :box nil))))
 '(mode-line-inactive ((t (:background "#282828" :foreground "#5B6268" :box nil))))
 '(term ((t (:foreground "#fafafa")))))
(set-face-attribute 'fringe nil :background "#212121")

(provide 'init)
;;; init.el ends here
