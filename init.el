;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("80c5aaa09dfdf6ecd7d70041725563e4c4807a89b5c1ce4f47f236174a7a64c6"
     "ccc95dff5bbc356d849ea81f7c24c54cb5bc3bae7c297dd158a08116475fd1f0"
     "7afef88ab9fd8f09161dfaa5c513cefb2b4517e83260dd9749133668cf4bfecc"
     "b7e9b676351da35874b35b9f1564296474be0d1587fcc2d2eeabc45853dd0671"
     "d015f7295925398145c42285e2ea4bb438d449d36e2b10ba0650024862ec93a8"
     "62097dbc0924e2b42f9eaeead73fc2c12cf7b579c214bbb5e755d4f2391ffc2f"
     "bc2936e8cd9c3e67623e76672ddf53411e60723e2ed0cad8b4ca59b5a2d80bbf"
     "6a784261d7e9bb651d6b4867b8f3c06ba7fa255a869a0a44ac35290e94776d38"
     "f4157511d5d4a31766a01ce6aeef7329a39afbfa61f6f6a96a29bb97dc9e00b1"
     "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577"
     "ccdc42b444da0b62c25850da75f59186319ee22ddfd153ffc9f7eb4e59652fc9"
     "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5"
     default))
 '(org-safe-remote-resources '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(package-selected-packages
   '(avy buffer-move consult-dir copilot dumb-jump embark-consult
         expand-region expreg fancy-dabbrev gcmh gptel
         highlight-numbers marginalia markdown-mode modus-themes
         move-text multiple-cursors nano-modeline orderless projectile
         rainbow-delimiters rg smartscan vertico visual-replace vterm
         vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package emacs
  :init

  (setq-default indent-tabs-mode nil)
  (setq lexical-binding t)

  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil)

  ;; theme
  (setq custom--inhibit-theme-enable nil)

  (setq grep-use-null-device nil)
  ;;(setq display-line-numbers-type 'relative)
  ;;(setq linum-format " %5i ")

  (setq split-width-threshold (- (window-width) 10))
  (setq split-height-threshold nil)

  ;;(setq split-width-threshold nil)
  ;; (setq split-height-threshold 200)

  (setq window-divider-default-places 'right-only)
  ;;(setq window-divider-default-right-width 12)

  (setq scroll-step 3)

  ;; always kill *compilation* buffer before new *compilation* start
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t)

  (setq switch-to-buffer-obey-display-actions t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers nil)

  (setq process-connection-type nil)
  (setq duplicate-line-final-position 1)

  :bind (("C-z"       . undo)
	 ("C-SPC"     . push-mark-no-activate)
	 ;;("M-\""       . jump-to-mark)
	 ("M-`"       . jump-to-mark)
	 ("M-o"       . other-window)
	 ("M-w"       . copy-region-as-kill)
	 ("C-a"       . back-to-indentation)
	 ("M-m"       . beginning-of-visual-line)
	 ("M-h"       . backward-kill-word)
	 ("C-h"       . backward-delete-char)
	 ("C-\\"      . help-command)
	 ("M-<right>" . enlarge-window-horizontally)
	 ("M-<left>"  . shrink-window-horizontally)
	 ("C-x C-z"   . grep-fd)
	 ("<M-f4>"    . save-buffers-kill-terminal)
	 ("C-M-c"     . scroll-other-window-down)
	 ("C-x C-b"   . ibuffer)
	 ("<f1>"      . build)
         ("<f2>"      . run)
         ("<f3>"      . generate)
	 ("<escape>"  . window-toggle-side-windows)
	 ;;("C-,"       . mark-sexp)
	 ("C-x j"     . previous-buffer)
	 ("C-x l"     . next-buffer)
	 ("M-n"       . forward-paragraph)
	 ("M-p"       . backward-paragraph)
	 ("C-/"       . duplicate-line)
         ("C-x C-x"   . (lambda () (interactive) (if (not (use-region-p)) (exchange-point-and-mark)) (exchange-point-and-mark)))
         (:map ctl-x-4-map
               ("t" . toggle-window-split)))

  :config

  ;; Use C-\ as help key instead of C-h.
  (setq help-char ?\C-\\)
  (setq help-event-list (list help-char))

  (my-load-all-in-directory '"~/.emacs.d/others/")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  (if (running-in-wsl-p)
      (progn
	(message "Running in WSL!")
	(setq default-directory "~/nu/")
	(add-hook 'comint-output-filter-functions 'my-comint-path-rewrite))
    (setq default-directory "C:/"))

  (add-hook 'emacs-lisp-mode-hook '(lambda ()
				     (local-set-key (kbd "<tab>")   #'fancy-dabbrev-expand)
			             (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
                                     (local-set-key (kbd "C-c C-c") #'comment-region)
			             (local-set-key (kbd "C-c C-v") #'uncomment-region)))

  (add-hook 'prog-mode-hook '(lambda ()
			       (local-set-key (kbd "<tab>") #'fancy-dabbrev-expand)
                               (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
			       (local-set-key (kbd "C-c C-c") #'comment-region)
			       (local-set-key (kbd "C-c C-v") #'uncomment-region)))

  (add-hook 'asm-mode-hook '(lambda ()
			      (local-unset-key (kbd ";"))))

  (load-theme 'monoglow t)
  (startup-screen)

  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)))

  ;; Introduce a bottom side window that catches compilations, greps etc.
  (add-to-list 'display-buffer-alist
               `(,(rx (| "*compilation*" "*grep*" "*ripgrep*" "*rg*" "*haskell*" "*Async Shell Command*" "*vterm*"))
        	 (display-buffer-in-side-window)
        	 (side . bottom)
        	 (slot . 0)
        	 (window-parameters . ((no-delete-other-windows . t)))
        	 (window-height . 0.25)))

  ;; We override these 2 functions to prevent pulsing after jumps.
  ;; (defcustom xref-after-jump-hook '(recenter)
  ;;   "Functions called after jumping to an xref." )
  ;; (defcustom xref-after-return-hook '()
  ;;  "Functions called after returning to a pre-jump location.")

  (advice-add 'compile :around
              (lambda (orig-fun command &optional _comint)
		"Ensure the COMINT argument to `compile` is always t."
		(funcall orig-fun command t)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defun count-visible-buffers (&optional frame)
    "Count how many buffers are currently being shown. Defaults to selected frame."
    (length (mapcar #'window-buffer (window-list frame))))

  (defun do-not-split-more-than-two-windows (window &optional horizontal)
    (if (and horizontal (> (count-visible-buffers) 1))
        nil
      t))

  (advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

  ;; The desired ratio of the focused window's size.
  ;; (setopt auto-resize-ratio 0.70)

  ;; (defun win/auto-resize ()
  ;;   (let* (
  ;;          (height (floor (* auto-resize-ratio (frame-height))))
  ;;          (width (floor (* auto-resize-ratio (frame-width))))
  ;;          ;; INFO We need to calculate by how much we should enlarge
  ;;          ;; focused window because Emacs does not allow setting the
  ;;          ;; window dimensions directly.
  ;;          (h-diff (max 0 (- height (window-height))))
  ;;          (w-diff (max 0 (- width (window-width)))))
  ;;     (enlarge-window h-diff)
  ;;     (enlarge-window w-diff t)))

  ;; (advice-add 'other-window :after  (lambda (&rest args)
  ;;                                     (win/auto-resize)))
  ;; (advice-add 'switch-to-buffer-other-window :after  (lambda (&rest args)
  ;;                                                      (win/auto-resize)))

  ;; (advice-add 'projectile-find-file-other-window :after  (lambda (&rest args)
  ;;                                                          (win/auto-resize)))
  ;; (advice-add 'consult-buffer-other-window :after  (lambda (&rest args)
  ;;                                                    (win/auto-resize)))
  )

(use-package window
  :bind (:map window-prefix-map
              ("w" . window-toggle-side-windows)))

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (setq modus-themes-custom-auto-reload nil
;; 	modus-themes-to-toggle '(modus-operandi modus-vivendi)
;; 	modus-themes-italic-constructs t
;; 	modus-themes-bold-constructs nil
;; 	modus-themes-completions '((t . (extrabold)))
;; 	modus-themes-prompts nil)

;;   (setq modus-themes-common-palette-overrides
;; 	'((cursor magenta-cooler)
;;           ;; Make the fringe invisible.
;;           (fringe unspecified)
;;           ;; Make line numbers less intense and add a shade of cyan
;;           ;; for the current line number.
;;           (fg-line-number-inactive "gray50")
;;           (fg-line-number-active cyan-cooler)
;;           (bg-line-number-inactive unspecified)
;;           (bg-line-number-active unspecified)
;;           ;; Make the current line of `hl-line-mode' a fine shade of
;;           ;; gray (though also see my `lin' package).
;;           ;;(bg-hl-line bg-dim)
;;           ;; Make the region have a cyan-green background with no
;;           ;; specific foreground (use foreground of underlying text).
;;           ;; "bg-sage" refers to Salvia officinalis, else the common
;;           ;; sage.
;;           (bg-region bg-sage)
;;           (fg-region unspecified)
;;           ;; Make matching parentheses a shade of magenta.  It
;;           ;; complements the region nicely.
;;           (bg-paren-match bg-magenta-intense)
;;           ;; Make the active mode line a fine shade of lavender
;;           ;; (purple) and tone down the gray of the inactive mode
;;           ;; lines.
;;           (bg-mode-line-active bg-lavender)
;;           (border-mode-line-active bg-lavender)

;;           (bg-mode-line-inactive bg-dim)
;;           (border-mode-line-inactive bg-inactive)
;;           ;; Make the prompts a shade of magenta, to fit in nicely with
;;           ;; the overall blue-cyan-purple style of the other overrides.
;;           ;; Add a nuanced background as well.
;;           (bg-prompt bg-magenta-nuanced)
;;           (fg-prompt magenta-cooler)
;;           ;; Tweak some more constructs for stylistic constistency.
;;           (name blue-warmer)
;;           (identifier magenta-faint)
;;           (keybind magenta-cooler)
;;           (accent-0 magenta-cooler)
;;           (accent-1 cyan-cooler)
;;           (accent-2 blue-warmer)
;;           (accent-3 red-cooler)))

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-vivendi)

;;   ;; Apply customizations after theme loads
;;   (add-hook 'modus-themes-after-load-theme-hook #'my-theme-customizations)

;;   ;; Apply immediately after loading theme
;;   (my-theme-customizations))

(use-package repeat
  :config
  (repeat-mode t))

(use-package recentf
  :init
  (recentf-mode t))

(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)

  :config
  (setq rainbow-delimiters-max-face-count 1)
  ;; set rainbow-delimiters to not highlight < and >
  (defun my-rainbow-delimiters-face (depth match loc)
    (unless (memq (char-after loc) '(?\< ?\>))
      (rainbow-delimiters-default-pick-face depth match loc)))
  (setq rainbow-delimiters-pick-face-function #'my-rainbow-delimiters-face))

(use-package smartscan
  :ensure t
  :hook (prog-mode . smartscan-mode)
  :bind (("C-M-n" . smartscan-symbol-go-forward)
	 ("C-M-p" . smartscan-symbol-go-backward))
  :bind* (("M-n" . forward-paragraph)
          ("M-p" . backward-paragraph)))

(use-package buffer-move
  :ensure t
  :bind (
	 ("<C-S-up>" . buf-move-up)
	 ("<C-S-down>" . buf-move-down)
	 ("<C-S-left>" . buf-move-left)
	 ("<C-S-right>" . buf-move-right)))

(use-package dumb-jump
  :ensure t
  :after consult
  :config

  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-rg-search-args "--pcre2 -j 8 -H --no-heading -n -S")
  (setq dumb-jump-default-project "C:/Programming/") ;; Project specific

  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

(use-package tramp
  :ensure t
  :defer 3
  :after compile
  :config
  (setq tramp-default-method "scp") ;; plink
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
  	(format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 0)

  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024)) ;; 1MB

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)
  ;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;(customize-set-variable 'tramp-syntax 'simplified)
  )

(use-package multiple-cursors
  :ensure t
  :bind (("C-x C-c l" . mc/edit-lines)
	 ("C-x C-c n"   . mc/mark-next-like-this-word)
	 ("C-x C-c p"   . mc/mark-previous-like-this-word)
	 ("C-x C-c a" . mc/mark-all-like-this)
         :repeat-map multiple-cursors-repeat-map
         ("l" . mc/edit-lines)
	 ("n" . mc/mark-next-like-this-word)
	 ("p" . mc/mark-previous-like-this-word)
	 ("a" . mc/mark-all-like-this)))

(use-package make-mark-visible)

(use-package narrow-indirect
  :init
  (setq ni-buf-name-prefix "")
  :bind (:map ctl-x-4-map
              ("nd" . ni-narrow-to-defun-indirect-other-window)
              ("nn" . ni-narrow-to-region-indirect-other-window)
              ("np" . ni-narrow-to-page-indirect-other-window)))

(use-package dired
  :hook
  (dired-mode . dired-omit-mode)
  :init
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer] . dired-jump-to-bottom)
              ([remap beginning-of-defun] . dired-back-to-top)
              ([remap end-of-defun] . dired-jump-to-bottom)
              ("RET" . dired-find-alternate-file)
              ("C-x C-j" . (lambda () (interactive) (find-alternate-file ".."))))
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line (if dired-omit-mode 1 3)))

  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1)))

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-single-candidate-jump nil)

  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package rg
  :ensure t
  :defer t
  :config
  ;; We need to add gnu/linux to rg.el:311 as a system-type because
  ;; without "." rg wasn't working properly under WSL.
  (setq rg-executable "rg"))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package projectile
  :ensure t
  ;; This is needed when remote projects exist.
  ;;:after tramp
  :defer t
  :bind-keymap (("C-x p" . projectile-command-map))   ;; default was C-c p
  :bind (("M-s f" . projectile-find-file)
	 ("M-s d" . projectile-find-dir)
	 ("M-s 4 f" . projectile-find-file-other-window)
	 ("C-x o" . projectile-find-other-file)
	 ("C-x 4 o" . projectile-find-other-file-other-window))

  :config

  (setq projectile-generic-command "fd . -0 --type f --color=never --full-path") ;; --strip-cwd-prefix
  (setq projectile-git-fd-args "-H -0 -E .git -tf --color=never") ;; --strip-cwd-prefix

  (add-to-list 'projectile-globally-ignored-directories "*.svg")

  (rg-define-search rg-c/cpp-project
    "Search C/C++ project source code."
    :dir project
    :flags '("--pcre2 -j 8 -S")
    :files "*.{c,cpp,h}")

  (advice-add 'projectile-ripgrep :override #'rg-c/cpp-project)
  (setq-default projectile-indexing-method 'alien)

  ;;(advice-add 'projectile-project-root :before-while
  ;;(lambda (&optional dir)
  ;;(not (file-remote-p (or dir default-directory)))))

  (projectile-mode t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode t))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ;; C-x bindings in `ctl-x-map'
         ("C-'" . consult-buffer)
         ("C-;" . consult-buffer-other-window)
         ("C-M-'" . consult-project-buffer)
         ("C-M-;" . consult-project-buffer-other-window)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
	 ("M-s z" . consult-find)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (defun my/excluded-buffers-p (buffer)
    "Return t if BUFFER is a `*compilation*` buffer, else nil."
    (let ((name (buffer-name buffer)))
      (string-match-p
       (rx (or "*compilation*" "*grep*" "*ripgrep*" "*rg*" "*haskell*" "*Async Shell Command*" "*vterm*"))
       name)))

  (setq consult-preview-excluded-buffers 'my/excluded-buffers-p)

  (defun consult-project-buffer-other-window ()
    (interactive)
    (let ((consult--buffer-display #'switch-to-buffer-other-window))
      (consult--with-project
        (consult-buffer consult-project-buffer-sources))))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;;(setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   ;;consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.1 "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
		"fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
		 (`(,re . ,hl) (funcall consult--regexp-compiler
					arg 'extended t)))
      (when re
	(cons (append
               (list consult--fd-command
                     "--color=never" "--full-path"
                     (consult--join-regexps re 'extended))
               opts)
              hl))))

  (defun consult-fd (&optional dir initial)
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "fd" dir))
		 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))

  (advice-add 'consult-find :override #'consult-fd)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (setq consult-find-args "fd -x ."))

(use-package which-key
  :ensure t
  :init
  :config
  (setq which-key-use-C-h-commands nil)
  (set-face-attribute 'which-key-command-description-face nil :foreground (face-foreground 'font-lock-variable-name-face)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  (add-to-list 'embark-pre-action-hooks '(:always embark--unmark-target))

  (setq embark-help-key ".")

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	  (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  (add-to-list 'embark-target-injection-hooks
               '(my/visual-query-replace embark--allow-edit (lambda (&rest _) (visual-replace-toggle-query) (visual-replace-tab))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package fancy-dabbrev
  :ensure t
  :hook (prog-mode . fancy-dabbrev-mode)
  :config
  (setq fancy-dabbrev-preview-delay 0.1))

(use-package eglot
 :hook
 (c-ts-mode . eglot-ensure)
 (c++-ts-mode . eglot-ensure)
 (csharp-mode . eglot-ensure)
 :config
 (add-to-list 'eglot-server-programs '((c++-mode c-mode c-ts-mode c++-ts-mode) "clangd-16"))
 (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp" "-lsp")))
 (add-to-list 'eglot-stay-out-of 'eldoc)
 (add-to-list 'eglot-stay-out-of 'flymake)
 (setf (plist-get eglot-events-buffer-config :size) 0)
 (setq track-changes-record-errors nil) ;; narrow-indirect causes some problems.
 :custom
 (eglot-ignored-server-capabilities
  '(
    :hoverProvider
    :documentHighlightProvider
    :documentFormattingProvider
    :documentRangeFormattingProvider
    :documentOnTypeFormattingProvider
    :colorProvider
    :foldingRangeProvider
    :inlayHintProvider
    )))

(use-package eglot-booster
  :after eglot
  :custom
  (eglot-booster-io-only t)
  :config (eglot-booster-mode))

(use-package treesit
  :init
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))))

  (setq treesit-font-lock-level 4)
  ;;(setq treesit--indent-verbose t)

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  :config
  (defface my-ts-number-face
    '((t (:inherit font-lock-constant-face)))
    "Number face.")

  (defvar my/ts-font-lock-settings
    (treesit-font-lock-rules
     :language 'c
     :feature 'literal
     :override t
     '((number_literal) @my-ts-number-face)
     :language 'cpp
     :feature 'literal
     :override t
     '((number_literal) @my-ts-number-face))))

(use-package c-ts-mode
  :init
  (setq c-ts-mode-indent-offset 4)

  :config
  (defun my-custom-c-ts-indent-rules ()
    "Override the built-in BSD indentation style with some additional rules"
    (let ((rules
           '(
	     ((node-is "field_declaration_list") parent-bol 0)
	     ((node-is "enumerator_list") parent-bol 0)
	     ((and (node-is "compound_statement") (parent-is "compound_statement")) parent-bol c-ts-mode-indent-offset)
	     ((node-is "compound_statement") parent-bol 0)
	     ((match "case_statement" "compound_statement") parent-bol c-ts-mode-indent-offset)
	     ;; For top level initializer_list
	     ((and (node-is "initializer_list") (not (parent-is "initializer_list"))) parent-bol 0)
	     ;; For nested initializer_list
	     ((and (node-is "initializer_list") (parent-is "initializer_list")) parent c-ts-mode-indent-offset))))
      `((c ,@rules ,@(alist-get 'c treesit-simple-indent-rules))
	(cpp ,@rules ,@(alist-get 'cpp treesit-simple-indent-rules)))))

  (defun c-ts-mode--declarator-name (node)
    "Extract the name from a typedef node."
    (let ((declarator (treesit-node-child-by-field-name node "declarator")))
      (when declarator
	(treesit-node-text declarator))))

  (defun c-ts-mode--struct-field-name (node)
    "Extract the field name combined with the struct name."
    (let* ((field-list (treesit-node-parent node)) ;; field_declaration_list
           (struct-spec (and field-list (treesit-node-parent field-list))) ;; struct_specifier
           (struct-name (and struct-spec
                             (string-equal (treesit-node-type struct-spec) "struct_specifier")
                             (treesit-node-text (treesit-node-child-by-field-name struct-spec "name"))))
           (field-name (treesit-node-text (treesit-node-child-by-field-name node "declarator"))))
      (if struct-name
          (format "%s::%s" struct-name field-name) ;; Combine struct name and field name
	field-name))) ;; Fallback to just the field name

  (defun c-ts-mode--preprocessor-define (node)
    "Extract the name and value from a preprocessor #define directive."
    (let* ((name-node (treesit-node-child-by-field-name node "name"))
           (value-node (treesit-node-child-by-field-name node "value"))
           (name (and name-node (treesit-node-text name-node)))
           (value (and value-node (treesit-node-text value-node))))
      (if (and name value)
          (format "%s %s" name value) ;; Combine name and value
	(or name ""))))

  (defun c-ts-mode--preprocessor-include (node)
    "Extract the name from a typedef node."
    (let ((include (treesit-node-child-by-field-name node "path")))
      (when include
	(treesit-node-text include))))

  (defun my-setup-c-ts-mode ()
    "Setup custom indentation for `c-ts-mode`."
    (setq-local treesit-simple-imenu-settings
		'(("enum" "\\`enum_specifier\\'" nil nil)
		  ("struct" "\\`struct_specifier\\'" nil nil)
		  ("union" "\\`union_specifier\\'" nil nil)
		  ("variable" "\\`declaration\\'" nil nil)
		  ("function" "\\`function_definition\\'" nil nil)
		  ("class" "\\`\\(?:class_specifier\\|function_definition\\)\\'" nil nil)
		  ("typedef" "\\`type_definition\\'" nil c-ts-mode--declarator-name)
		  ("field" "\\`field_declaration\\'" nil c-ts-mode--struct-field-name)
		  ("#define" "\\`preproc_def\\'" nil c-ts-mode--preprocessor-define)
		  ("#include" "\\`preproc_include\\'" nil c-ts-mode--preprocessor-include)))

    (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
    (setq-local treesit-simple-indent-rules (my-custom-c-ts-indent-rules))

    (cond
     ((derived-mode-p 'c-ts-mode)
      (setq-local c-ts-common-list-indent-style 'simple))
     ((derived-mode-p 'c++-ts-mode)
      (setq-local c++-ts-common-list-indent-style 'simple)))

    (setq-local treesit-font-lock-settings
  		(append treesit-font-lock-settings my/ts-font-lock-settings)))

  (add-hook 'c-ts-mode-hook #'my-setup-c-ts-mode)
  ;; Do we need to add c++ separately?
  (add-hook 'c++-ts-mode-hook #'my-setup-c-ts-mode))

(use-package visual-replace
  :defer nil
  :ensure t
  :bind (("M-%" . my/visual-query-replace)
         :map isearch-mode-map
         ("M-%" . visual-replace-from-isearch))
  :config
  (setq visual-replace-min-length 0)

  (define-key visual-replace-mode-map (kbd "<escape>")
              visual-replace-secondary-mode-map)

  (defun my/visual-query-replace (args ranges)
    "Like visual-replace but defaults to query mode, like query-replace"
    (interactive (visual-replace-read (visual-replace-make-args
                                       :query t
                                       :word (and current-prefix-arg (not (eq current-prefix-arg '-))))))
    (visual-replace args ranges)))

(use-package expreg
  :ensure t
  :bind (("C-," . expreg-expand)
         (:repeat-map expreg-repeat-map
                      ("," . expreg-expand)
                      ("." . expreg-contract))))

(use-package vundo
  :ensure t
  :init
  (setq vundo-compact-display t)
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package nano-modeline
  :ensure t
  :init
  (setq nano-modeline-position #'nano-modeline-footer)
  (setq-default mode-line-format nil)
  :config
  (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
  (nano-modeline-text-mode t))

(use-package move-text
  :ensure t
  :bind (("C-c p". move-text-up)
         ("C-c n". move-text-down)
         (:repeat-map move-text-repeat-map
                      ("p". move-text-up)
                      ("n". move-text-down)))
  :config
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package copilot
  :ensure t
  :hook (prog-mode-hook . copilot-mode)
  :bind (("C-c C-m" . my/copilot-complete-or-accept)
         :map copilot-mode-map
         ("C-c C-n" . copilot-next-completion)
         ("C-c C-p" . copilot-previous-completion)
         ("C-c C-l" . my/copilot-accept-completion-by-line)
         :repeat-map copilot-repeat-map
         ("n" . copilot-next-completion)
         ("p" . copilot-previous-completion)
         ("l" . my/copilot-accept-completion-by-line)
         ("m" . my/copilot-complete-or-accept))
  :init
  (setq copilot-max-char -1)
  (setq copilot-indent-offset-warning-disable t)
  :config

  (defun my/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion)
          (open-line 1)
          (next-line))
      (copilot-complete)))

  (defun my/copilot-accept-completion-by-line ()
    (interactive)
    (progn
      (copilot-accept-completion-by-line)
      (if (not (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
          (progn
            (copilot-clear-overlay)
            (open-line 1)
            (forward-line)
            (copilot-complete))))))

;; https://www.rahuljuliato.com/posts/emacs-tab-bar-groups
;; https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
(use-package tab-bar
  :defer t
  :bind (:map tab-prefix-map
              ("N" . my/tab-next-group)
              ("n" . my/tab-next-in-group)
              ("P" . my/tab-group-from-project)
              ("g" . my/tab-switch-to-group)
              :repeat-map tab-bar-repeat-map
              ("N" . my/tab-next-group)
              ("P" . my/tab-prev-group)
              ("n" . my/tab-next-in-group)
              ("p" . my/tab-prev-in-group))
  :custom
  (tab-bar-define-keys nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  ;; (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
   (tab-bar-separator nil)
  (tab-bar-format '(tab-bar-format-tabs-groups
       	            ;;tab-bar-format-tabs
                    tab-bar-separator
       	            tab-bar-format-add-tab))
  :init

  ;; (defun tab-bar-tab-name-format-hints (name _tab i)
  ;;   (if tab-bar-tab-hints (concat (format "»%d«" i) "") name))

  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
    (propertize
     (concat (funcall tab-bar-tab-group-function tab))
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

  (defun my/tab-next-in-group (&optional arg)
    "Move to the next tab that shares the current tab's `group' parameter.
With numeric prefix ARG, move ARG tabs forward (negative = backward)."
    (interactive "p")
    (let* ((tabs (tab-bar-tabs))
           (cur  (tab-bar--current-tab-index))
           (grp  (alist-get 'group (tab-bar--current-tab)))
           (n    (or arg 1))
           (len  (length tabs)))
      (cl-loop
       for i from 1 to len
       for idx = (mod (+ cur (* i n (cl-signum n))) len)
       if (equal grp (alist-get 'group (nth idx tabs)))
       do (tab-bar-select-tab (1+ idx)) (cl-return))))

  (defun my/tab-prev-in-group () (interactive) (my/tab-next-in-group -1))

  (defun my/tab--select-first-of-group (group)
    "Select the first tab whose `group' parameter equals GROUP."
    (let* ((tabs (tab-bar-tabs))
           ;; find index of first tab in that group
           (idx  (cl-position group
                              (mapcar (lambda (tab) (alist-get 'group tab))
                                      tabs)
                              :test #'equal)))
      (when idx
        ;; tab-bar-select-tab uses 1-based indices
        (tab-bar-select-tab (1+ idx)))))

  (defun my/tab-switch-group (delta)
    "Move 1 group forward (negative = backward) **without** cycling
through tabs inside each group."
    (let* ((tabs   (tab-bar-tabs))
           ;; ordered list of distinct group names
           (groups (delete-dups
                    (mapcar (lambda (tab) (alist-get 'group tab)) tabs)))
           (cur    (alist-get 'group (tab-bar--current-tab)))
           (len    (length groups)))
      (when (> len 1)
        (let* ((pos  (or (cl-position cur groups :test #'equal) 0))
               (next (nth (mod (+ pos delta) len) groups)))
          (my/tab--select-first-of-group next)))))

  (defun my/tab-next-group () (interactive) (my/tab-switch-group +1))
  (defun my/tab-prev-group () (interactive) (my/tab-switch-group -1))

  (defun my/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
      (let* ((groups (delete-dups (mapcar (lambda (tab)
		                            (funcall tab-bar-tab-group-function tab))
                                          tabs)))
	     (group (completing-read "Switch to group: " groups nil t)))
	(let ((i 1) (found nil))
	  (dolist (tab tabs)
	    (let ((tab-group (funcall tab-bar-tab-group-function tab)))
	      (when (and (not found)
			 (string= tab-group group))
		(setq found t)
		(tab-bar-select-tab i)))
	    (setq i (1+ i)))))))

  (defvar first-tab-set nil
    "Variable to check if it is first project tab created.")

  (defun my/default-tab-group ()
    (setq first-tab-set t)
    (tab-group (format "[%s]" (file-name-parent-base (directory-file-name "~/.emacs.d")))))

  (tab-bar-mode t)


  (my/default-tab-group)
  (add-hook 'server-after-make-frame-hook #'my/default-tab-group))

;; This is not a package.
;; Integration for tab-bar and projectile.
(use-package projectile-tab-bridge
  :ensure nil
  :no-require t
  :after (projectile tab-bar)
  :demand t
  :bind (:map tab-prefix-map
              ("P" . my/tab-group-from-project)
              :map projectile-command-map
              ("t" . my/projectile-switch-project-in-new-tab))
  :config

  (defun my/projectile-switch-project-in-new-tab ()
    "Switch PROJECTILE project in a new tab group."
    (interactive)
    (let* ((proj (projectile-completing-read
                  "Switch to project: "
                  (projectile-relevant-known-projects)))
           (name (file-name-parent-base
                  (directory-file-name proj))))
      (unless first-tab-set
        (setq first-tab-set t)
        (tab-group (format "[%s]" name))) ; rename current group first
      (when first-tab-set
        (tab-bar-new-tab))
      (projectile-switch-project-by-name proj)
      (tab-group (format "[%s]" name))))

  (defun my/tab-group-from-project ()
    "Rename current tab-group from current Projectile project."
    (interactive)
    (when-let* ((proj (project-current nil))
                (name (file-name-parent-base (project-root proj))))
      (tab-group (format "[%s]" name)))))

(use-package vterm
  :ensure t
  :bind ("C-c v" . my/toggle-vterm)
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

  (defvar my/vterm-prev-window nil
    "The window that was selected right before a vterm was popped.
Used by `my/toggle-vterm' to restore focus when the vterm is hidden.")

  (defun my/toggle-vterm (&optional arg)
    "Toggle a side-window vterm.
     Behaviour:
       - If a visible vterm window exists → hide it and return to the
          previously active window (stored in `my/vterm-prev-window').
       - Else → show (or create) a vterm in the side window and select it.
     With prefix ARG (\\[universal-argument]) always create a *new* vterm buffer."
    (interactive "P")
    ;; 1.  Find existing vterm buffers & windows
    (let* ((vterm-bufs (seq-filter (lambda (b)
                                     (string-match-p "\\`\\*vterm"
                                                     (buffer-name b)))
                                   (buffer-list)))
           (vterm-win  (seq-some (lambda (buf) (get-buffer-window buf 'visible))
                                 vterm-bufs)))
      (cond
       ;; (A) Vterm is visible and no prefix → hide it, restore prev
       ((and vterm-win (not arg))
        (delete-window vterm-win)
        (when (window-live-p my/vterm-prev-window)
          (select-window my/vterm-prev-window)))

       (t
        ;; remember where we came from *before* we display vterm
        (setq my/vterm-prev-window (selected-window))
        (let* ((buf (cond
                     ;; new vterm requested
                     (arg (let ((default-directory
                                 (or (when (fboundp 'projectile-project-root)
                                       (ignore-errors
                                         (projectile-project-root)))
                                     default-directory)))
                            (vterm "*vterm*<new>")
                            (current-buffer)))
                     ;; reuse first existing buffer if any
                     (vterm-bufs (car vterm-bufs))
                     ;; else create one
                     (t (let ((default-directory
                               (or (when (fboundp 'projectile-project-root)
                                     (ignore-errors
                                       (projectile-project-root)))
                                   default-directory)))
                          (vterm)
                          (current-buffer)))))
               (win (or (get-buffer-window buf 'visible)
                        (display-buffer buf))))
          (select-window win)))))))

(use-package markdown-mode
  :custom
  (markdown-hide-markup t)
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t))

(use-package gptel
  :bind (("C-c j" . gptel-menu))
  :config
  (setq gptel-default-mode 'gfm-mode)
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "##### Prompt: \n"
        (alist-get 'markdown-mode gptel-response-prefix-alist) "##### Response: \n"
        (alist-get 'gfm-mode gptel-prompt-prefix-alist) "##### Prompt: \n"
        (alist-get 'gfm-mode gptel-response-prefix-alist) "##### Response: \n"
        (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: \n"
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*: \n")

  (setq gptel-model 'gpt-4o
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  
  (setf (alist-get 'infill gptel-directives) #'my/gptel-code-infill)
  (defun my/gptel-code-infill ()
    "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
    (let ((lang (gptel--strip-mode-suffix major-mode)))
      `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown code fences. NEVER EVER use markdown formatting. You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
        nil
        "What is the code AFTER the cursor?"
        ,(format "AFTER\n```\n%s\n```\n"
                 (buffer-substring-no-properties
                  (if (use-region-p) (max (point) (region-end)) (point))
                  (point-max)))
        "And what is the code BEFORE the cursor?"
        ,(format "BEFORE\n```%s\n%s\n```\n" lang
                 (buffer-substring-no-properties
                  (point-min)
                  (if (use-region-p) (min (point) (region-beginning)) (point))))
        ,@(when (use-region-p) "What should I insert at the cursor?"))))


  (cl-defun my/clean-up-gptel-refactored-code (beg end)
    "Clean up the code responses for refactored code in the current buffer.

The response is placed between BEG and END.  The current buffer is
guaranteed to be the response buffer."
    (when gptel-mode          ; Don't want this to happen in the dedicated buffer.
      (cl-return-from my/clean-up-gptel-refactored-code))
    (when (and beg end)
      (save-excursion
        (let ((contents
               (replace-regexp-in-string
                "\n*``.*\n*" ""
                (buffer-substring-no-properties beg end))))
          (delete-region beg end)
          (goto-char beg)
          (insert contents))
        ;; Indent the code to match the buffer indentation if it's messed up.
        (indent-region beg end)
        (pulse-momentary-highlight-region beg end))))
  
  (add-hook 'gptel-post-response-functions #'my/clean-up-gptel-refactored-code)

  (defun my/gptel-finish-code-block (beg end)
    "Force fontify again for correct markdown formatting.
     This is expensive probably but what you gonna do. "
      (when (and beg end gptel-mode)
        (font-lock-flush beg end)
        (font-lock-ensure beg end)))

  (add-hook 'gptel-post-response-functions #'my/gptel-finish-code-block))

(use-package llm-tool-collection
  :after gptel
  :config (mapcar (apply-partially #'apply #'gptel-make-tool)
                  (llm-tool-collection-get-all))
  :defer)

;; This is valid after emacs-31. We use this in WSL side.
;;(use-package c-ts-mode
;;  :init
;;  (setq c-ts-mode-indent-offset 4)
;;  :config
;;  ;;(setq c-ts-mode-indent-style 'bsd)
;;
;;  (defun my-custom-c-ts-indent-rules ()
;;  "Combine BSD rules with custom rules for `c-ts-mode`."
;;  (let ((bsd-rules (c-ts-mode--simple-indent-rules 'c 'bsd)))
;;    (append bsd-rules
;;            `(
;;	      ;; Add new rules here.
;;  	      ;;((match "case_statement" "compound_statement") parent-bol c-ts-mode-indent-offset)
;;  	      ))))
;;
;;  (defun my-setup-c-ts-mode ()
;;    "Setup custom indentation for `c-ts-mode`."
;;    (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
;;    (setq-local treesit-simple-indent-rules (my-custom-c-ts-indent-rules))
;;    (setq-local treesit-font-lock-settings
;;	      (append treesit-font-lock-settings my/ts-font-lock-settings))
;;    )
;;
;;  (add-hook 'c-ts-mode-hook #'my-setup-c-ts-mode)
;;  ;; Do we need to add c++ separately?
;;  (add-hook 'c++-ts-mode-hook #'my-setup-c-ts-mode)
;;  )
