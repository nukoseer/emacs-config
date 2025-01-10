;; -*- lexical-binding: t; -*-
(setq native-comp-speed 3) ;; maximum native Elisp speed!

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
 '(package-selected-packages
   '(fancy-dabbrev consult-dir multiple-cursors modus-themes use-package which-key embark-consult embark consult marginalia orderless vertico rg projectile avy dumb-jump smartscan rainbow-delimiters highlight-numbers gcmh buffer-move))
 '(custom-safe-themes
   '("d015f7295925398145c42285e2ea4bb438d449d36e2b10ba0650024862ec93a8"
     "62097dbc0924e2b42f9eaeead73fc2c12cf7b579c214bbb5e755d4f2391ffc2f"
     "bc2936e8cd9c3e67623e76672ddf53411e60723e2ed0cad8b4ca59b5a2d80bbf"
     "6a784261d7e9bb651d6b4867b8f3c06ba7fa255a869a0a44ac35290e94776d38"
     "f4157511d5d4a31766a01ce6aeef7329a39afbfa61f6f6a96a29bb97dc9e00b1"
     "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577"
     "ccdc42b444da0b62c25850da75f59186319ee22ddfd153ffc9f7eb4e59652fc9"
     "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5"
     default))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package emacs
  :init
  (add-hook 'emacs-startup-hook '(lambda ()
				   (message "Emacs ready in %s with %d garbage collections."
					    (format "%.2f seconds"
						    (float-time
						     (time-subtract after-init-time before-init-time)))
					    gcs-done)))

  (add-hook 'emacs-lisp-mode-hook '(lambda ()
				     (local-set-key (kbd "<tab>") #'fancy-dabbrev-expand)
			             (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
				     ))

  ;;close git service
  (setq vc-handled-backends nil)

  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil)

  ;; theme
  (setq custom--inhibit-theme-enable nil)

  (setq grep-use-null-device nil)
  (setq linum-format " %5i ")

  ;; switch-to-buffer-other-window will switch vertically
  (setq split-width-threshold nil)
  (setq split-height-threshold 200)

  (setq window-divider-default-places 'right-only)
  ;;(setq window-divider-default-right-width 12)

  ;; no ugly button for checkboxes
  (setq widget-image-enable nil)

  (setq file-name-handler-alist nil)
  (setq frame-inhibit-implied-resize t)

  (setq default-frame-alist
	(append (list
		 '(font . "Iosevka Fixed-12")
		 '(internal-border-width . 0)
		 '(left-fringe  . 12)
		 '(right-fringe . 12))))

  ;; set default directory to c++ folder
  (setq default-directory "C:/")

  ;; disable auto save mode
  (setq auto-save-default nil)
  ;; disable back up
  (setq make-backup-file-name-function (quote ignore))
  (setq make-backup-files nil)

  ;; turn off the bell
  (defun nil-bell ())
  (setq ring-bell-function 'nil-bell)

  ;; smooth scroll
  (setq scroll-step 3)

  ;; always kill *compilation* buffer before new *compilation* start
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t)

  (setq lexical-binding t)

  (setq switch-to-buffer-obey-display-actions t)

  ;; activate fullscreen, open empty buffer and init.el
  (defun startup-screen ()
    (if (< (count-windows) 2)
	(progn
	  (setq inhibit-startup-message t)
	  (setq inhibit-splash-screen t)
	  (setq initial-scratch-message nil)
	  (toggle-frame-fullscreen)
	  (switch-to-buffer "*scratch*")
	  (split-window-right)
	  (other-window 1)
	  (find-file "~/.emacs.d/init.el")
	  (other-window 1))))

  (startup-screen)

  (defun my-load-all-in-directory (dir)
    "`load' all elisp libraries in directory DIR which are not already loaded."
    ;;  (interactive "D")
    (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                    (delq nil (mapcar #'car load-history)))))
      (dolist (file (directory-files dir t ".+\\.elc?$"))
	(let ((library (file-name-sans-extension file)))
          (unless (member library libraries-loaded)
            (load library nil t)
            (push library libraries-loaded))))))

  (my-load-all-in-directory '"~/.emacs.d/others/")

  :bind (
	 ("C-z"       . undo)
	 ("M-<up>"    . move-line-up)
	 ("M-<down>"  . move-line-down)
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
	 ("C-x C-c"   . nil)
	 ("<M-f4>"    . save-buffers-kill-terminal)
	 ("C-M-c"     . scroll-other-window-down)
	 ("C-x C-b"   . ibuffer)
	 ("<f1>"      . build)
         ("<f2>"      . run)
         ("<f3>"      . generate)
	 ("C-x w"     . window-toggle-side-windows)
	 ("<escape>"  . window-toggle-side-windows)
	 ("C-,"       . mark-sexp)
	 ("C-x j"     . previous-buffer)
	 ("C-x l"     . next-buffer)
	 ("M-n"       . forward-paragraph)
	 ("M-p"       . backward-paragraph)
	 ("C-/"       . duplicate-line))

  :config

  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (blink-cursor-mode 0)
  (column-number-mode t)
  (electric-pair-mode t)
  (global-visual-line-mode t)
  (global-hl-line-mode t)
  (global-so-long-mode t)
  (global-display-line-numbers-mode 0)
  (global-auto-revert-mode t)
  (window-divider-mode t)
  (global-eldoc-mode 0)

  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)))

  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  ;;(load-theme 'nano t)

  (defun reset-emacs ()
    "Reset emacs."
    (interactive)
    (mapc 'kill-buffer (cl-remove-if
			(lambda (x)
                          (or
                           ;; (eq x (current-buffer))
                           (member (buffer-name x) '("*Messages*" "*scratch*" "init.el"))))
			(buffer-list)))
    (unlock-compilation-directory)
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (split-window-right)
    (other-window 1)
    (find-file "~/.emacs.d/init.el")
    (other-window 1))

  ;; change window split orientation (horizontal to vertical or opposite)
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
	(let* ((this-win-buffer (window-buffer))
	       (next-win-buffer (window-buffer (next-window)))
	       (this-win-edges (window-edges (selected-window)))
	       (next-win-edges (window-edges (next-window)))
	       (this-win-2nd (not (and (<= (car this-win-edges)
					   (car next-win-edges))
				       (<= (cadr this-win-edges)
					   (cadr next-win-edges)))))
	       (splitter
		(if (= (car this-win-edges)
		       (car (window-edges (next-window))))
		    'split-window-horizontally
		  'split-window-vertically)))
	  (delete-other-windows)
	  (let ((first-win (selected-window)))
	    (funcall splitter)
	    (if this-win-2nd (other-window 1))
	    (set-window-buffer (selected-window) this-win-buffer)
	    (set-window-buffer (next-window) next-win-buffer)
	    (select-window first-win)
	    (if this-win-2nd (other-window 1))))))

  (define-key ctl-x-4-map "t" 'toggle-window-split)

  ;; move line
  (defun move-line (n)
    "Move the current line up or down by N lines."
    (interactive "p")
    (setq col (current-column))
    (beginning-of-line) (setq start (point))
    (end-of-line) (forward-char) (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col)))

  (defun move-line-up (n)
    "Move the current line up by N lines."
    (interactive "p")
    (move-line (if (null n) -1 (- n))))

  (defun move-line-down (n)
    "Move the current line down by N lines."
    (interactive "p")
    (move-line (if (null n) 1 n)))

  ;; Introduce a bottom side window that catches compilations, greps etc.
  (add-to-list 'display-buffer-alist
	       `(,(rx (| "*compilation*" "*grep*" "*ripgrep*" "*rg*" "*haskell*" "*Async Shell Command*"))
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (slot . 0)
		 (window-parameters . ((no-delete-other-windows . t)))
		 (window-height . 0.25)))

  (defun add-todo ()
    (interactive)
    (insert "// TODO: "))

  (defun add-note ()
    (interactive)
    (insert "// NOTE: "))

  ;; highlighting for TODO and NOTE
  (setq fixme-modes '(c-mode c-ts-mode c++-mode c++-ts-mode emacs-lisp-mode))
  (make-face 'font-lock-fixme-face)
  (make-face 'font-lock-note-face)
  (make-face 'font-lock-important-face)
  (make-face 'font-lock-study-face)
  (mapc (lambda (mode)
	  (font-lock-add-keywords
	   mode
	   '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
             ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	     ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
	     ("\\<\\(STUDY\\)" 1 'font-lock-study-face t))))
	fixme-modes)
  (modify-face 'font-lock-fixme-face "Red" nil nil t nil nil nil nil)
  (modify-face 'font-lock-note-face "Dark Green" nil nil t nil nil nil nil)
  (modify-face 'font-lock-important-face "Orange" nil nil t nil nil nil nil)
  (modify-face 'font-lock-study-face "Orange" nil nil t nil nil nil nil)

  ;; find project root, build, run
  (setq project-base-bat "build.bat")
  (setq project-base-sh  "./build.sh")
  (setq compilation-directory-locked nil)

  (defun find-project-directory-recursive ()
    "Recursively search for a makefile."
    (interactive)
    (if (or (file-exists-p project-base-bat) (file-exists-p project-base-sh)) t
      (cd "../")
      (find-project-directory-recursive)))

  (defun lock-compilation-directory ()
    "The compilation process should NOT hunt for a makefile"
    (interactive)
    (setq compilation-directory-locked t)
    (message "Compilation directory is locked."))

  (defun unlock-compilation-directory ()
    "The compilation process SHOULD hunt for a makefile"
    (interactive)
    (setq compilation-directory-locked nil)
    (message "Compilation directory is unlocked."))

  (defun find-project-directory ()
    "Find the project directory."
    (interactive)
    (setq find-project-from-directory default-directory)
    (switch-to-buffer-other-window "*compilation*")
    (if compilation-directory-locked (cd last-compilation-directory)
      (lock-compilation-directory)
      (cd find-project-from-directory)
      (find-project-directory-recursive)
      (setq last-compilation-directory default-directory)))

  (defun build ()
    "Make the current build."
    (interactive)
    (if (find-project-directory)
	(if (file-exists-p project-base-bat)
	    (compile project-base-bat)
	  (compile project-base-sh)))
    (other-window 1))

  (defun run ()
    "Run the current build."
    (interactive)
    (if (find-project-directory) (compile "run.bat"))
    (other-window 1))

  ;; We override these 2 functions to prevent pulsing after jumps.
  (defcustom xref-after-jump-hook '(recenter)
    "Functions called after jumping to an xref." )
  (defcustom xref-after-return-hook '()
    "Functions called after returning to a pre-jump location.")

  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
 Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Mark set"))

  (defun jump-to-mark ()
    "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
    (interactive)
    (set-mark-command 1))

  ;;(set-variable 'grep-command "rg --pcre2 -j 8 -H --no-heading --color=always -n -S -e ")
  ;;(grep-apply-setting
  ;; 'grep-find-command
  ;; '("rg --pcre2 -j 8 -H --no-heading --color=always -n -S -e \"\" . -tc -tcpp" . 58)
  ;; )

  (defun grep-fd (command-args)
    (interactive
     (progn
       (grep-compute-defaults)
       (if grep-find-command
	   (if (file-remote-p default-directory)
	       (list (read-shell-command "Run find (like this): "
					 '("fd \"\" . -x \"echo\" {}:1:" . 5) 'grep-find-history))
	     (list (read-shell-command "Run find (like this): "
				       `(,(format "fd \"\" %S -x cmd /C echo \"{}:1:\"" default-directory) . 5) 'grep-find-history)))
	 ;; No default was set
	 (read-string
          "compile.el: No `grep-find-command' command available. Press RET.")
	 (list nil))))
    (when command-args
      (let ((null-device nil))		; see grep
	(grep command-args))))

  (advice-add 'compile :around
              (lambda (orig-fun command &optional _comint)
		"Ensure the COMINT argument to `compile` is always t."
		(funcall orig-fun command t)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (add-hook 'prog-mode-hook '(lambda ()
			       (local-set-key (kbd "<tab>") #'fancy-dabbrev-expand)
			       (local-set-key (kbd "C-c C-c") #'comment-region)
			       (local-set-key (kbd "C-c C-v") #'uncomment-region)
			       ))

  (add-hook 'asm-mode-hook '(lambda ()
			      (local-unset-key (kbd ";"))))

  (setq process-connection-type nil)
  (setq duplicate-line-final-position 1)

  (defface mode-line-remote
    '((t (:inherit mode-line-buffer-id :box (:line-width 2 :color "orange"))))
    "Face for mode line buffer identification in remote buffers."
    :group 'mode-line)

  (defun my-update-mode-line-buffer-identification ()
    "Apply a custom face to `mode-line-buffer-identification` for remote buffers only."
    (setq mode-line-buffer-identification
          (if (or (and buffer-file-name (file-remote-p buffer-file-name))
                  (and default-directory (file-remote-p default-directory)))
              (list (propertize "%b" 'face 'mode-line-remote))
            (list (propertize "%b" 'face 'mode-line-buffer-id))))
    )

  (add-hook 'buffer-list-update-hook 'my-update-mode-line-buffer-identification)
  
  (defun my-theme-customizations ()
    (set-face-attribute 'window-divider nil :foreground (face-background 'default) :background (face-background 'default))
    (set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'mode-line))
    (set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'mode-line))
    (set-face-attribute 'font-lock-variable-name-face nil :foreground (face-foreground 'default))
    
    (face-spec-set 'ansi-color-bright-white
  		   '((t (:background "gray55" :foreground "gray55"))))
    
    (face-spec-set 'mode-line
  		   '((t (:box (:style released-button)))))
    
    (with-eval-after-load 'fancy-dabbrev
      (set-face-attribute 'fancy-dabbrev-preview-face nil :background (face-background 'hl-line) :foreground (face-foreground 'default))
      (set-face-attribute 'fancy-dabbrev-menu-face nil :background (face-background 'default) :foreground (face-foreground 'default))
      (set-face-attribute 'fancy-dabbrev-selection-face nil :background (face-background 'region) :foreground (face-foreground 'font-lock-type-face)))
    )

  ;; This hook is called after emacsclient creates a frame.
  (add-hook 'server-after-make-frame-hook '(lambda ()
					     (startup-screen)
					     (my-theme-customizations)))
  )

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-custom-auto-reload nil
	modus-themes-to-toggle '(modus-operandi modus-vivendi)
	modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-completions '((t . (extrabold)))
	modus-themes-prompts nil)

  (setq modus-themes-common-palette-overrides
	'((cursor magenta-cooler)
          ;; Make the fringe invisible.
          (fringe unspecified)
          ;; Make line numbers less intense and add a shade of cyan
          ;; for the current line number.
          (fg-line-number-inactive "gray50")
          (fg-line-number-active cyan-cooler)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          ;;(bg-hl-line bg-dim)
          ;; Make the region have a cyan-green background with no
          ;; specific foreground (use foreground of underlying text).
          ;; "bg-sage" refers to Salvia officinalis, else the common
          ;; sage.
          (bg-region bg-sage)
          (fg-region unspecified)
          ;; Make matching parentheses a shade of magenta.  It
          ;; complements the region nicely.
          (bg-paren-match bg-magenta-intense)
          ;; Make the active mode line a fine shade of lavender
          ;; (purple) and tone down the gray of the inactive mode
          ;; lines.
          (bg-mode-line-active bg-lavender)
          (border-mode-line-active bg-lavender)

          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-inactive)
          ;; Make the prompts a shade of magenta, to fit in nicely with
          ;; the overall blue-cyan-purple style of the other overrides.
          ;; Add a nuanced background as well.
          (bg-prompt bg-magenta-nuanced)
          (fg-prompt magenta-cooler)
          ;; Tweak some more constructs for stylistic constistency.
          (name blue-warmer)
          (identifier magenta-faint)
          (keybind magenta-cooler)
          (accent-0 magenta-cooler)
          (accent-1 cyan-cooler)
          (accent-2 blue-warmer)
          (accent-3 red-cooler)))

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi)

  ;; Apply customizations after theme loads
  (add-hook 'modus-themes-after-load-theme-hook #'my-theme-customizations)

  ;; Apply immediately after loading theme
  (my-theme-customizations))

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode))

(use-package repeat
  :defer t
  :config
  (repeat-mode t))

(use-package recentf
  :init
  (recentf-mode t))

(use-package paren
  :init
  (setq show-paren-delay 0)
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
  :bind (("C-M-n" . smartscan-symbol-go-forward)
	 ("C-M-p" . smartscan-symbol-go-backward))
  :config
  (smartscan-mode t))

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
  :config
  (setq tramp-default-method "plink")
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
  	(format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 0)

  ;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;(customize-set-variable 'tramp-syntax 'simplified)
  )

(use-package multiple-cursors
  :ensure t
  :bind (
	 ("C-c c" . mc/edit-lines)
	 ("C->"   . mc/mark-next-like-this-word)
	 ("C-<"   . mc/mark-previous-like-this-word)
	 ("C-c a" . mc/mark-all-like-this)))

(use-package make-mark-visible
  :bind (
	 ("C-c v" . mmv-toggle-mark-visibility)))

(use-package narrow-indirect
  :init
  (setq ni-buf-name-prefix "")
  (define-key ctl-x-4-map "nd" #'ni-narrow-to-defun-indirect-other-window)
  (define-key ctl-x-4-map "nn" #'ni-narrow-to-region-indirect-other-window)
  (define-key ctl-x-4-map "np" #'ni-narrow-to-page-indirect-other-window)
  )

(use-package dired
  :init
  (setq dired-dwim-target t)
  :hook
  (dired-mode . dired-omit-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line (if dired-omit-mode 2 4)))

  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'beginning-of-defun) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-defun) 'dired-jump-to-bottom)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-x C-j") (lambda () (interactive) (find-alternate-file "..")))
  )

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
  :config
  (projectile-mode t)
  (setq projectile-generic-command "fd . -0 --type f --color=never --full-path --strip-cwd-prefix")
  (setq projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix --color=never")

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

  ;; default was C-c p
  :bind-keymap (("C-x p" . projectile-command-map))

  :bind (
	 ("M-s f" . projectile-find-file)
	 ("M-s 4 f" . projectile-find-file-other-window)
	 ("C-x o" . projectile-find-other-file)
	 ("C-x 4 o" . projectile-find-other-file-other-window)))

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

  (setq consult-find-args "fd -x .")
  )

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

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

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
  (setq fancy-dabbrev-preview-delay 0.1)
  )

;;(use-package eglot
;;  :hook
;;  (c-ts-mode . eglot-ensure)
;;  (c++-ts-mode . eglot-ensure)
;;  (csharp-mode . eglot-ensure)
;;  :config
;;  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;;  (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp" "-lsp")))
;;  (add-to-list 'eglot-stay-out-of 'eldoc)
;;  (add-to-list 'eglot-stay-out-of 'flymake)
;;  (setf (plist-get eglot-events-buffer-config :size) 0)
;;  (setq track-changes-record-errors nil) ;; narrow-indirect causes some problems.
;;  :custom
;;  (eglot-ignored-server-capabilities
;;   '(
;;     :hoverProvider
;;     :documentHighlightProvider
;;     :documentFormattingProvider
;;     :documentRangeFormattingProvider
;;     :documentOnTypeFormattingProvider
;;     :colorProvider
;;     :foldingRangeProvider
;;     :inlayHintProvider
;;     )))
;;
;;(use-package eglot-booster
;;  :after eglot
;;  :config (eglot-booster-mode))

(use-package treesit
  :init
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  ;;(setq treesit-language-source-alist
  ;; '((c "https://github.com/tree-sitter/tree-sitter-c")
  ;;   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  ;;   ))

  (setq treesit-font-lock-level 4)
  ;;(setq treesit--indent-verbose t)

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
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
    `((c
      ((node-is "field_declaration_list") parent-bol 0)
      ((node-is "enumerator_list") parent-bol 0)
      ((node-is "compound_statement") parent-bol 0)
      ((match "case_statement" "compound_statement") parent-bol c-ts-mode-indent-offset)
      ((node-is "initializer_list") parent-bol 0)
      ,@(alist-get 'c treesit-simple-indent-rules))))

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
    (setq-local c-ts-common-list-indent-style 'simple)
    (setq-local treesit-font-lock-settings
  		(append treesit-font-lock-settings my/ts-font-lock-settings)))

  (add-hook 'c-ts-mode-hook #'my-setup-c-ts-mode)
  ;; Do we need to add c++ separately?
  (add-hook 'c++-ts-mode-hook #'my-setup-c-ts-mode))

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; C-c C-\ c-backslash-region

;; M-s h r highlight-regexp
;; M-s h u unhighlight-regexp

;; C-x C-l downcase-region
;; C-x C-u upcase-region

;; C-M-Space mark-sexp

;; C-x n n Narrow down to between point and mark (narrow-to-region).
;; C-x n w Widen to make the entire buffer accessible again (widen).
;; C-x n p Narrow down to the current page (narrow-to-page).
;; C-x n d Narrow down to the current defun

;; https://www.reddit.com/r/emacs/comments/xvj3b/til_about_calc_and_quick_calc_modes/
;; https://www.reddit.com/r/emacs/comments/jdrcer/calc_is_fun/
;; C-x * e
;; C-x * u calc-embedded-update-formula
;; C-x * : calc-grab-sum-down

;; C-M-u backward-up-list
;; C-M-n forward-list

;; C-M-l reposition-window

;; C-x r w save window layout to register
;; C-x r j open window layout in register

;; M-s w isearch-forward-word

;; C-x r N rectangle line numbers
