(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

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
 '(grep-use-null-device nil)
 '(linum-format " %5i ")
 '(package-selected-packages
   '(vdiff expand-region dumb-jump smartscan rainbow-delimiters highlight-numbers glsl-mode gcmh buffer-move))
 '(rainbow-delimiters-max-face-count 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

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

;; theme
(setq custom--inhibit-theme-enable nil)
(load-theme 'nano t)

(gcmh-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)
(column-number-mode 1)
(icomplete-mode 1)
(electric-pair-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(global-smartscan-mode 1)
(global-so-long-mode 1)
(global-display-line-numbers-mode 0)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(repeat-mode 1)
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(run-with-idle-timer 0.2 nil (lambda ()
			       (setq ido-enable-flex-matching t)
			       (setq ido-everywhere t)
			       (ido-mode 1)))

;; switch-to-buffer-other-window will switch vertically
(setq split-width-threshold nil)
(setq split-height-threshold 200)

(setq window-divider-default-places 'right-only)
;;(setq window-divider-default-right-width 12)
(window-divider-mode 1)

;; no ugly button for checkboxes
(setq widget-image-enable nil)

(setq file-name-handler-alist nil)
(setq frame-inhibit-implied-resize t)

(setq default-frame-alist
      (append (list
	       '(font . "Iosevka Fixed-12") ;; PxPlus ToshibaSat 8x16:pixelsize=16:dpi=96:autohint=false:hinting=false:antialias=standard -- Liberation Mono-11.5
	       '(internal-border-width . 0)
	       '(left-fringe  . 12)
               '(right-fringe . 12))))

;; force emacs for utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;;(modify-coding-system-alist 'file "" 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq default-buffer-file-coding-system 'utf-8) 
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; set default directory to c++ folder
(setq default-directory "C:/Programming")

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; activate fullscreen, open empty buffer and init.el
(defun start-up-screen ()
  (if (< (count-windows) 2)
      (progn
	(toggle-frame-fullscreen)
	(setq inhibit-splash-screen 'my-start-screen)
	(switch-to-buffer "*scratch*")
	(setq initial-scratch-message nil)
	(split-window-right)
	(other-window 1)
	(find-file "~/.emacs.d/init.el")
	(other-window 1))))

(start-up-screen)

(show-paren-mode 1)
(setq show-paren-delay 0)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; set rainbow-delimiters to not highlight < and >
(defun my-rainbow-delimiters-face (depth match loc)
  (unless (memq (char-after loc) '(?\< ?\>))
    (rainbow-delimiters-default-pick-face depth match loc)))

(setq rainbow-delimiters-pick-face-function #'my-rainbow-delimiters-face)

;; shortcuts to change buffer position
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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

;; equal to C-<down> and C-<up>
;; Replaced with smartscan
;;(global-set-key (kbd "M-n") 'forward-paragraph)
;;(global-set-key (kbd "M-p") 'backward-paragraph)

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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;;(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;;(define-key emacs-lisp-mode-map (kbd "<tab>") 'dabbrev-expand)
   (define-key emacs-lisp-mode-map (kbd "<C-tab>") 'indent-for-tab-command)
   ))

;; shortcut for buffer switching
;;(global-set-key (kbd "C-,") 'switch-to-buffer)
(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-z") 'undo)

;; c / c++ visual studio style code indentation
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-cont 0)
  ;; other customizations can go here
  
  (c-set-offset 'inextern-lang 0)
  
  (setq c-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2
  (c-set-offset 'case-label '+)       ;; for switch-case
  (c-set-offset 'statement-case-intro 0)
  (c-set-offset 'statement-case-open 0)

  (c-set-offset 'brace-list-open 0)      ;; open brace of an enum or static array list
  (c-set-offset 'brace-list-close 0)      ;; open brace of an enum or static array list
  (c-set-offset 'brace-list-intro '+)      ;; first line in an enum or static array list
  (c-set-offset 'brace-list-entry 0)      ;; subsequent lines in an enum or static array
  
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; use spaces only if nil
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook
 'prog-mode-hook
 (lambda ()
   (local-set-key (kbd "<tab>") #'dabbrev-expand)
   ))

(add-hook
 'c++-mode-hook
 (lambda ()
   ;; C-c C-c comment region (in c++ mode) (already defined)
   ;; C-c C-v uncomment region (in c++ mode)
   (local-set-key (kbd "C-c C-v") #'uncomment-region)
   (local-set-key (kbd "<C-tab>") #'c-indent-line-or-region)
   ;; ***_API keywords will be like noise macros (ex. __declspec(dllexport))
   ;; for correct indentation 
   (setq c-noise-macro-names "[A-Z_]+_API")
   ;(setq c-macro-names-with-semicolon
   ;'("INVALID_DEFAULT_CASE"))
   ))

(add-hook
 'python-mode-hook
 (lambda ()

   (local-set-key (kbd "C-c C-c") #'comment-region)
   (local-set-key (kbd "C-c C-v") #'uncomment-region)
   (local-set-key (kbd "<C-tab>") #'c-indent-line-or-region)
   ))

;; disable auto save mode
(setq auto-save-default nil)
;; disable back up
(setq make-backup-file-name-function (quote ignore))
(setq make-backup-files nil)

;; add todo function
(defun add-todo ()
  (interactive)
  (insert "// TODO: "))

(defun add-note ()
  (interactive)
  (insert "// NOTE: "))

;; add #if 0 #endif block
(defun add-if0 (start end)
  (interactive "r")
  (let ((end (copy-marker end)))
    (goto-char start)
    (beginning-of-line)
    (open-line 1)
    (insert "#if 0")
    (goto-char end)
    (end-of-line)
    (newline)
    (insert "#endif")
    (goto-char start)
    (beginning-of-line)))

;; delete #if 0 #endif block
(defun delete-if0 ()
  (interactive)
  (if (search-forward "#if 0" nil t)
      (progn
	(kill-whole-line)
	(if (search-forward "#endif" nil t)
	    (kill-whole-line)))))

;; highlighting for TODO and NOTE
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode dart-mode))
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

;; turn off the bell
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; smooth scroll
(setq scroll-step 3)

;; find project root, build, run
(setq project-base "build.bat")
(setq compilation-directory-locked nil)

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p project-base) t
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
  (if (find-project-directory) (compile project-base))
  (other-window 1))

(defun run ()
  "Run the current build."
  (interactive)
  (if (find-project-directory) (compile "run.bat"))
  (other-window 1))

(defun generate ()
  "Generate the current project."
  (interactive)
  (if (find-project-directory) (compile "generate.bat"))
  (other-window 1))

;; always kill *compilation* buffer before new *compilation* start
(setq compilation-always-kill t)

(setq project-files-loaded nil)

(defun lock-project-directory ()
  (interactive)
  (setq project-files-loaded t)
  (message "Project directory is locked."))

(defun unlock-project-directory ()
  (interactive)
  (setq project-files-loaded nil)
  (message "Project directory is unlocked."))

(defun load-project-files ()
  (interactive)
  (if (not project-files-loaded)
      (progn
	(other-window 1)
	(setq other-window-buffer (buffer-name))
	(other-window 1)
	(if (find-project-directory)
	    (progn
	      (setq project-files-loaded t)
	      (setq project-root-directory default-directory)
	      (mapc #'find-file (directory-files-recursively project-root-directory "\\(\\.glsl$\\|\\.cpp$\\|\\.h$\\|\\.c$\\)" nil))
	      (switch-to-buffer other-window-buffer)
	      (other-window 1)
	      (message "Project files have been loaded."))))
    (message "Project files have already been loaded.")))

(define-key global-map (kbd "<f1>") 'build)
(define-key global-map (kbd "<f2>") 'run)
(define-key global-map (kbd "<f3>") 'generate)

(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
	 ;;   ("\\.glsl$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
	 ("\\.hlsl$"   . c++-mode)
	 ) auto-mode-alist))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Mark set"))

(global-set-key (kbd "C-SPC") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;(global-set-key (kbd "M-\"") 'jump-to-mark)
(global-set-key (kbd "M-`") 'jump-to-mark)

;; C-n adds new line if it is end of the buffer
(setq next-line-add-newlines t)

(global-set-key (kbd "M-o") 'other-window) ;; default C-x o
(global-set-key (kbd "M-w") 'copy-region-as-kill)

;; Replace C-a and M-m
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'beginning-of-visual-line)

(global-set-key (kbd "M-h") 'backward-kill-word) ;; default C-<backspace>
(global-set-key (kbd "C-h") 'backward-delete-char)
;;(global-set-key (kbd "C-c d") 'kill-region) ;; default C-w

(global-set-key (kbd "C-?") 'help-command) ;; default C-h

;; enlarge and shrink window
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

;; HideShow minor mode
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(global-set-key (kbd "C-+") 'hs-toggle-hiding)

;; (setq magit-refresh-status-buffer nil)
;; (setq magit-commit-show-diff nil
;;       magit-revert-buffers 1)

;; (setq auto-revert-buffer-list-filter
;;       'magit-auto-revert-repository-buffer-p)

;;(if (eq system-type 'windows-nt)
;;     (progn
;;       (setq exec-path (add-to-list 'exec-path "~/.emacs.d"))
;;       (setenv "PATH" (concat "~/.emacs.d" (getenv "PATH")))))

;;(defun nano-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."

;;  (dolist (window (window-list))
;;    (with-selected-window window
;;      (if (or (one-window-p t)
;;	      (eq (window-in-direction 'below) (minibuffer-window))
;;	      (not (window-in-direction 'below)))
;;	  (with-current-buffer (window-buffer window)
;;	    (setq mode-line-format "%-"))
;;	(with-current-buffer (window-buffer window)
;; 	  (setq mode-line-format nil)))
;;            (if (window-in-direction 'above)
;;      	      (face-remap-add-relative 'header-line '(:overline "#777777"))
;;      	    (face-remap-add-relative 'header-line '(:overline nil)))
;;      )))
;;(add-hook 'window-configuration-change-hook 'nano-modeline-update-windows)

;; (defun pulse-line (&rest _)
;;   "Pulse the current line."
;;   (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;; 				     recenter-top-bottom other-window))
;;   (advice-add command :after #'pulse-line))

;; (defun add-custom-keyw()
;;   ;;adds a few special keywords for c and c++ modes"
;;   (font-lock-add-keywords nil '(("\\<\\(global\\)" . 'font-lock-special-macro-face)
;; 				("\\<\\(internal\\)" . 'font-lock-special-macro-face)
;; 				("\\<\\(local_persist\\)" . 'font-lock-special-macro-face))))

;; (add-hook 'c++-mode-hook 'add-custom-keyw)

;; (defvar special-macro-regexp (rx bow (or "global" "internal" "local_persist") eow))
;; (font-lock-add-keywords nil `((,special-macro-regexp . `font-lock-special-macro-face)))

;; add global, internal and local_persist keywords for c/c++
(add-hook 'c-mode-common-hook
	  (lambda ()
            (font-lock-add-keywords nil
				    '(("\\<\\(global_variable\\)\\>" . font-lock-keyword-face)
				      ("\\<\\(internal\\)\\>" . font-lock-keyword-face)
				      ("\\<\\(local_persist\\)\\>" . font-lock-keyword-face)))))

;;(set-variable 'grep-command "findstr -s -n -i -l ")

(set-variable 'grep-command "rg --pcre2 -H --no-heading -n -S -e ")
 (grep-apply-setting
   'grep-find-command
   '("rg --pcre2 -H --no-heading -n -S -e \"\" ./ " . 38)
   )

(defun grep-fd (command-args)
  (interactive
   (progn
     (grep-compute-defaults)
     (if grep-find-command
	 (list (read-shell-command "Run find (like this): "
                                   '("fd \"\" -x \"%HOMEDRIVE%%HOMEPATH%\\.emacs.d\\concat.bat\" {}" . 5) 'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))		; see grep
      (grep command-args))))

(global-set-key (kbd "C-x C-z") 'grep-fd)

;; (defun casey-find-corresponding-file ()
;;   "Find the file that corresponds to this one."
;;   (interactive)
;;   (setq CorrespondingFileName nil)
;;   (setq BaseFileName (file-name-sans-extension buffer-file-name))
;;   (if (string-match "\\.c" buffer-file-name)
;;       (setq CorrespondingFileName (concat BaseFileName ".h")))
;;   (if (string-match "\\.h" buffer-file-name)
;;       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
;; 	(setq CorrespondingFileName (concat BaseFileName ".cpp"))))
;;   (if (string-match "\\.hin" buffer-file-name)
;;       (setq CorrespondingFileName (concat BaseFileName ".cin")))
;;   (if (string-match "\\.cin" buffer-file-name)
;;       (setq CorrespondingFileName (concat BaseFileName ".hin")))
;;   (if (string-match "\\.cpp" buffer-file-name)
;;       (setq CorrespondingFileName (concat BaseFileName ".h")))
;;   (if CorrespondingFileName (find-file CorrespondingFileName)
;;     (error "Unable to find a corresponding file")))

(defun casey-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (setq FileExtension nil)
  (if (string-match "\\.c" buffer-file-name)
      (setq FileExtension ".h"))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq FileExtension ".c")
	(setq FileExtension ".cpp")))
  (if (string-match "\\.hin" buffer-file-name)
      (setq FileExtension ".cin"))
  (if (string-match "\\.cin" buffer-file-name)
      (setq FileExtension ".hin"))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq FileExtension ".h"))

  (setq DefDirectory default-directory)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (setq NonDirectory (file-name-nondirectory BaseFileName))
  (setq CorrespondingFileName (concat DefDirectory NonDirectory FileExtension))

  (if (string-match ".h" FileExtension)
      (if (file-exists-p CorrespondingFileName)
	  (find-file CorrespondingFileName)
	(setq CorrespondingFileName (concat DefDirectory "inc/" NonDirectory FileExtension))
	(if (file-exists-p CorrespondingFileName)
	    (find-file CorrespondingFileName)
	  (setq CorrespondingFileName (concat DefDirectory "include/" NonDirectory FileExtension))
	  (if (file-exists-p CorrespondingFileName)
	      (find-file CorrespondingFileName)
	    (setq CorrespondingFileName (concat DefDirectory "includes/" NonDirectory FileExtension))
	    (if (file-exists-p CorrespondingFileName)
		(find-file CorrespondingFileName)
	      (setq CorrespondingFileName (concat DefDirectory "../" NonDirectory FileExtension))
	      (if (file-exists-p CorrespondingFileName)
		  (find-file CorrespondingFileName)
		(setq CorrespondingFileName (concat DefDirectory "../inc/" NonDirectory FileExtension))
		(if (file-exists-p CorrespondingFileName)
		    (find-file CorrespondingFileName)
		  (setq CorrespondingFileName (concat DefDirectory "../include/" NonDirectory FileExtension))
		  (if (file-exists-p CorrespondingFileName)
		      (find-file CorrespondingFileName)
		    (setq CorrespondingFileName (concat DefDirectory "../includes/" NonDirectory FileExtension))
		    (if (file-exists-p CorrespondingFileName)
			(find-file CorrespondingFileName)
		      (error "Unable to find a corresponding file")))))))))
    (setq CorrespondingFileName (concat DefDirectory "../" NonDirectory FileExtension))
    (if (file-exists-p CorrespondingFileName)
	(find-file CorrespondingFileName)
      (setq CorrespondingFileName (concat DefDirectory "../src/" NonDirectory FileExtension))
      (if (file-exists-p CorrespondingFileName)
	  (find-file CorrespondingFileName)
	(setq CorrespondingFileName (concat DefDirectory "../source/" NonDirectory FileExtension))
	(if (file-exists-p CorrespondingFileName)
	    (find-file CorrespondingFileName)
	  (setq CorrespondingFileName (concat DefDirectory "../sources/" NonDirectory FileExtension))
	  (if (file-exists-p CorrespondingFileName)
	      (find-file CorrespondingFileName)
	    (if (string-match ".c" FileExtension)
		(setq FileExtension ".c")
	      (setq FileExtension ".cpp"))
	    (setq CorrespondingFileName (concat DefDirectory "../" NonDirectory FileExtension))
	    (if (file-exists-p CorrespondingFileName)
		(find-file CorrespondingFileName)
	      (setq CorrespondingFileName (concat DefDirectory "../src/" NonDirectory FileExtension))
	      (if (file-exists-p CorrespondingFileName)
		  (find-file CorrespondingFileName)
		(setq CorrespondingFileName (concat DefDirectory "../source/" NonDirectory FileExtension))
		(if (file-exists-p CorrespondingFileName)
		    (find-file CorrespondingFileName)
		  (setq CorrespondingFileName (concat DefDirectory "../sources/" NonDirectory FileExtension))
		  (if (file-exists-p CorrespondingFileName)
		      (find-file CorrespondingFileName)
		    (error "Unable to find a corresponding file")))))))))))
    

(defun casey-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (casey-find-corresponding-file)
  (other-window -1))

(add-hook
 'c++-mode-hook
 (lambda ()
   (local-set-key (kbd "C-x o") #'casey-find-corresponding-file)
   (local-set-key (kbd "C-x 4 o") #'casey-find-corresponding-file-other-window)
   ))


(global-set-key (kbd "C-x C-c") 'grep-find)
(global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
;; (define-key c++-mode-map (kbd "C-x o") 'casey-find-corresponding-file)
;; (define-key c++-mode-map (kbd "C-x 4 o") 'casey-find-corresponding-file-other-window)

(setq dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; (find-library xref)
;; We override these 2 functions to prevent pulsing after jumps.
(defcustom xref-after-jump-hook '(recenter)
  "Functions called after jumping to an xref." )
(defcustom xref-after-return-hook '()
  "Functions called after returning to a pre-jump location.")

;; M-s o Occur

;; Ubuntu like emacs colors for nano theme
;; (setq nano-color-foreground "#ECEFF4")
;; (setq nano-color-background "#300A24")
;; (setq nano-color-highlight  "#3D1836")
;; (setq nano-color-critical   "#EBCB8B")
;; (setq nano-color-salient    "#BA7BA6")
;; (setq nano-color-strong     "#ECEFF4")
;; (setq nano-color-popout     "#D08770")
;; (setq nano-color-subtle     "#5E4356")
;; (setq nano-color-faded      "#785065")

(global-auto-revert-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; describe-char to learn face of the char cursor stands on (maybe also describe-face, customize-face, customize-group)
;; C-M-h mark function

;; C-c C-\ c-backslash-region

;; M-s h r highlight-regexp
;; M-s h u unhighlight-regexp

;; C-x C-l downcase-region
;; C-x C-u upcase-region

;; M-! shell-command
;; M-& async-shell-command
;; M-| shell-command-on-region

;; C-M-Space mark-sexp

;; C-x n n Narrow down to between point and mark (narrow-to-region).

;; C-x n w Widen to make the entire buffer accessible again (widen).

;; C-x n p Narrow down to the current page (narrow-to-page).

;; C-x n d Narrow down to the current defun

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (if (and (boundp 'display-line-numbers-mode)
           display-line-numbers-mode)
      (call-interactively 'goto-line)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (call-interactively 'goto-line))
      (display-line-numbers-mode 0))))

;; C-x C-j go back to parent directory in dired.
(add-hook 'dired-mode-hook 'dired-omit-mode)
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line (if dired-omit-mode 2 4)))
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map
			      (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
			    (define-key dired-mode-map
			      (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
			    ))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(require 'tramp)
(setq tramp-default-method "plink")
(customize-set-variable 'tramp-syntax 'simplified)

(eval-after-load "vdiff" '(progn
			    (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
			    (setq exec-path (add-to-list 'exec-path "c:/Program Files/Git/usr/bin"))
			    (setenv "PATH" (concat (getenv "PATH") ";c:/Program Files/Git/usr/bin"))))

(setq dired-dwim-target t)

(defvar buffer-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") 'next-buffer)
    (define-key map (kbd "<left>") 'previous-buffer)
    map)
  "Keymap to repeat `next-buffer' and `previous-buffer'.  Used in `repeat-mode'.")
(put 'next-buffer 'repeat-map 'buffer-navigation-repeat-map)
(put 'previous-buffer 'repeat-map 'buffer-navigation-repeat-map)

;; make-mark-visible.el
(eval-after-load "make-mark-visible" '(progn
					(global-set-key (kbd "C-c v") 'mmv-toggle-mark-visibility)
					))

(which-function-mode 1)
(setq which-func-unknown "-")

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "C-i") 'ido-goto-symbol)

;; narrow-indirect.el
(eval-after-load "narrow-indirect" '(progn
				      (define-key ctl-x-4-map "nd" #'ni-narrow-to-defun-indirect-other-window)
				      (define-key ctl-x-4-map "nn" #'ni-narrow-to-region-indirect-other-window)
				      (define-key ctl-x-4-map "np" #'ni-narrow-to-page-indirect-other-window)
				      (setq ni-buf-name-prefix "")
				      ))

;;close git service
(setq vc-handled-backends nil)

(setq gc-cons-threshold 16777216 ; 16mb
      gc-cons-percentage 0.1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
