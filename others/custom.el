;; -*- lexical-binding: t; -*-

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

(defun running-in-wsl-p ()
  "Check if Emacs is running inside WSL by environment variables."
  (getenv "WSL_DISTRO_NAME"))

(defun my-comint-path-rewrite (output)
  "Rewrite Windows-style paths (C:\\) to WSL paths (/mnt/c/) in comint buffers."
  (let ((start (or comint-last-output-start (point-min))))
    (save-excursion
      (goto-char start)
      ;; Replace `C:\` with `/mnt/c/`
      (while (re-search-forward "C:\\\\" nil t) ;; `nil` for `end` ensures it searches to the updated `point-max`
        (replace-match "/mnt/c/"))
      ;; Replace all remaining `\` with `/`
      (goto-char start)
      (while (re-search-forward "\\\\" nil t)
        (replace-match "/")))))

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

(if (running-in-wsl-p)
    (progn
      (setq project-base-script "./build.sh")
      (setq project-base-run-script "./run.sh"))
  (progn
    (setq project-base-script "build.bat")
    (setq project-base-run-script "run.bat")))

(setq compilation-directory-locked nil)

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p project-base-script) t
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
      (if (file-exists-p project-base-script)
	  (compile project-base-script)))
  (other-window 1))

(defun run ()
  "Run the current build."
  (interactive)
  (if (find-project-directory) (compile project-base-run-script))
  (other-window 1))


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

(defun increment-number-at-point (&optional n)
  "Increment the first number in the symbol at point by N (default 1)."
  (interactive "p")
  (setq n (or n 1))
  (setq beginning-point (point))
  (save-excursion
    (pcase-let* ((`(,beg . ,end)
                  (or (bounds-of-thing-at-point 'symbol)
                      (user-error "No symbol at point")))
                 (sym (buffer-substring-no-properties beg end)))
      (unless (string-match "\\([-+]?[0-9]+\\)" sym)
        (user-error "Symbol ‘%s’ contains no number" sym))
      (let* ((num-start (match-beginning 0))
             (num-end   (match-end 0))
             (prefix    (substring sym 0 num-start))
             (number    (string-to-number (match-string 1 sym)))
             (suffix    (substring sym num-end))
             )
        (if (eq ?- (char-before beg))
            (progn
              (setq beg (- beg 1))
              (setq number (* -1 number))
              (if (or (eq -1 number) (eq 0 number))
                  (setq beginning-point (- beginning-point 1))))
          n)
        (setq new-sym (format "%s%d%s" prefix (+ number n) suffix))
        (delete-region beg end)
        (insert new-sym))
      ))
  (goto-char beginning-point))

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
          (list (propertize "%b" 'face 'mode-line-buffer-id)))))

(add-hook 'buffer-list-update-hook 'my-update-mode-line-buffer-identification)

(defun my-theme-customizations ()
  ;; (set-face-attribute 'window-divider nil :foreground (face-background 'default) :background (face-background 'default))
  ;; (set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'mode-line))
  ;; (set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'mode-line))
  ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground (face-foreground 'default))
  
  ;; (face-spec-set 'ansi-color-bright-white
  ;;     	   '((t (:background "gray55" :foreground "gray55"))))
  
  ;; (face-spec-set 'mode-line
  ;;     	   '((t (:box (:style released-button)))))
  
  ;; (with-eval-after-load 'fancy-dabbrev
  ;;   (set-face-attribute 'fancy-dabbrev-preview-face nil :background (face-background 'hl-line) :foreground (face-foreground 'default))
  ;;   (set-face-attribute 'fancy-dabbrev-menu-face nil :background (face-background 'default) :foreground (face-foreground 'default))
  ;;   (set-face-attribute 'fancy-dabbrev-selection-face nil :background (face-background 'region) :foreground (face-foreground 'font-lock-type-face)))
  )

;; This hook is called after emacsclient creates a frame.
(add-hook 'server-after-make-frame-hook '(lambda ()
					   (startup-screen)
					   (my-theme-customizations)))

;; highlighting for TODO and NOTE
(setq fixme-modes '(c-mode c++-mode c-ts-mode c++-ts-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-study-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO:\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE:\\)" 1 'font-lock-note-face t)
	   ("\\<\\(IMPORTANT:\\)" 1 'font-lock-important-face t)
	   ("\\<\\(STUDY:\\)" 1 'font-lock-study-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil nil nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil nil nil nil)
(modify-face 'font-lock-important-face "Orange" nil nil t nil nil nil nil)
(modify-face 'font-lock-study-face "Orange" nil nil t nil nil nil nil)

;; This is useful for code blocks in org mode. Idk how to use tree-sitter in org mode for code blocks.
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
  (local-set-key (kbd "<C-tab>") #'indent-for-tab-command)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
