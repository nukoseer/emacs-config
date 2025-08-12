;; -*- lexical-binding: t; -*-

(add-hook 'emacs-startup-hook
          (let ((old-list file-name-handler-alist)
                (threshold (* 2 gc-cons-threshold))
                (percentage gc-cons-percentage))
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist old-list
                    gc-cons-threshold threshold
                    gc-cons-percentage percentage)
              (garbage-collect)))
          t)

(setq native-comp-speed 3
      package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 1024 1024) ;; 1mb
      source-directory nil
      vc-handled-backends nil
      widget-image-enable nil
      frame-inhibit-implied-resize t)

(setq default-frame-alist
      (append (list
	       '(font . "IosevkaTerm NFM-12") ;; "Iosevka Fixed-12"
	       '(internal-border-width . 0)
	       '(left-fringe  . 12)
	       '(right-fringe . 12))))

;; disable auto save mode
(setq auto-save-default nil
      ;; disable back up
      make-backup-file-name-function (quote ignore)
      make-backup-files nil)

(setq ring-bell-function 'ignore)

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
(delete-selection-mode t)

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
