;;; monoglow-theme.el --- Mono Glow-inspired theme  -*- lexical-binding: t; -*-

;; Author: nukoseer <uygarkoseer@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: faces, theme

;;; Commentary:
;; A dark, near-monochrome theme with one bright “glow” accent colour.
;; This is a starting template you can tweak and iterate on.
;;
;; 1.  Save this file somewhere on your load-path, e.g. ~/.emacs.d/themes/
;; 2.  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; 3.  M-x load-theme RET monoglow RET
;; 4.  Adjust palette or faces below, then M-x eval-buffer to test.

;;; Code:
(require 'cl-lib)

;;;; Helper functions ---------------------------------------------------------

(defun monoglow-hex->rgb (hex)
  "Return (R G B) list of integers 0–255 for a HEX string like \"#rrggbb\"."
  (let* ((clean (replace-regexp-in-string "#" "" hex)))
    (mapcar (lambda (i)
              (string-to-number (substring clean i (+ i 2)) 16))
            '(0 2 4))))

(defun monoglow-round (x)
  "Round X to nearest integer using the classic +0.5/floor trick."
  (floor (+ x 0.5)))

(defun monoglow-blend (foreground alpha background)
  (interactive)
  "Blend FOREGROUND over BACKGROUND with opacity ALPHA (0–1 or \"cc\")."
  (let* ((alpha (if (stringp alpha)
                    (/ (string-to-number alpha 16) 255.0)
                  alpha))
         (fg (monoglow-hex->rgb foreground))
         (bg (monoglow-hex->rgb background))
         (blend (cl-mapcar (lambda (f b)
                             (monoglow-round (+ (* alpha f) (* (- 1 alpha) b))))
                           fg bg)))
    (apply #'format "#%02x%02x%02x" blend)))

;;;; Palette ------------------------------------------------------------------

(defgroup monoglow nil
  "Mono Glow-inspired theme."
  :group 'faces)

(defcustom monoglow-style 'z
  "Brightness variant to use.
'z   – original Mono Glow\n'void – jet-black background\n'lack – slightly warmer black."
  :type '(choice (const :tag "Z (default)" z)
                 (const :tag "Void" void)
                 (const :tag "Lack" lack))
  :group 'monoglow)

(defconst monoglow-palette
  '((z
     :gray1 "#080808" :gray2 "#191919" :gray3 "#2a2a2a" :gray4 "#444444"
     :gray5 "#555555" :gray6 "#7a7a7a" :gray7 "#aaaaaa" :gray8 "#cccccc"
     :gray9 "#dddddd" :gray10 "#f1f1f1" :modeline "#1c1c1c"
     :white "#ffffff" :black "#000000" :fg "#cccccc":bg "#121212" :glow "#1bfd9c")

    (void :bg "#000000" :bg-alt "#0a0a0a" :fg "#c0c0c0" :glow "#7affc1"
          :red "#ff5f5f" :orange "#ff875f" :yellow "#ffd75f" :green "#5fff87"
          :cyan "#5fd7ff" :blue "#5fafff" :magenta "#d787ff" :comment "#5e5e5e")
    (lack :bg "#050505" :bg-alt "#101010" :fg "#cecece" :glow "#7affc1"
          :red "#ff5f5f" :orange "#ff875f" :yellow "#ffd75f" :green "#5fff87"
          :cyan "#5fd7ff" :blue "#5fafff" :magenta "#d787ff" :comment "#616161"))
  "Association list of palette variants.")

(defun monoglow-color (key)
  "Fetch colour KEY from the current style palette."
  (plist-get (cdr (assq monoglow-style monoglow-palette)) key))

;;;; Theme definition ---------------------------------------------------------

(deftheme monoglow "Monochrome base with glow accent.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'monoglow

   ;; Basic faces -------------------------------------------------------------
   `(default ((,class (:background ,(monoglow-color :bg)
                                   :foreground ,(monoglow-color :fg)))))
   `(cursor  ((,class (:background ,(monoglow-color :glow)))))
   `(region  ((,class (:background ,(monoglow-color :white) :foreground ,(monoglow-color :black)))))
   `(fringe  ((,class (:background ,(monoglow-color :bg)))))

   ;; Font-lock ---------------------------------------------------------------
   `(font-lock-builtin-face       ((,class (:foreground ,(monoglow-color :gray5)))))
   `(font-lock-comment-face       ((,class (:foreground ,(monoglow-blend (monoglow-color :gray4) 0.95 (monoglow-color :bg)) :slant italic))))
   `(font-lock-doc-face           ((,class (:foreground ,(monoglow-blend (monoglow-color :gray4) 0.95 (monoglow-color :bg)) :slant italic))))
   `(font-lock-constant-face      ((,class (:foreground ,(monoglow-color :gray7)))))
   `(font-lock-function-name-face ((,class (:foreground ,(monoglow-color :gray8)))))
   `(font-lock-function-call-face ((,class (:foreground ,(monoglow-color :gray6)))))
   `(font-lock-keyword-face       ((,class (:foreground ,(monoglow-blend (monoglow-color :gray6) 0.90 (monoglow-color :bg)) :weight regular))))
   `(font-lock-string-face        ((,class (:foreground ,(monoglow-color :gray7)))))
   `(font-lock-type-face          ((,class (:foreground ,(monoglow-color :gray7)))))
   `(font-lock-number-face        ((,class (:foreground ,(monoglow-color :gray7)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(monoglow-color :gray8)))))
   `(font-lock-warning-face       ((,class (:foreground ,(monoglow-color :gray4) :weight bold))))
   `(font-lock-operator-face      ((,class (:foreground ,(monoglow-color :glow) :weight bold))))
   `(font-lock-preprocessor-face  ((,class (:foreground ,(monoglow-blend (monoglow-color :gray5) 0.70 (monoglow-color :fg))))))
   ;;`(font-lock-property-name-face  ((,class :foreground ,(monoglow-color :glow))))
   `(font-lock-property-use-face  ((,class :foreground ,(monoglow-blend (monoglow-color :gray7) 0.80 (monoglow-color :bg)))))

   ;; UI ----------------------------------------------------------------------
   `(mode-line            ((,class (:foreground ,(monoglow-color :fg) :background ,(monoglow-color :modeline)))))
   `(mode-line-inactive   ((,class ( :foreground ,(monoglow-color :gray4) :background ,(monoglow-blend (monoglow-color :modeline) 0.95 (monoglow-color :bg))))))
   
   `(nano-modeline-active     ((,class (:foreground ,(monoglow-color :fg) :background ,(monoglow-color :modeline)))))
   `(nano-modeline-status     ((,class (:foreground ,(monoglow-color :glow)))))
   `(nano-modeline-inactive   ((,class (:foreground ,(monoglow-color :gray4) :background ,(monoglow-blend (monoglow-color :modeline) 0.95 (monoglow-color :bg))))))

   `(minibuffer-prompt ((,class :foreground ,(monoglow-color :glow))))
   
   `(vertical-border            ((,class (:foreground ,(monoglow-color :modeline)))))
   `(window-divider             ((,class :foreground ,(monoglow-color :bg))))
   `(window-divider-first-pixel ((,class :foreground ,(monoglow-color :modeline))))
   `(window-divider-last-pixel  ((,class :foreground ,(monoglow-color :modeline))))

   `(hl-line                    ((,class :background ,(monoglow-color :modeline))))
   `(highlight                  ((,class :background ,(monoglow-color :modeline))))
   `(show-paren-match           ((,class :foreground ,(monoglow-color :glow))))

   `(isearch                    ((,class :foreground ,(monoglow-color :bg) :background ,(monoglow-color :glow))))
   `(lazy-highlight             ((,class :background ,(monoglow-color :gray4))))

   `(completions-annotations    ((,class :foreground ,(monoglow-color :gray4) :slant italic :underline nil)))
   `(link                       ((,class :foreground ,(monoglow-color :glow))))
   `(match                      ((,class :foreground ,(monoglow-color :glow))))
   `(success                    ((,class :foreground ,(monoglow-color :glow) :underline t)))
   
   `(orderless-match-face-0     ((,class :foreground ,(monoglow-color :glow))))

   `(which-key-key-face         ((,class :foreground ,(monoglow-color :glow))))
   `(help-key-binding           ((,class :foreground ,(monoglow-color :glow))))

   `(fancy-dabbrev-preview-face   ((,class :underline t)))
   `(fancy-dabbrev-menu-face      ((,class :foreground ,(monoglow-color :gray7) :background ,(monoglow-color :modeline)))) ;; :box (:line-width -1 :color ,(monoglow-color :gray4))
   `(fancy-dabbrev-selection-face ((,class :foreground ,(monoglow-color :black) :background ,(monoglow-color :glow) :weight bold)))

   `(diff-header      ((,class :foreground ,(monoglow-color :gray6) :background ,(monoglow-color :bg))))
   `(diff-file-header ((,class :foreground ,(monoglow-color :gray6) :background ,(monoglow-color :bg))))

   `(vundo-highlight ((,class :foreground ,(monoglow-color :glow))))
   
   `(embark-target ((,class :background ,(monoglow-color :gray3))))
   ))

;;(defun my/unhighlight-dot ()
;;  "Give '.' the normal face, overriding `font-lock-operator-face'."
;;  (font-lock-add-keywords
;;   nil
;;   '(("\\." 0 'default t))   ; t = OVERRIDE (replace any face already set)
;;   'prepend))                ; make it the first rule evaluated
;;
;;(add-hook 'prog-mode-hook #'my/unhighlight-dot)

;;(add-to-list 'treesit-font-lock-settings '(punctuation.delimiter @font-lock-punctuation-face))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))

(provide-theme 'monoglow)

;;; monoglow-theme.el ends here
