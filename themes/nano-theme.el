;;; nano-theme.el

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The nano theme requires Emacs 24 or later!"))

(deftheme nano "Nano")

;;(setq nano-color-foreground "#ECEFF4") ;; Snow Storm 3  / nord  6
;;(setq nano-color-background "#2E3440") ;; Polar Night 0 / nord  0
;;(setq nano-color-highlight  "#3B4252") ;; Polar Night 1 / nord  1
;;(setq nano-color-critical   "#EBCB8B") ;; Aurora        / nord 11
;;(setq nano-color-salient    "#81A1C1") ;; Frost         / nord  9
;;(setq nano-color-strong     "#ECEFF4") ;; Snow Storm 3  / nord  6
;;(setq nano-color-popout     "#D08770") ;; Aurora        / nord 12
;;(setq nano-color-subtle     "#434C5E") ;; Polar Night 2 / nord  2
;;(setq nano-color-faded      "#616E87") ;;
;;(setq nano-color-function nano-color-foreground) ;; Added new
;;(setq nano-color-highlight-line nano-color-highlight) ;; Added new
;;(setq nano-color-cursor nano-color-foreground) ;; Added new

;;(setq nano-color-foreground "#ECEFF4")
;;(setq nano-color-background "#300A24")
;;(setq nano-color-highlight  "#3D1836")
;;(setq nano-color-critical   "#EBCB8B")
;;(setq nano-color-salient    "#BA7BA6") ;;  ;;#c181a3 ;;#BA7BA6
;;(setq nano-color-strong     "#ECEFF4")
;;(setq nano-color-popout     "#D08770")
;;(setq nano-color-subtle     "#5E4356")
;;(setq nano-color-faded      "#785065")
;;(setq nano-color-function nano-color-foreground) ;; Added new
;;(setq nano-color-highlight-line nano-color-highlight) ;; Added new
;;(setq nano-color-cursor nano-color-foreground) ;; Added new

;;(setq nano-color-foreground "#ECEFF4")
;;(setq nano-color-background "#262334")
;;(setq nano-color-highlight  "#423D53")
;;(setq nano-color-critical   "#D84D61")
;;(setq nano-color-salient    "#A893D1")
;;(setq nano-color-strong     "#C9AFFF")
;;(setq nano-color-popout     "#FBD25A") ;;#FF977B
;;(setq nano-color-subtle     "#5A5470") ;;#44404F
;;(setq nano-color-faded      "#8688C1")
;;(setq nano-color-function nano-color-foreground) ;; Added new
;;(setq nano-color-highlight-line nano-color-highlight) ;; Added new
;;(setq nano-color-cursor nano-color-foreground) ;; Added new

;;(setq nano-color-foreground "#37474F")
;;(setq nano-color-background "#FFFFFF")
;;(setq nano-color-highlight  "#CCD5F0")
;;(setq nano-color-critical   "#D84D61")
;;(setq nano-color-salient    "#40568D")
;;(setq nano-color-strong     "#C9AFFF")
;;(setq nano-color-popout     "#A31515")
;;(setq nano-color-subtle     "#BFCDDB")
;;(setq nano-color-faded      "#5D6B99")
;;(setq nano-color-highlight-line "#F5CC84")
;;(setq nano-color-function "#74531F");; #8A1BFF
;;(setq nano-color-cursor nano-color-foreground) ;; Added new

(setq nano-color-foreground "#D4D4D4")
;;(setq nano-color-background "#1E1E1E")
;;(setq nano-color-background "#1F1F1F")
(setq nano-color-background "#1D1D1D")
(setq nano-color-highlight  "#343434");; "#9CDCFE" #4EC9B0 #FFE7C2
(setq nano-color-critical   "#D16969")
(setq nano-color-salient    "#4EC9B0")
(setq nano-color-strong     "#C9AFFF")
(setq nano-color-popout     "#F5534B")
(setq nano-color-subtle     "#606060")
(setq nano-color-faded      "#707070") ;;#2E7869
(setq nano-color-function   "#FFE7C2")
(setq nano-color-highlight-line "#343434") ;; Added new
(setq nano-color-cursor     "#4EC9B0") ;; Added new


(setq my-font-name         "Roboto Mono") ;; if you enable this add :weight bold option to line 124
;;(setq my-font-name         "PxPlus ToshibaSat 8x16:pixelsize=16:dpi=96:autohint=false:hinting=false:antialias=standard")

(let  ((background 	nano-color-background)
       (gutters    	nano-color-background)
       (gutter-fg  	nano-color-background)
       (gutters-active 	nano-color-background)
       (builtin      	nano-color-salient)
       (selection  	nano-color-subtle)
       (text       	nano-color-foreground)
       (comments   	nano-color-faded)
       (punctuation 	nano-color-salient)
       (keywords 	nano-color-salient)
       (variables 	nano-color-foreground)
       (functions 	nano-color-function);; nano-color-foreground
       (methods    	nano-color-foreground) ;; #c1d1e3
       (strings    	nano-color-popout)
       (constants 	nano-color-salient)
       (macros 		nano-color-salient)
       (numbers 	nano-color-salient)
       (white     	nano-color-foreground)
       (error 		nano-color-critical)
       (warning 	nano-color-critical)
       (highlight-line 	nano-color-highlight-line) ;;nano-color-highlight
       (line-fg 	nano-color-subtle))

  (custom-theme-set-faces
   'nano

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,nano-color-foreground :background ,nano-color-background :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,nano-color-cursor)))) ;; ,nano-color-foreground
   `(linum                            ((t (:background ,nano-color-background :foreground ,nano-color-foreground))))
   `(highlight                        ((t (:foreground nil :background ,nano-color-subtle))))
   `(link                             ((t (:foreground ,nano-color-salient :weight light))))
   `(lazy-highlight                   ((t (:background ,nano-color-subtle :weight light))))
   `(isearch                          ((t (:foreground ,nano-color-foreground :family ,my-font-name :weight medium))))
   `(isearch-fail                     ((t (:foreground ,nano-color-faded))))
   `(minibuffer-prompt                ((t (:foreground ,nano-color-foreground :family ,my-font-name :weight medium))))
   `(ido-subdir                       ((t (:foreground ,nano-color-foreground)))) ;; Face used by ido for highlighting subdirs in the alternatives.
   `(ido-first-match                  ((t (:foreground ,nano-color-salient :weight bold)))) ;; Face used by ido for highlighting first match.
   `(ido-only-match                   ((t (:foreground ,nano-color-faded :weight bold))))
   `(compilation-mode-line-exit       ((t (:foreground ,nano-color-salient :weight bold))))
   `(compilation-mode-line-fail       ((t (:foreground ,nano-color-critical :weight bold))))
   `(success                          ((t (:foreground ,nano-color-salient :weight bold))))
   `(warning                          ((t (:foreground ,nano-color-popout :weight bold))))
   `(error                            ((t (:foreground ,nano-color-critical :weight bold))))
   `(match                            ((t (:foreground ,nano-color-popout :weight bold))))
   `(window-divider                   ((t (:foreground ,nano-color-background))))
   `(window-divider-first-pixel       ((t (:foreground ,nano-color-highlight))))
   `(window-divider-last-pixel        ((t (:foreground ,nano-color-highlight))))
   `(fringe                           ((t (:foreground ,nano-color-faded :weight light)))) ;faded
   `(help-key-binding                 ((t (:foreground ,nano-color-faded :background ,nano-color-highlight :weight normal))))
   `(completions-common-part          ((t (:foreground ,nano-color-faded :weight normal))))
   `(custom-group-tag                 ((t (:foreground ,nano-color-salient :weight normal))))
   `(custom-variable-tag              ((t (:foreground ,nano-color-salient :weight normal))))
   

   ;; Font lock faces
   ;; *****************************************************************************

   (font-lock-add-keywords 'c++-mode
			   '(("\\(\\w+\\)\\s-*\("
			      (1 font-lock-function-name-face)))
			   t)
   
   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

    ;; --- ANSI colors ----------------------------------------------------

    ;;'(ansi-color-black                       ((t (:inherit nano-default))))
    ;;'(ansi-color-bold                         ((t (:inherit nano-strong))))
    ;;'(ansi-color-bright-black                 ((t (:inherit nano-strong))))
    ;;'(ansi-color-faint                         ((t (:inherit nano-faded))))
    ;;'(ansi-color-fast-blink                    ((t (:inherit nano-faded))))
    ;;'(ansi-color-slow-blink                    ((t (:inherit nano-faded))))
    ;;'(ansi-color-inverse                   ((t (:inherit nano-default-i))))
    ;;'(ansi-color-italic                            ((t (:inherit italic))))
    ;;'(ansi-color-underline                     ((t (:inherit nano-faded))))
    ;;'(ansi-color-blue           ((t (:foreground "#42A5F5")))) ;; material color blue L400
    ;;'(ansi-color-bright-blue    ((t (:background "#BBDEFB")))) ;; material color blue L100
    ;;'(ansi-color-cyan           ((t (:foreground "#26C6DA")))) ;; material color cyan L400
    ;;'(ansi-color-bright-cyan    ((t (:background "#B2EBF2")))) ;; material color cyan L100
    ;;'(ansi-color-green          ((t (:foreground "#66BB6A")))) ;; material color green L400
    ;;'(ansi-color-bright-green   ((t (:background "#C8E6C9")))) ;; material color green L100
    ;;'(ansi-color-magenta        ((t (:foreground "#AB47BC")))) ;; material color purple L400
    ;;'(ansi-color-bright-magenta ((t (:background "#E1BEE7")))) ;; material color purple L100
    ;;'(ansi-color-red            ((t (:foreground "#EF5350")))) ;; material color red L400
    ;;'(ansi-color-bright-red     ((t (:background "#FFCDD2")))) ;; material color red L100
   ;;'(ansi-color-white          ((t (:inherit nano-color-subtle))))
      '(ansi-color-bright-white          ((t (:inherit nano-color-background))))
    ;;'(ansi-color-bright-white   ((t (:inherit default))))
    ;;'(ansi-color-yellow         ((t (:foreground "#FFEE58")))) ;; material color yellow L400
    ;;'(ansi-color-bright-yellow  ((t (:background "#FFF9C4")))) ;; material color yellow L100

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,nano-color-critical))))
   `(whitespace-trailing ((t (:background nil :foreground ,nano-color-critical :inverse-video t))))

   `(linum                           ((t (:foreground ,nano-color-subtle :background ,background))))
   `(linum-relative-current-face     ((t (:foreground ,nano-color-foreground :background ,nano-color-background))))
   `(line-number                     ((t (:foreground ,nano-color-subtle :background ,nano-color-background))))
   `(line-number-current-line        ((t (:foreground ,nano-color-foreground :background ,nano-color-background))))

   ;; hl-line-mode
   `(hl-line      ((t (:background ,nano-color-highlight-line)))) ;;,nano-color-highlight
   `(hl-line-face ((t (:background ,nano-color-highlight-line)))) ;;,nano-color-highlight

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,nano-color-foreground))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,nano-color-critical))))
   `(show-paren-match ((t (:foreground ,nano-color-popout))))
   `(show-paren-mismatch ((t (:foreground ,nano-color-critical))))
   
    ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,nano-color-background :distant-foreground ,nano-color-foreground :text ,nano-color-foreground :family ,my-font-name :weight bold)))) ;; add bold roboto-mono :weight bold
   `(mode-line ((t (:foreground ,nano-color-faded
                    :background ,nano-color-highlight
 		    :overline nil
		    :underline nil
                    :box nil))))
   ;; buttons on mode-line
   `(mode-line-highlight ((t (:foreground ,nano-color-faded
                              :background ,nano-color-background
 		              :overline nil
		              :underline nil
                              :box `(:line-width 1
                                       :color ,nano-color-background
                                       :style nil)
))))

   ;;`(powerline-active1 ((t (:background ,nano-color-subtle :foreground ,nano-color-foreground))))
   ;;`(powerline-active2 ((t (:background ,nano-color-subtle :foreground ,nano-color-foreground))))

   `(mode-line-inactive ((t (:foreground ,nano-color-faded
                             :background ,nano-color-background
 		             :overline nil
		             :underline nil
                             :box nil))))

   `(header-line ((t (:weight light
                    :foreground ,nano-color-foreground
                    :background ,nano-color-background
                    :overline nil
                    :underline nil
                    :box nil
                    :box `(:line-width 1
                                       :color ,nano-color-background
                                       :style nil)
                    :inherit nil))))

   `(internal-border ((t (:background ,nano-color-background))))

   ;;`(powerline-inactive1 ((t (:background ,background :foreground ,nano-color-foreground))))
   ;;`(powerline-inactive2 ((t (:background ,background :foreground ,nano-color-foreground))))


   `(custom-button ((t (:foreground ,nano-color-faded
                        :background ,nano-color-background
                         :box `(:line-width 1
                                            :color ,nano-color-faded
                                            :style nil)))))
                         
   `(custom-button-mouse ((t (;;:inherit 'custom-button
                                :foreground ,nano-color-faded
                                :background ,nano-color-subtle
                                :box `(:line-width 1
                                            :color ,nano-color-faded
                                            :style nil)))))
                                 
   `(custom-button-pressed ((t (:foreground ,nano-color-background
                                :background ,nano-color-salient
                                ;;:inherit `(:foreground ,nano-color-salient :weight light)
                                :box `(:line-width 1
                                       :color ,nano-color-salient
                                       :style nil)
                                :inverse-video nil))))

    ;; for example state field in customize face settings
    `(custom-state ((t (:foreground ,nano-color-salient :weight light))))
    `(custom-modified ((t (:foreground ,nano-color-salient :weight light))))
    `(custom-changed ((t (:foreground ,nano-color-salient :weight light))))

    ;; for example search field in customize-face settings
    `(widget-field ((t (:background ,nano-color-subtle))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,nano-color-salient))))
   )

  (custom-theme-set-variables
   'nano
   '(linum-format " %5i ")
   )
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'nano)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'nano-theme)

;;; nano-theme.el ends here
