;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs                                                                    ;;
;; Author: Antonello Salis                                                   ;;
;; Licence: GNU public licence                                               ;; 
;; Ver. 20201218.02                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DISCLAIMER: All this content has been copied and adapted from              ;;
;;            several sources. None of it  is being developed or mantained   ;;
;;            directly by me.                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: EMACS settings                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goes through the list of installed packages and tries to run their auto-loads .
(package-initialize)

(package-refresh-contents)

(package-install 'use-package)

;; The following 3 lines disable unnecessary GUI elements, in this case the
;; menu bar, the tool bar and the scroll bar. If you wish, you can comment out
;; the menu-bar and keep it, but eventually I recommend you disable it.
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(global-display-line-numbers-mode)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'monokai t)

;; NANO Section starts here
;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/home/antonello/.emacs.d/nano-emacs-master")
(add-to-list 'load-path ".")
;; Window layout (optional)
(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (require 'nano-theme-dark))
 (t (require 'nano-theme-light)))

;;;NANO Dark
(require 'nano-base-colors)

(defun nano-theme-set-dark ()
  "Apply dark Nano theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq nano-color-foreground "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq nano-color-background "#2E3440") ;; Polar Night 0 / nord  0
  (setq nano-color-highlight  "#3B4252") ;; Polar Night 1 / nord  1
  (setq nano-color-critical   "#EBCB8B") ;; Aurora        / nord 11
  (setq nano-color-salient    "#81A1C1") ;; Frost         / nord  9
  (setq nano-color-strong     "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq nano-color-popout     "#D08770") ;; Aurora        / nord 12
  (setq nano-color-subtle     "#434C5E") ;; Polar Night 2 / nord  2
  (setq nano-color-faded      "#677691") ;;
  )

(nano-theme-set-dark)

(provide 'nano-theme-dark)

;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "nano-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; Theme
(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

;; Nano default settings (optional)
(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)

;; Welcome message (optional)
;(let ((inhibit-message t))
;  (message "Welcome to GNU Emacs / N Λ N O edition")
;  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
;(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
;(unless (member "-no-splash" command-line-args)
;  (require 'nano-splash))

;; Help (optional)
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

(provide 'nano)
(setq line-spacing .2)

;;NANO Section Ends here

;; ;Ivy
;; (ivy-mode 1)
;; (use-package ivy-prescient
;;   :after counsel
;;   :config
;;   (ivy-prescient-mode 1))

;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; ;Company
;; (use-package company-prescient
;;   :after company
;;   :config
;;   (company-prescient-mode 1))
;;Avy
(global-set-key (kbd "C-$") 'avy-goto-char-2)

;;Window maximized on start
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; F11 = Full Screen
 (defun toggle-fullscreen (&optional f)
 (interactive)

  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;;Set the username
(setq user-full-name "Antonello Salis")

;; use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;;Remove bell sound and visulize it instead
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Emacs comes with a built-in text based browser.
(setq browse-url-browser-function 'eww-browse-url)

;;IDO
(setq ido-enable-flex-matching t)
(ido-mode 1)
(setq ido-everywhere t)

;;Recent files list
(require 'recentf)
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; enable recent files mode.
(recentf-mode t)
; 50 files ought to be enougof recent items.
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list)) 
     (message "Opening file...")
    (message "Aborting")))

(require 'auto-complete)
(global-auto-complete-mode t)

;;autocomplete ess
(require 'ess-site)
(require 'ess-r-mode)
(setq ess-use-auto-complete t)

(defun my-auto-hook ()
  (auto-complete-mode 1)
  ;; Colors
  (set-face-attribute 'popup-tip-face nil :background "#bfbaac" :foreground "black")
  (define-key ac-completing-map [return] nil)
  (define-key ac-completing-map "\r" nil)
  )
(add-hook 'ess-mode-hook 'my-auto-hook)
(add-hook 'inferior-ess-mode-hook 'my-auto-hook)

(ac-config-default)
(setq ess-use-auto-complete t)

;; use ESS
(require 'ess-r-mode)
 (use-package ess
  :ensure t
  :init (require 'ess-site))

(setq global-visual-line-mode 0) ; 1 for on, 0 for off.

;; complete brackets and quotes etc.l
(electric-pair-mode 1)

;;auto complete in ess
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
(define-key ess-mode-map (kbd "TAB") 'ac-complete-filename)
;; Get tab completion in R script files
;; See this page here
;; https://stat.ethz.ch/pipermail/ess-help/2013-March/008719.html
;; Make sure that this is after the auto-complete package initialization
(setq  ess-tab-complete-in-script t)

;; Find files faster
;; (require 'deft)
;; (setq deft-recursive t)
;; (setq deft-use-filename-as-title t)
;; (setq deft-extensions '("org"))
;; (setq deft-directory "~/")
;; ;; Deft search
;; (global-set-key [f9] 'deft)

;;Change Yes and No with y and n
(fset 'yes-or-no-p 'y-or-n-p)

;;Winner-mode C-c left and right to undo
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; save history after closing the session
(savehist-mode 1)

;;wrap text visually
(global-visual-line-mode 1) ;1 for on, 0 for off.

;;guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x t" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode
(require 'which-key)
(which-key-setup-side-window-right)
(which-key-mode 1)

;;Title of the buffer on top
(setq frame-title-format "%b")

;; Auto completion eshell
(setq eshell-cmpl-cycle-completions nil)

;; Magit
;(global-set-key (kbd "C-x g") 'magit-status)

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)


;smartparents
(require 'smartparens-config)


;;Using paradox package for updates
(use-package paradox
  :init
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Reporsitories                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;emacs repositories
(package-refresh-contents t)

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

(add-to-list' package-archives 
	      '("gnu" . "http://elpa.gnu.org/packages/")t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Dashboard                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Antonello Salis")
;; Set the banner
;(setq dashboard-startup-banner 'logo)
(setq dashboard-startup-banner "/home/antonello/.emacs.d/EN.png")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

;; Content is not centered by default. To center, set
;(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
;(setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 15)
                        (bookmarks . 5)
;                        (ToDo . 5)
;                        (agenda . 5)
;                        (registers . 5)))
))


(setq dashboard-week-agenda t)

;(defun dashboard-insert-custom (list-size)
;  (insert "ToDo"))
;(add-to-list 'dashboard-item-generators  '(org-todo-list . dashboard-insert-custom))
;(add-to-list 'dashboard-items '(org-todo-list) t)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(dashboard-modify-heading-icons '((recents . "rocket")
                                  (bookmarks . "package")))

(setq dashboard-set-navigator t)

; Format: "(icon title help action face prefix suffix)"
;(setq dashboard-navigator-buttons
;      `(;; line1
;        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
;         "Homepage"
;         "Browse homepage"
;         (lambda (&rest _) (browse-url "homepage")))
;        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
;        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
;        ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
;          "Linkedin"
;          ""
;          (lambda (&rest _) (browse-url "homepage")))
;         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))



(setq dashboard-set-footer nil)

;(add-to-list 'dashboard-items '(agenda) t)

;(setq dashboard-week-agenda t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Org-mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;just a fancy way to see text
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-with-inline-images t)

(require 'org)
(require 'org-journal)
(setq org-log-done t)
;;Org-mode-agenda files
(setq org-agenda-files (list "~/Agenda/todo.org"
			     "~/Agenda/Meetings.org"
;			     "~/Agenda/Projects.org"
			     "~/org/Emacs_key.org"
			     ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "Follow-Up" "|" "DONE" "CANCELED" "POSSIBLE")))

;;Open ToDo list  at start
;(org-todo-list)
;(setq inhibit-splash-screen t)

;;Capture the ToDoS in the various documents
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Agenda/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Agenda/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
	))

(require 'ox-reveal)
;;reveal for the presentations in Org-mode
(setq org-reveal-root "file:///home/antonello/reveal.js/")

;;Agenda
(setq org-agenda-include-diary t)
;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;;Flyspell for the inline word correction 
(add-hook 'org-mode-hook 'turn-on-flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Undo-tree                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)
(add-to-list 'load-path "/emacstuff/neotree")
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'nerd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: polymode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (unless (package-installed-p 'polymode)
;;   (package-install 'poly-markdown))

;; https://polymode.github.io/installation/


(use-package poly-markdown
       :ensure t)
     (use-package poly-R
       :ensure t)


;; https://github.com/vspinu/polymode
(use-package polymode
       :diminish (poly-org-mode
              poly-markdown-mode
              poly-noweb+r-mode
              poly-noweb+r-mode
              poly-markdown+r-mode
              poly-rapport-mode
              poly-html+r-mode
              poly-brew+r-mode
              poly-r+c++-mode
              poly-c++r-mode)
       :init
       (require 'poly-R)
       (require 'poly-markdown)
       :config
       (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
       (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
       (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
       (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
       )


;; (use-package polymode
;;     :mode
;;     (("\\.Rmd" . poly-markdown+r-mode))
;;     :init
;;     (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
;;     :defer t
;;     )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: google-translate                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-translation-directions-alist
      '(("it" . "en") ("en" . "it") ("it" . "fr") ("fr" . "it")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Latex                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auctex settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: ibuffer                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(popup flycheck good-scroll avy multiple-cursors counsel prescient ess-view ess-R-data-view poly-markdown polymode poly-org ox-tufte paradox auto-package-update all-the-icons-gnus all-the-icons dashboard-hackernews dashboard-project-status dashboard solarized-theme projectile pdf-tools markdown-toc magit htmlize flx-ido ess which-key use-package undo-tree smartparens poly-R ox-reveal org-journal org-bullets neotree monokai-theme guide-key google-translate deft auto-complete))
 '(paradox-github-token t)
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
