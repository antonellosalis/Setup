
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs                                                                    ;;
;; Author: Antonello Salis                                                   ;;
;; Licence: GNU public licence                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DISCLAIMER: All this content has been copied and adapted from              ;;
;;            several sources. None of it  is being developed or mantained   ;;
;;            directly by me.                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Generic settings                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.

;; Goes through the list of installed packages and tries to run their auto-loads .
(package-initialize)

;; The following 3 lines disable unnecessary GUI elements, in this case the
;; menu bar, the tool bar and the scroll bar. If you wish, you can comment out
;; the menu-bar and keep it, but eventually I recommend you disable it.
;; global-linum-mode adds line numbers to all open windows.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;linum mode not enabled as not compatible with pdf-tool
;;(if (fboundp 'global-linum-mode) (global-linum-mode 1))
;;Monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)
      
;;Window maximized on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))
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

;; Emacs comes with a built-in text based browser.
(setq browse-url-browser-function 'eww-browse-url)

;;IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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
(setq ess-use-auto-complete 'script-only)

(setq global-visual-line-mode 1) ; 1 for on, 0 for off.

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
(require 'deft)
(setq deft-recursive t)
(setq deft-directory "~/Documents")
;; Deft search
(global-set-key [f9] 'deft)

;;Change Yes and No with y and n
(fset 'yes-or-no-p 'y-or-n-p)

;;for imap search
(require 'nnir)

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

(setq load-path (cons "~/.emacs.d/org2blog/" load-path))
(require 'org2blog-autoloads)
(require 'metaweblog)
(require 'xml-rpc)

;;Title of the buffer on top
(setq frame-title-format "%b")

;; Auto completion eshell
(setq eshell-cmpl-cycle-completions nil)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Reporsitories                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;emacs repositories
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
			     "~/Agenda/Meetings.org"))


(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED")))

;;Open ToDo list  at start
(org-todo-list)
(setq inhibit-splash-screen t)

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

;;Diary in the agenda
(setq org-agenda-include-diary t)

;;Flyspell for the inline word correction 
(add-hook 'org-mode-hook 'turn-on-flyspell)

;;wiki
(require 'org-wiki)

;;
;; (setq org-wiki-location "~/org/wiki")

(setq org-wiki-location-list
      '(
        "~/Wiki"    ;; First wiki (root directory) is the default. 
        ))

;; Initialize first org-wiki-directory or default org-wiki 
(setq org-wiki-location (car org-wiki-location-list))

;;Org-cliplink
(global-set-key (kbd "C-x p i") 'org-cliplink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Org-mode ditaa  and babel                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-ditaa-jar-path "/usr/share/ditaa.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (ditaa . t)
   ))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-confirm-babel-evaluate t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Python                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To enable elpy by default just uncomment here
;;(package-initialize)
;;(elpy-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Undo-tree                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Neotree                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/emacstuff/neotree")
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Projectile                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/Projects/")
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode) ;; to enable in all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Latex                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auctex settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Org2blog                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org2blog/wp-blog-alist
      '(("ch3o"
         :url "https://ch3o.wordpress.com/xmlrpc.php"
         :username "ch3o"
	 :password "@wordp@1r1nuMl3@"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil)
        ("my-blog"
         :url "http://username.server.com/xmlrpc.php"
         :username "admin")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: ibuffer                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Global Key Bindings                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default C-x o is bound to 'other window, but I find I use it much more
;; ofther than open-line, which is bound to C-o, so I swap their definitions
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x o") 'open-line)
;; M-0..3 are bound to 'digit-argument. To be used with C-u. I don't use them
;; ofthen, so I prefer to rebind them to the window commands, since M-1 is
;; easyer to type than C-x 1. 
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
;;Org mode keys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-S-a") 'org-archive-subtree)


;;Auto complete and fill
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(setq tab-always-indent 'complete)

;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-wiki-template
   "#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  content
#+DATE: %d

- [[wiki:index][Index]]

- Related: 

* %n
")
 '(package-selected-packages
   (quote
    (hippie-expand-slime hippie-exp-ext org-download interleave org-cliplink smartparens balanced-windows org-notebook org-noter org-pdfview epresent engine-mode tabbar hydra org2blog org-index which-key guide-key org-vcard el-get undo-tree deft org-plus-contrib org-wiki exwm org-journal ox-reveal projectile magit monokai-theme neotree auto-complete solarized-theme popup pdf-tools markdown-toc htmlize ess poly-org poly-markdown org-wild-notifier org-edna org-bullets org-alert elpy autopair auctex)))
 '(send-mail-function (quote mailclient-send-it))
 '(solarized-high-contrast-mode-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
