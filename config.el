;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:

;; ma config :D

;;; Code:

;; =========================

;; | [PACKAGE LOADING] |

;; Setting up package repositories
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("tromey" . "http://tromey.com/elpa/")))
;; Initializing packages
(package-initialize)

;; Installing org mode
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; Ensure use-package is installed and configured
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; =========================

;; | [SETTINGS] |

;; Dracula theme for a dark color scheme
(use-package dracula-theme :config
  (load-theme 'dracula t))

;; Minimal UI for a cleaner look
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Basic editing settings
(setq-default indent-tabs-mode)
(setq-default tab-width 4)
(setq-default fill-column 80)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Disable the Emacs ping sound
(setq ring-bell-function 'ignore)

;; Show matching parentheses
(show-paren-mode t)

;; Performance Improvement
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Disable spc in minibuffer
(define-key minibuffer-local-completion-map "\M- "
            (lambda () (interactive) (insert " ")))

;; Changing where backups are saved
(setq backup-directory-alist '(("." . "/mnt/g/My Drive/backups/emacs-backup")))

;; Custom Region Highlighting
(custom-set-faces '(region ((t (:background "yellow" :foreground "black" :bold t)))))

;; Disable secondary selection
(global-set-key [remap mouse-drag-secondary] 'mouse-drag-region)
(global-set-key [remap mouse-set-secondary] 'mouse-set-region)
(global-set-key [remap mouse-start-secondary] 'mouse-set-point)
(global-set-key [remap mouse-yank-secondary] 'mouse-yank-primary)
(global-set-key [remap mouse-secondary-save-then-kill] 'mouse-save-then-kill)

;; Enable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 10) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Set the default font to "Source Code Pro" with a size of 14
(set-face-attribute 'default nil :family "Source Code Pro" :height 160)

;; All The Icons | Icons for folders
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; All the Icons Dired | Updates our dired to have the icons
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Dired Sidebar | Dired but on the side >:)
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'dracula)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-width 30))

;; Treemacs | Tree manager for emacs
(use-package treemacs
  :ensure t
  :bind
  (("C-c o t" . treemacs))
  :config
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-git-mode 'deferred))

;; Optional: Install and configure Treemacs Icons Dired
(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config
  (treemacs-icons-dired-mode))

;; Small mode-line
(set-face-attribute 'mode-line nil
                    :height 0.9) ; Adjust the height as needed
(set-face-attribute 'mode-line-inactive nil
                    :height 0.9) ; Adjust the height as needed

;; Mood-line | Mininal modeline
(use-package mood-line
  ;; Enable mood-line
  :config
  (mood-line-mode)
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Open links in chrome
(setq browse-url-generic-program "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe")
(setq browse-url-browser-function 'browse-url-generic)

;; Display time
(require 'time)
(setq display-time-format "%d, %b | %H:%M:%S |")
(setq display-time-interval 1) ; Update interval in seconds
(display-time-mode 1)

;; Obsidian Emulation
(use-package visual-fill-column
  :ensure t
  :hook ((org-mode . visual-fill-column-mode)))

;; Setting it so that org-mode centers
(setq visual-fill-column-width 95) ;; The less the thinner
(setq visual-fill-column-center-text t)

;; Full screen
(defun my/fullscreen-on-startup ()
  "Set Emacs to fullscreen on startup."
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Setting our full screen to launch on startup
(add-hook 'emacs-startup-hook 'my/fullscreen-on-startup)

;; =========================

;; | [EDITING] |

;; Company Mode | Auto-completion for code and text
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . company-mode)
         (lisp-mode . company-mode)
         (slime-repl-mode . company-mode))
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (global-company-mode)
  )

;; Flycheck | On-the-fly syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode))

;; Ivy | Autocompletion in buffer and for words
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

;; Counsel | Enhanced commands and completion
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

;; =========================

;; | [CLISP] |

;; SLIME | Superior Lisp Interaction Mode for Emacs
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (add-to-list 'exec-path "/usr/local/bin")
  (slime-setup '(slime-fancy)))

;; Paredit | Structured editing for Lisp code
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (slime-repl-mode . enable-paredit-mode)))

;; Rainbow Delimiters | Adds color to matching parentheses
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode)
         (lisp-interaction-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (slime-repl-mode . rainbow-delimiters-mode))
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666"))

;; =========================

;; [PYTHON]

;; =========================

;; [C]

;; =========================

;; | [ORG-MODE] |

;; -- Setup --

;; Org | A document editing, formatting, and organizing file format
(use-package org
  :ensure t
  :config
  ;; Basic Org mode settings
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-confirm-babel-evaluate nil
        org-directory "/mnt/g/My Drive/documents/notehub/org-roam"
        org-agenda-files '("/mnt/g/My Drive/documents/notehub/org-roam"))

  ;; Org syntax highlighting
  (setq org-src-fontify-natively t)

  ;; Fancy Lambdas
  (global-prettify-symbols-mode t)
  
  ;; Enable Babel languages for code execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (lisp . t)
     (C . t)
     (shell . t)))

  ;; Org modern mode
  (global-org-modern-mode)

  ;; Set fill column to 50 and enable visual line mode
  (defun my-org-mode-setup ()
    "Custom configurations for Org mode."
    (setq fill-column 50)
    (visual-line-mode 1)
    (org-indent-mode))

  ;; Apply custom setup to Org mode
  (add-hook 'org-mode-hook 'my-org-mode-setup))

;; -- Packages --

;; Org-Bullets | Better visibility of org mode headlines
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Automatically refresh dired when files are created or deleted
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; -- Org Modern --

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))

;; =========================

;; | [ORG-ROAM] |

;; Requiring certian packages
(require 'org-roam)
(require 'org-roam-dailies)

;; -- Setup --
(use-package org-roam
  :ensure t
  :custom
  ;; Our org-roam directory
  (org-roam-directory "/mnt/g/My Drive/documents/notehub/org-roam")
  ;; Our org-roam journal directory
  (org-roam-dailies-directory "journal")
  ;; Auto completion
  (org-roam-completion-everywhere t)
  ;; Auto indentation disabled
  (org-adapt-indentation nil)
  (org-roam-capture-templates
   '(
     ("n" "Note" plain
      "%?\n* ${title}\n\n\n\n* Connections:\n\n\n\n* Parent:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: :Note: :${Tag}:")
      :unnarrowed t)

     ("t" "Topic Note" plain
      "%?\n* Children:\n\n\n* Parent:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: :Topic: :${Tag}:")
      :unnarrowed t)

     ("i" "Index Note" plain
      "%?\n* Children:\n\n\n* Parent:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: :Index: :${Tag}:")
      :unnarrowed t)
     
     ("r" "Reference Note" plain
      "%?\n* Synopsis:\n\n\n* Source:\n%^{Link/Path}"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+author: %^{Author}\n#+year: %^{Year}\n#+filetags: :Reference: :${Tag}:")
      :unnarrowed t)

     ))
  ;; Keybinds for org-roam nodes
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n p" . org-id-get-create)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-db-autosync-enable)
  )

;; Org Roam | A tool for organizing and navigating your notes
(use-package org-roam
  :ensure t
  :config
  ;; Add any additional org-roam specific configurations here
  )

;; Websocket | WebSocket client for Emacs
(use-package websocket
  :ensure t)

;; Simple HTTPD | Simple HTTP server for Emacs
(use-package simple-httpd
  :ensure t
  :config
  ;; Set the HTTP server port to 1725
  (setq httpd-port 1725))

;; F | Modern API for working with files and directories
(use-package f
  :ensure t)

;; Org Roam UI | Provides a graphical interface for Org Roam
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :bind ("C-c n g" . org-roam-ui-open)
  :config
  ;; Org Roam UI settings
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; =========================

;; | [ORG-ROAM IMPROVEMENTS] |

(defun org-roam-node-insert-immediate (arg &rest args)
  "Finishes the creation of a org-roam-capture-template immediately"
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))


(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-tag (tag)
  "Find or create an Org-roam node with a specific TAG.
This function prompts for a tag and then either finds an existing node
with that tag or creates a new one."
  (interactive "sEnter tag: ")
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; Select a node to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag tag)))

(defun my/org-roam-capture-inbox ()
  "Inbox to capture quick ideas."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:40}" 'face 'org-tag)))

(setq org-startup-indented t)  ;; Start with indentation for better visual hierarchy
(setq org-hide-leading-stars t) ;; Hide leading stars to clean up the view

;; =========================

;; | [ORG-AGENDA] |

;; Requiring the org-agenda package
(require 'org-agenda)

;; Remove tags in org-agenda
(setq org-agenda-hide-tags-regexp ".")

;; Remove details in org task view
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "CANCELLED(c)" "DONE(d)")))

;; Capture a quick idea
(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Move to ending of file later
(define-key global-map (kbd "C-c n x") 'org-capture-inbox)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))

;; Am-Pm in Org Agena
(setq org-agenda-timegrid-use-ampm t)

;; ;; Set the default agenda span to todays view
;; (setq org-agenda-span 1)

;; ;; Set the default time range for agenda views
;; (setq org-agenda-start-day "today")

;; =========================
;; | [BROWSER] |

;; I don't think using a browser in emacs is the most fun experience?..

;; ;; Ensure Emacs uses Python 3 for EAF
;; (setenv "PYTHONPATH" "/usr/bin/python3")

;; ;; Add EAF directory to load-path
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

;; ;; Use-package configuration for EAF
;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
;;   :custom
;;   (eaf-browser-continue-where-left-off t)  ;; Continue where you left off
;;   (eaf-browser-enable-adblocker t)         ;; Enable Adblocker
;;   :bind
;;   ("C-c w" . eaf-open-browser)
;;   :config
;;   (require 'eaf)          ;; Ensure EAF core is loaded
;;   (require 'eaf-browser)  ;; Load the browser module
;;   ;; Define eaf keybindings
;;   (eaf-bind-key scroll_up "C-n" eaf-browser-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-browser-keybinding)
;;   ;; More keybindings can be added similarly
;; )

;; =========================

;; | [KEY-BINDINGS] |
;; Description: For settings related to key-binding

(defun my-org-export-to-pdf ()
  "Export the current Org buffer to PDF and save it in the specified folder without a table of contents."
  (interactive)
  (let* ((file-dir "/mnt/g/My Drive/documents/filehub/pdfs/")
         (file-name (concat (file-name-as-directory file-dir)
                            (file-name-base (buffer-file-name)) ".pdf"))
         (org-export-with-toc nil)) ;; Disable table of contents
    (org-latex-export-to-pdf)
    (rename-file (concat (file-name-base (buffer-file-name)) ".pdf") file-name t)))

(define-key org-mode-map (kbd "C-c o e") 'my-org-export-to-pdf)

(require 'dired-sidebar)

(defun open-key-sheet ()
  "Open the key_sheet.org file."
  (interactive)
  (find-file "~/.emacs.d/key_sheet.org"))

(global-set-key (kbd "C-c o k") 'open-key-sheet)

(defun open-devhub ()
  "Open the devhub directory in dired-sidebar."
  (interactive)
  (dired-sidebar-toggle-sidebar) ;; Ensure the sidebar is open
  (dired-sidebar-find-file "/mnt/g/My Drive/documents/devhub"))

(global-set-key (kbd "C-c o d") 'open-devhub)

(defun open-notehub ()
  "Open the notehub directory in dired-sidebar."
  (interactive)
  (dired-sidebar-toggle-sidebar) ;; Ensure the sidebar is open
  (dired-sidebar-find-file "/mnt/g/My Drive/documents/notehub"))

(global-set-key (kbd "C-c o n") 'open-notehub)

(defun open-config-el ()
  "Open the ~/.emacs.d/config.el file."
  (interactive)
  (find-file "~/.emacs.d/config.el"))

(global-set-key (kbd "C-c o c") 'open-config-el)

;; Opens our agenda
(global-set-key (kbd "C-c o a") 'org-agenda)

;; Insert a note immeditately key-binding
(global-set-key (kbd "C-c n I") 'org-roam-node-insert-immediate)

;; Creates a quick note to be stored in our Inbox.org
(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

;; Bind to a key
(global-set-key (kbd "C-c n t") #'my/org-roam-find-tag)

;; Keybinding to open enlight menu
(global-set-key (kbd "C-c o q") #'enlight-open)

;; =========================

;; | [START-UP ] |

;; https://github.com/ichernyshovvv/enlight?tab=readme-ov-file#installation

;; Enlight UI
(defvar enlight-guix
  ;; ASCII Art
  (propertize
   "⠀⠀⠀⠀⠀⠀⠀⠀⣀⣤⣴⣶⣾⡿⠿⠿⢿⣿⣶⣶⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⣀⣴⣿⠿⠛⠉⠁⠀⠀⠀⠀⠀⠀⠀⠉⠛⠿⣿⣶⣄⠀⠀⠀⠀⠀
⠀⠀⠀⢠⣾⡿⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣷⣄⠀⠀⠀
⠀⠀⣴⣿⠟⠀⠀⠀⠀⠀⠀⣀⡀⠀⠀⠀⠀⢀⣀⠀⠀⠀⠀⠀⠀⠙⣿⣧⡀⠀
⠀⣼⣿⠃⠀⠀⠀⠀⠀⠀⣾⣯⣈⣧⠀⠀⢰⣿⣅⣹⡀⠀⠀⠀⠀⠀⠈⢿⣷⠀
⢰⣿⠇⠀⠀⠀⠀⠀⠀⠀⠹⢿⣿⠏⠀⠀⠈⠿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠘⣿⣇
⣾⣿⠀⠀⢰⣾⡿⣷⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣰⠿⣿⣶⠀⠀⢻⣿
⣿⣿⠀⠀⠀⢸⣷⣄⠉⠙⠛⠷⠶⠶⠶⠶⠶⠶⠶⠛⠛⠉⢀⣴⣿⠀⠀⠀⢸⣿
⢻⣿⠀⠀⠀⠘⣿⣿⣷⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣿⣿⡏⠀⠀⠀⣼⣿
⠸⣿⡇⠀⠀⠀⢻⣿⣿⣿⣿⣿⣷⣶⣶⣶⣶⣾⣿⣿⣿⣿⣿⣿⠁⠀⠀⢠⣿⡇
⠀⢻⣿⣄⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇⠀⠀⢀⣾⡟⠀
⠀⠀⠻⣿⣦⠀⠀⠈⢻⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡿⠃⠀⠀⣠⣿⡟⠀⠀
⠀⠀⠀⠘⢿⣷⣤⡀⠀⠙⠷⣦⣀⡀⠀⠀⠀⣀⣤⡾⠋⠀⠀⣠⣾⡿⠋⠀⠀⠀
⠀⠀⠀⠀⠀⠉⠻⣿⣶⣤⣀⡀⠉⠙⠛⠛⠛⠉⢁⣀⣤⣶⣿⠟⠋⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠻⠿⢿⣿⣷⣾⣿⡿⠿⠟⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀"
   'face 'enlight-yellow-bold))

;; Enlight | Custom startup screen
(use-package enlight
  :ensure t
  :custom
  ;; Enlight quick access
  (enlight-content
   (concat
    enlight-guix
    "\n\n\n"
    (enlight-menu
     '(("Org Mode"
        ("Org-Agenda" (org-agenda nil "a") "C-c o a")
        ("Agenda" (lambda () (interactive) (find-file "/mnt/g/My Drive/documents/notehub/org-roam/agenda.org")) "C-c o g"))
       ("\nFolders"
        ("Devhub" (dired "/mnt/g/My Drive/documents/devhub") "C-c o d")
        ("Notehub" (dired "/mnt/g/My Drive/documents/notehub") "C-c o n")))))))

;; Disable scratch
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; Set our inital buffer to be enlight
(setopt initial-buffer-choice #'enlight)

;; =========================

;;; config.el ends here
