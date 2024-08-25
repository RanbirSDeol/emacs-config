;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:

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
(setq backup-directory-alist '(("." . "/mnt/c/Users/ranbi/Dropbox/backups/emacs-backup")))

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
  (("C-c o t" . my-treemacs-open-current-directory))
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

(defun my-dired-home ()
  "Open Dired in the home directory."
  (interactive)
  (dired "/mnt/c/Users/ranbi/Dropbox/devhub"))  ;; Replace "~/" with any directory path you want

;; Optionally bind it to a key
(global-set-key (kbd "C-x d") 'my-dired-home)

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
  (global-company-mode))

;; Flycheck | On-the-fly syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Flyspell with Hunspell dictionary
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")  ;; Set your desired dictionary
(setq ispell-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;; Enable Flyspell in text modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Disable Flyspell in certain modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

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

;; LanguageTool | Grammar checking
(use-package langtool
  :ensure t
  :init
  (setq langtool-language-tool-jar "~/opt/LanguageTool-6.4/languagetool-commandline.jar") ;; Update with the path to your language-tool.jar
  (setq langtool-mother-tongue "en"))

;; Keybindings for LanguageTool
;; Starting and stopping a langcheck
(global-set-key (kbd "C-c l s") 'langtool-check)
(global-set-key (kbd "C-c l d") 'langtool-check-done)
;; Fix at point
(global-set-key (kbd "C-c l c") 'langtool-correct-at-point)
;; Correct the buffer
(global-set-key (kbd "C-c l b") 'langtool-correct-buffer)
;; Check what the language error is
(global-set-key (kbd "C-c l e") 'langtool-show-brief-message-at-point)

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

;; [JavaScript]



;; =========================

;; | [ORG-MODE] |

;; Org | A document editing, formatting, and organizing file format
(use-package org
  :ensure t
  :config
  ;; Basic Org mode settings
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-confirm-babel-evaluate nil
        org-directory "/mnt/c/Users/ranbi/Dropbox/notehub"
        org-agenda-files '("/mnt/c/Users/ranbi/Dropbox/notehub"))

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

;; -- Org-Timer --

;; Org Timer Ended Message
(setq org-show-notification-handler
      (lambda (message)
        ;; Display the bold notification in the minibuffer
        (message (propertize "Pomodoro timer has ended!" 'face 'bold))
        ;; Play the sound using Windows Media Player
        (start-process "play-sound" nil "powershell.exe"
                       "-c" "Start-Process 'wmplayer.exe' -ArgumentList 'C:\\Users\\ranbi\\Downloads\\Sounds\\ding.wav'")))

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
  (org-roam-directory "/mnt/c/Users/ranbi/Dropbox/notehub")
  ;; Our org-roam journal directory | JOURNAL DISABLED
  ;;(org-roam-dailies-directory "journal")
  ;; Auto completion
  (org-roam-completion-everywhere t)
  ;; Auto indentation disabled
  (org-adapt-indentation nil)
  (org-roam-capture-templates
   '(
     ("p" "Permanent Note" plain
      "%?\n* ${title}\n\n\n* Connections:\n\n\n* Parent:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: :PNote: :${Tag}:")
      :unnarrowed t)

     ("f" "Fleeting Note" plain
      "%?\n\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: :FNote: :${Tag}:")
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
  :ensure t)

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

;; Open file links in the current buffer
(setq org-link-frame-setup '((file . find-file)))
(setq org-file-apps '(("\\.org\\'" . emacs)
                      (auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . default)))

;; Ensure file links reuse the current window
(setq org-link-frame-setup '((file . find-file)))
(setq org-open-link-to-file-in-emacs 'find-file)

;; Optional: prevent Emacs from splitting windows when opening files
(setq split-width-threshold nil)
(setq split-height-threshold nil)

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

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:40}" 'face 'org-tag)))

(setq org-startup-indented t)  ;; Start with indentation for better visual hierarchy
(setq org-hide-leading-stars t) ;; Hide leading stars to clean up the view

;; =========================

;; | [ORG-AGENDA] |

;; Requiring the org-agenda package
(require 'org-agenda)

;; Remove details in org task view
(setq org-agenda-prefix-format
      '((agenda . "  %i %?-12t% s")
        (timeline . "  % s")
        (todo . "  %i ")
        (tags . "  %i %s ")
        (search . "  %i ")))

;; Days Config
(setq org-agenda-deadline-leaders '("Deadline: " "%d Days: " "%d Days Late: "))

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h)" "|" "CANCELLED(c)" "DONE(d)")))

(defface org-completed-task-face
  '((t (:foreground "gray")))
  "Face for completed tasks on deadlines."
  :group 'org-faces)

;; Automatically add a CLOSED timestamp when a task is marked as DONE
(setq org-log-done 'time)

(defun my/org-roam-capture-inbox ()
  "Inbox to capture quick ideas."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "20240822090227-inbox.org" "Inbox")))))

(defun org-capture-inbox ()
  "Quick capture into the Inbox."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Move to ending of file later
(define-key global-map (kbd "C-c n x") 'org-capture-inbox)

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
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-Priority")))
          (agenda ""
                  ((org-agenda-overriding-header "Week-Agenda")))
             (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "Tasks Completed Today:")))
          (alltodo ""
                   ((org-agenda-overriding-header "Global list of all TODO items")
                    (org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))

;; Disable future tasks
(setq org-agenda-todo-ignore-scheduled 'future)

;; Am-Pm in Org Agena
(setq org-agenda-timegrid-use-ampm t)

;; Archive location
(setq org-archive-location "/mnt/c/Users/ranbi/Dropbox/notehub/20240822200723-archive.org::")

;; Auto archive
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "DONE")
              (org-archive-subtree))))

;; Set the default agenda span to todays view
(setq org-agenda-span 1)

;; =========================

;; | [KEY-BINDINGS] |

(defun my-org-export-to-pdf ()
  "Export the current Org buffer to PDF and save it in the specified folder without a table of contents."
  (interactive)
  (let* ((file-dir "/mnt/c/Users/ranbi/Dropbox/documents/filehub/pdfs/")
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
  (dired-sidebar-find-file "/mnt/c/Users/ranbi/Dropbox/devhub"))

(global-set-key (kbd "C-c o d") 'open-devhub)

(defun open-notehub ()
  "Open the notehub directory in dired-sidebar."
  (interactive)
  (dired-sidebar-toggle-sidebar) ;; Ensure the sidebar is open
  (dired-sidebar-find-file "/mnt/c/Users/ranbi/Dropbox/notehub"))

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

;; Starting and stopping a langcheck
(global-set-key (kbd "C-c l s") 'langtool-check)
(global-set-key (kbd "C-c l d") 'langtool-check-done)

;; Fix at point
(global-set-key (kbd "C-c l c") 'langtool-correct-at-point)

;; Correct the buffer
(global-set-key (kbd "C-c l b") 'langtool-correct-buffer)

;; Check what the language error is
(global-set-key (kbd "C-c l e") 'langtool-show-brief-message-at-point)

;; =========================

;; ;; | [START-UP ] |

;; ;; https://github.com/ichernyshovvv/enlight?tab=readme-ov-file#installation

;; ;; Enlight UI
;; (defvar enlight-guix
;;   ;; ASCII Art
;;   (propertize
;;    "⠀⠀⠀⠀⠀⠀⠀⠀⣀⣤⣴⣶⣾⡿⠿⠿⢿⣿⣶⣶⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀
;; ⠀⠀⠀⠀⠀⣀⣴⣿⠿⠛⠉⠁⠀⠀⠀⠀⠀⠀⠀⠉⠛⠿⣿⣶⣄⠀⠀⠀⠀⠀
;; ⠀⠀⠀⢠⣾⡿⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣷⣄⠀⠀⠀
;; ⠀⠀⣴⣿⠟⠀⠀⠀⠀⠀⠀⣀⡀⠀⠀⠀⠀⢀⣀⠀⠀⠀⠀⠀⠀⠙⣿⣧⡀⠀
;; ⠀⣼⣿⠃⠀⠀⠀⠀⠀⠀⣾⣯⣈⣧⠀⠀⢰⣿⣅⣹⡀⠀⠀⠀⠀⠀⠈⢿⣷⠀
;; ⢰⣿⠇⠀⠀⠀⠀⠀⠀⠀⠹⢿⣿⠏⠀⠀⠈⠿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠘⣿⣇
;; ⣾⣿⠀⠀⢰⣾⡿⣷⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣰⠿⣿⣶⠀⠀⢻⣿
;; ⣿⣿⠀⠀⠀⢸⣷⣄⠉⠙⠛⠷⠶⠶⠶⠶⠶⠶⠶⠛⠛⠉⢀⣴⣿⠀⠀⠀⢸⣿
;; ⢻⣿⠀⠀⠀⠘⣿⣿⣷⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣿⣿⡏⠀⠀⠀⣼⣿
;; ⠸⣿⡇⠀⠀⠀⢻⣿⣿⣿⣿⣿⣷⣶⣶⣶⣶⣾⣿⣿⣿⣿⣿⣿⠁⠀⠀⢠⣿⡇
;; ⠀⢻⣿⣄⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇⠀⠀⢀⣾⡟⠀
;; ⠀⠀⠻⣿⣦⠀⠀⠈⢻⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡿⠃⠀⠀⣠⣿⡟⠀⠀
;; ⠀⠀⠀⠘⢿⣷⣤⡀⠀⠙⠷⣦⣀⡀⠀⠀⠀⣀⣤⡾⠋⠀⠀⣠⣾⡿⠋⠀⠀⠀
;; ⠀⠀⠀⠀⠀⠉⠻⣿⣶⣤⣀⡀⠉⠙⠛⠛⠛⠉⢁⣀⣤⣶⣿⠟⠋⠀⠀⠀⠀⠀
;; ⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠻⠿⢿⣿⣷⣾⣿⡿⠿⠟⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀"
;;    'face 'enlight-yellow-bold))

;; ;; Enlight | Custom startup screen
;; (use-package enlight
;;   :ensure t
;;   :custom
;;   ;; Enlight quick access
;;   (enlight-content
;;    (concat
;;     enlight-guix
;;     "\n\n\n"
;;     (enlight-menu
;;      '(("Org Mode"
;;         ("Org-Agenda" (org-agenda nil "a") "C-c o a")
;;         ("Agenda" (lambda () (interactive) (find-file "/mnt/g/My Drive/documents/notehub/agenda.org")) "C-c o g"))
;;        ("\nFolders"
;;         ("Devhub" (dired "/mnt/g/My Drive/documents/devhub") "C-c o d")
;;         ("Notehub" (dired "/mnt/g/My Drive/documents/notehub") "C-c o n")))))))

;; ;; Disable scratch
;; (setq initial-scratch-message nil)
;; (setq inhibit-startup-screen t)

;; ;; Set our inital buffer to be enlight
;; (setopt initial-buffer-choice #'enlight)

;; =========================

;;; config.el ends here
