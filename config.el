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
(use-package dracula-theme  :config
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

;; Full screen
(defun my/fullscreen-on-startup ()
  "Set Emacs to fullscreen on startup."
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Setting our full screen to launch on startup
(add-hook 'emacs-startup-hook 'my/fullscreen-on-startup)

;; Set the default font to "Source Code Pro" with a size of 14
(set-face-attribute 'default nil :family "Source Code Pro" :height 190)

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
  (setq dired-sidebar-width 20))

;; Set custom height for the mode line to make it thinner
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
        org-confirm-babel-evaluate nil)

  ;; ;; Ellipsis icon
  ;; (setq org-ellipsis "⤵")

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

;; Custom TODO Keywords and Faces
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "CANCELLED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "orange" :weight bold))
        ("IN-PROGRESS" . (:foreground "white" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))
        ("DONE" . (:foreground "green" :weight bold :strike-through t))))

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
     ("f" "Fleeting Note" plain
     "%?\n* Note:\n\n\n* Next Action:\n\n"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+source: %^{Source}\n#+filetags: fleeting")
     :unnarrowed t)

    ("l" "Literature Note" plain
     "%?\n* Atomic Idea:\n\n\n* Summary:\n\n\n* Quotes (if applicable):\n\n\n* Reflection:\n\n\n* References:\n\n"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: literature")
     :unnarrowed t)

    ("c" "Class Note" plain
     "%?\n* Core Ideas:\n\n\n* Key Takeaways:\n\n\n* Questions:\n\n\n* References:\n\n"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+course_code: %^{Course Code}\n#+filetags: class")
     :unnarrowed t)

    ("p" "Permanent Note" plain
     "%?\n* Atomic Concept:\n\n\n* Explanation:\n\n\n* Connections:\n\n\n* References:\n\n"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: permanent")
     :unnarrowed t)

    ("i" "Index Note" plain
     "%?\n* References:\n\n"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+filetags: index")
     :unnarrowed t)

    ("r" "Reference Note" plain
     "%?\n* Synopsis:\n\n\n* Source:\n%^{Link/Path}"
     :if-new (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %H:%M>\n#+author: %^{Author}\n#+year: %^{Year}\n#+filetags: reference")
     :unnarrowed t)
    
         ))
  ;; Keybinds for org-roam nodes
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n p" . org-id-get-create)
         :map org-mode-map
         ("C-M-i"   . completion-at-point)
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

(require 'org-refile)
(defun my/org-roam-copy-todo-to-today ()
  "Copy the TODO item to today's Org-roam daily file when the TODO state change."
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        today-file
        pos)
    (save-window-excursion
      ;; Capture today's daily file
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; Hook to copy TODO items to today's daily file when their state changes to DONE
(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:20}" 'face 'org-tag)))

;; =========================

;; | [ORG-AGENDA] |

(use-package org
  :ensure t
  :config
  (require 'org-agenda))  ;; Explicitly load org-agenda

;; Requiring the org-agenda package
(require 'org-agenda)

;; Org-Agenda connecting to Org-Roam Files
(defun my/org-roam-filter-by-tag (tag-name)
  "Return a predicate function that check if a node has the specified TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Return a list of file paths for Org-roam nodes that have the specified TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-list-all-notes ()
  "Return a list of file paths for all Org-roam nodes."
  (mapcar #'org-roam-node-file
          (org-roam-node-list)))

(defun my/org-roam-refresh-agenda-list ()
  "Update `org-agenda-files` to include files based on specific criteria.
By default, includes files with the 'note' tag. Can be modified to include all files."
  (interactive)
  ;; Change this line to include all files if desired:
  ;; (setq org-agenda-files (my/org-roam-list-notes-by-tag "note"))
  (setq org-agenda-files (my/org-roam-list-all-notes)))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

;; Refresh Org-Agenda Bug
(defun my/refresh-org-agenda-files ()
  "Refresh `org-agenda-files` to include all Org-roam nodes."
  (when (derived-mode-p 'org-mode)
    (my/org-roam-refresh-agenda-list)))

(add-hook 'after-save-hook #'my/refresh-org-agenda-files)

;; Customize the appearance of the agenda
(setq org-agenda-custom-commands
      '(("c" "Custom Agenda View"
         ((agenda "" ((org-agenda-span 'day)))
          (tags "TODO=\"TODO\""
                ((org-agenda-overriding-header "Tasks to do")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          (tags "TODO=\"IN-PROGRESS\""
                ((org-agenda-overriding-header "In-Progress Tasks")))))))

;; Set the default agenda span to todays view
(setq org-agenda-span 1)

;; Set the default time range for agenda views
(setq org-agenda-start-day "today")

;; Automatically add newly created nodes to the agenda
(defun my/org-roam-capture-add-to-agenda ()
  "Add newly created Org-roam nodes to the agenda."
  (when (org-roam-node-file (org-roam-node-at-point))
    (add-to-list 'org-agenda-files (org-roam-node-file (org-roam-node-at-point)))))

(add-hook 'org-roam-capture-after-finalize-hook #'my/org-roam-capture-add-to-agenda)

;; =========================

;; | [KEY-BINDINGS] |
;; Description: For settings related to key-binding

(defun my-org-export-to-pdf ()
  "Export the current Org buffer to PDF and save it in the specified folder without a table of contents."
  (interactive)
  (let* ((file-dir "/mnt/g/My Drive/documents/filehub/pdfs/")
         (file-name (concat (file-name-as-directory file-dir)
                            (file-name-base (buffer-file-name)) ".pdf"))
         (org-export-with-toc nil))  ;; Disable table of contents
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

;; Goes to top of the buffer
(global-set-key (kbd "C-c g t") 'beginning-of-buffer)

;; Goes to end of the buffer
(global-set-key (kbd "C-c g b") 'end-of-buffer)

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
  (propertize
   " ..                             `.
 `--..```..`           `..```..--`   
   .-:///-:::.       `-:::///:-.     
      ````.:::`     `:::.````        
           -//:`    -::-             
            ://:   -::-              
            `///- .:::`              
             -+++-:::.               
              :+/:::-                
              `-....`                "
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
