;;; init.el --- Global settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Name: Ranbir Singh Deol

;;; Code:

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; Add MELPA repository for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load custom configuration
(load-file (expand-file-name "config.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   '(eyebrowse mood-line moody all-the-icons-dired all-the-icons dired-sidebar org-roam-ui org-roam company dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "yellow" :foreground "black" :bold t)))))

;;; init.el ends here
