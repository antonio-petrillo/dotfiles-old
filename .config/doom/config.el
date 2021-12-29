;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Antonio Petrillo"
      user-mail-address "antonio.petrillo4@studenti.unina.it")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'spacemacs-dark)
(setq doom-theme 'gruber-darker)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package! dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config (evil-collection-define-key 'normal 'dired-mode-map
            "H" 'dired-hide-dotfiles-mode))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-chmod
  (kbd "O") 'dired-chown
  (kbd "R") 'dired-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; chage font
(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono Nerd Font" :size 15))

;; org roam
(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Org/Roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; use trash can in dired
(setq delete-by-moving-to-trash t)

;; keybindings to open vterm popup (SPC o t is a little bit annoying because I need to be in normal mode)
(map!
 :desc "Open vterm popup buffer"
 "<C-f12>" #'+vterm/toggle)

;; keybindings to eval s-exp
(map! :leader
      (:prefix-map ("e" . "eval s-exp")
       :desc "eval s-expression" "e" #'eval-last-sexp
       :desc "eval region" "r" #'eval-region
       :desc "eval buffer" "b" #'eval-buffer
       :desc "just for hydra test" "t" #'nto-test/body))

;; window management with hydra
;; inspired/borrowed from [[https://github.com/jakebox/jake-emacs][JakeB]]  --> for the future literate config

(general-define-key "s-o" 'nto/hydra-window-management/body)

(defhydra nto/hydra-window-management (:pre (message "window management")
                                       :color pink
                                       :hint nil
                                       :post (message "bye bye"))

  "basic window management with hydra (what an awesome package!)"

  ;; quit wm mode
  ("q" nil "exit" :color blue)

  ;; buffer & file
  ("b" consult-buffer "switch buffer")
  ("[" previous-buffer "previous buffer")
  ("]" next-buffer "next buffer")
  ("K" kill-this-buffer "kill current buffer")
  ("C-k" kill-buffer "kill current buffer")
  ("f" find-file "find file")

  ;; window size
  ("=" evil-window-increase-height "increase height") ;; = so I don't need to use shift
  ("-" evil-window-decrease-height "decrease height")
  ("." evil-window-increase-width "increase width")
  ("," evil-window-decrease-width "increase width")
  ("e" balance-window "balance window")

  ;; split & swap
  ("c" evil-window-delete "close window")
  ("d" evil-window-delete "close window")
  ("S" ace-swap-window "swap current window")
  ("v" evil-window-vsplit "vertical split")
  ("s" evil-window-split "horizontal split")

  ;; split, then open app
  ("t" nto/open-terminal-vertical "open terminal vertically")
  ("T" nto/open-terminal-horizontal "open terminal horizontally")

  ;; movement
  ("h" evil-window-left "go left")
  ("j" evil-window-down "go down")
  ("k" evil-window-up "go up")
  ("l" evil-window-right "go right"))

;; for me the split-window-vertically & split-window-horizontally don't make sense, so I have inverted them
(defun nto/open-terminal-horizontal ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (eshell))

(defun nto/open-terminal-vertical ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (eshell))

;; make org pretty
(setq org-ellipsis " ▾")
(setq org-startup-folded t)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)

(use-package! org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; transparent background;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
