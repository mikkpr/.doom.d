;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(let ((secret.el (expand-file-name "secret.el" "~/.doom.d")))
  (when (file-exists-p secret.el)
    (load secret.el)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

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
(setq doom-font (font-spec :family "Hasklig" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
;;

(add-to-list 'load-path "/home/mikk/.emacs.d/spotify")
(require 'spotify)

;; Spotify settings
(setq spotify-transport 'connect)
(define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

(setq org-mobile-directory "/davs:mikkpristavka@gmail.com@webdav.opendrive.com:/Org"
      org-mobile-use-encryption nil
      org-mobile-inbox-for-pull "~/org/from-mobile.org"
      org-mobile-files '("~/org/home.org" "~/org/work.org")
      )

(setq
  js-indent-level 2
  projectile-project-search-path '("~/dev/"))

(defun mikkpr/open-home-org ()
  (interactive)
  (find-file "~/org/home.org"))
(defun mikkpr/open-work-org ()
  (interactive)
  (find-file "~/org/work.org"))

;; with `evil-define-key'
(evil-define-key nil evil-normal-state-map
  "J" (lambda() (interactive) (evil-next-visual-line 5))
  "K" (lambda() (interactive) (evil-previous-visual-line 5))
  (kbd "SPC o h") 'mikkpr/open-home-org
  (kbd "SPC o w") 'mikkpr/open-work-org
  (kbd "C-j") 'next-error
  (kbd "C-k") 'previous-error
  (kbd ", e") 'centaur-tabs-forward
  (kbd ", q") 'centaur-tabs-backward
  (kbd ", x") 'kill-this-buffer
  (kbd ", 1") 'centaur-tabs-select-visible-tab
  (kbd ", 2") 'centaur-tabs-select-visible-tab
  (kbd ", 3") 'centaur-tabs-select-visible-tab
  (kbd ", 4") 'centaur-tabs-select-visible-tab
  (kbd ", 5") 'centaur-tabs-select-visible-tab
  (kbd ", 6") 'centaur-tabs-select-visible-tab
  (kbd ", 7") 'centaur-tabs-select-visible-tab
  (kbd ", 8") 'centaur-tabs-select-visible-tab
  (kbd ", 9") 'centaur-tabs-select-visible-tab
  (kbd ", c") 'evil-avy-goto-char
  (kbd ", l") 'evil-avy-goto-line
  (kbd ", w") 'evil-avy-goto-word-0)

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(setq org-log-state-notes-into-drawer t)

(require 'org-super-agenda)
(def-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  :config
  (org-super-agenda-mode)
  )

(require 'org-gcal)
(setq org-gcal-file-alist '(("oinasz@gmail.com" .  "~/org/schedule.org")))

(setq org-agenda-directory "~/org/")
(setq org-agenda-files '("~/org/" "~/org-jira/"))

(setq org-capture-templates
'(("p" "Home" entry
  (file+headline "~/org/home.org" "Inbox")
  "* %?\n%i\n%a" :prepend t)
 ("w" "Work" entry
  (file+headline "~/org/work.org" "Inbox")
  "* %?\n%i\n%a" :prepend t)))

 ;("j" "Journal" entry
 ; (file+datetree "~/org/journal.org")
 ;"**** %U%?%a \n" :tree-type week))

(require 'flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun mikkpr/JSXHook ()
  "My Hook for JSX Files"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (flycheck-select-checker 'javascript-eslint)
  (flycheck-mode))

(add-to-list 'magic-mode-alist '("import " . mikkpr/JSXHook))

(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun mikkpr/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'mikkpr/use-eslint-from-node-modules)

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(require 'org-agenda-property)
(setq org-agenda-property-list '("status"))
(setq org-agenda-property-position 'where-it-fits)
