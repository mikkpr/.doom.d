(let ((secret.el (expand-file-name "secret.el" "~/.doom.d")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(setq doom-font (font-spec :family "Hasklig" :size 12))

(setq doom-theme 'doom-tomorrow-night)

(setq display-line-numbers-type t)

(setq
  js-indent-level 2)

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

(add-to-list 'load-path "/home/mikk/.emacs.d/spotify")
(require 'oauth2 "~/.emacs.d/spotify/oauth2.el")
(require 'spotify-api "~/.emacs.d/spotify/spotify-api.el")
(require 'spotify-track-search "~/.emacs.d/spotify/spotify-track-search.el")
(require 'spotify-playlist-search "~/.emacs.d/spotify/spotify-playlist-search.el")
(require 'spotify-device-select "~/.emacs.d/spotify/spotify-device-select.el")
(require 'spotify-remote "~/.emacs.d/spotify/spotify-remote.el")
(require 'spotify-dbus "~/.emacs.d/spotify/spotify-dbus.el")
(require 'spotify-connect "~/.emacs.d/spotify/spotify-connect.el")
(require 'spotify-controller "~/.emacs.d/spotify/spotify-controller.el")
(require 'spotify "~/.emacs.d/spotify/spotify.el")
(require 'decide "~/.emacs.d/decide-mode/decide.el")
(require 'org-expiry)

(setq org-log-state-notes-into-drawer t)

;;setq org-directory "~/Dropbox/org/")
(setq org-agenda-files '(
                         "~/Dropbox/org/home.org"
                         "~/Dropbox/org/work.org"
                         ))

(defun mikkpr/open-home-org ()
  (interactive)
  (find-file "~/Dropbox/org/home.org"))
(defun mikkpr/open-work-org ()
  (interactive)
  (find-file "~/Dropbox/org/work.org"))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(setq org-capture-templates
'(("p" "Home" entry
  (file+headline "~/Dropbox/org/home.org" "Inbox")
  "* %?\n%i\nCREATED: %u" :prepend t)
 ("l" "Work log entry" entry (file+olp+datetree+prompt "~/Dropbox/org/work.org" "Log")
  "* %?\nCREATED: %u" :prepend t :jump-to-captured t)
 ("j" "Journal entry" entry (file+olp+datetree+prompt "~/Dropbox/org/home.org" "Journal")
  "* %?\nCREATED: %u" :prepend t :jump-to-captured t)
 ("w" "Work" entry
  (file+headline "~/Dropbox/org/work.org" "Inbox")
  "* %?\n%i\nCREATED: %u" :prepend t)))

(require 'org-gcal)
(setq org-gcal-file-alist '(("oinasz@gmail.com" .  "~/Dropbox/org/schedule.org")))

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

(add-hook 'org-insert-heading-hook
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (org-expiry-insert-created))))

(require 'org-datetree)
(require 'org-reverse-datetree)

(defun mikkpr/org-refile-to-work-log (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file "~/Dropbox/org/work.org" "Log"
                                       :ask-always arg :prefer '("SCHEDULED" "CREATED_TIME" "CREATED_AT" "CLOSED")))

(defun mikkpr/org-refile-to-journal (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file "~/Dropbox/org/home.org" "Journal"
                                       :ask-always arg :prefer '("SCHEDULED" "CREATED_TIME" "CREATED_AT" "CLOSED")))

(set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.25)

(setq org-agenda-custom-commands
      '(("." "Overview (Custom)"
         ((agenda ""
                  ((org-agenda-span 5)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-show-future-repeats 'next)
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-overriding-header "* Calendar\n")))
          (todo ""
                ((org-agenda-overriding-header "\n* Open\n")
                 (org-agenda-block-separator nil)
                 (org-agenda-sorting-strategy '(todo-state-up))
                 (org-agenda-todo-ignore-scheduled 'all)))
          ))
      ("h" "Browse entries in home.org"
         org-ql-block '(level 4)
         ((org-super-agenda-groups
           '((:todo "DONE")
             (:todo t)))
          (org-agenda-files '("~/Dropbox/org/home.org"))))
      ("w" "Browse entries in work.org"
         org-ql-block '(level 4)
         ((org-super-agenda-groups
           '((:todo "DONE")
             (:todo t)))
          (org-agenda-files '("~/Dropbox/org/work.org"))))))

(require 'org-agenda-property)
(setq org-agenda-property-list '("status"))
(setq org-agenda-property-position 'where-it-fits)

;; Spotify settings
(setq spotify-transport 'connect)
(define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

(define-prefix-command 'decide-prefix-map)
(define-key decide-mode-map (kbd "C-c ?") 'decide-prefix-map)
(define-key decide-mode-map (kbd "C-c ? ?") 'decide-dwim-insert)
(define-key decide-mode-map (kbd "C-c ? +") 'decide-for-me-likely)
(define-key decide-mode-map (kbd "C-c ? -") 'decide-for-me-unlikely)
(define-key decide-mode-map (kbd "C-c ? d") 'decide-roll-dice)
(define-key decide-mode-map (kbd "C-c ? D") 'decide-roll-2d6)
(define-key decide-mode-map (kbd "C-c ? 3") 'decide-roll-1d3)
(define-key decide-mode-map (kbd "C-c ? 4") 'decide-roll-1d4)
(define-key decide-mode-map (kbd "C-c ? 5") 'decide-roll-1d5)
(define-key decide-mode-map (kbd "C-c ? 6") 'decide-roll-1d6)
(define-key decide-mode-map (kbd "C-c ? 7") 'decide-roll-1d7)
(define-key decide-mode-map (kbd "C-c ? 8") 'decide-roll-1d8)
(define-key decide-mode-map (kbd "C-c ? 9") 'decide-roll-1d9)
(define-key decide-mode-map (kbd "C-c ? 1 0") 'decide-roll-1d10)
(define-key decide-mode-map (kbd "C-c ? 1 2") 'decide-roll-1d12)
(define-key decide-mode-map (kbd "C-c ? 2 0") 'decide-roll-1d20)
(define-key decide-mode-map (kbd "C-c ? %") 'decide-roll-1d100)
(define-key decide-mode-map (kbd "C-c ? f") 'decide-roll-fate)
(define-key decide-mode-map (kbd "C-c ? a") 'decide-roll-1dA)
(define-key decide-mode-map (kbd "C-c ? A") 'decide-roll-2dA)
(define-key decide-mode-map (kbd "C-c ? r") 'decide-random-range)
(define-key decide-mode-map (kbd "C-c ? c") 'decide-random-choice)
(define-key decide-mode-map (kbd "C-c ? t") 'decide-from-table)

(setq
  projectile-project-search-path '("~/dev/"))

(require 'flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun mikkpr/JSXHook ()
  "My Hook for JSX Files"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (flycheck-select-checker 'javascript-eslint)
  (flycheck-mode))

(add-to-list 'magic-mode-alist '("import " . mikkpr/JSXHook))

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
  (kbd ", w") 'evil-avy-goto-word-0
  (kbd "SPC m r w") 'mikkpr/org-refile-to-work-log
  (kbd "SPC m r j") 'mikkpr/org-refile-to-journal
  (kbd ", a") 'evil-window-left
  (kbd ", d") 'evil-window-right)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
