(let ((secret.el (expand-file-name "secret.el" "~/.doom.d")))
  (when (file-exists-p secret.el)
    (load secret.el)))

(defun set-windows-paths ()
  (defvar dropbox-path "D:/Dropbox/org")
  (setq org-gcal-file-alist '(("oinasz@gmail.com" . "D:\Dropbox\org\schedule.org")))
  )

(defun set-linux-paths ()
  (defvar dropbox-path "~/Dropbox/org")
  (setq org-gcal-file-alist '(("oinasz@gmail.com" . "~\org\schedule.org")))
  )

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (set-windows-paths)
    ))
 ((string-equal system-type "gnu/linux")
  (progn
    (set-linux-paths)
    ))
 )
(defvar org-home-file-path (expand-file-name "home.org" dropbox-path))
(defvar org-work-file-path (expand-file-name "work.org" dropbox-path))
(defvar org-schedule-file-path (expand-file-name "schedule.org" dropbox-path))

(setq doom-font (font-spec :family "Cascadia Code" :size 12))

(setq doom-theme 'doom-tomorrow-night)

(setq display-line-numbers-type t)

(setq tab-width 2)
(setq standard-indent 2)

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

(setq max-mini-window-height 0.5)

(use-package org-expiry :defer t)

(setq org-log-state-notes-into-drawer t
      org-habit-preceding-days 5
      org-habit-following-days 5)

(setq org-agenda-files (list
                           org-home-file-path
                           org-work-file-path
                           ))

(defun mikkpr/open-home-org ()
  (interactive)
  (find-file org-home-file-path))
(defun mikkpr/open-work-org ()
  (interactive)
  (find-file org-work-file-path))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "TESTING(T)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(setq org-display-custom-times t
      org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))

(setq org-capture-templates
'(("p" "Home" entry
  (file+headline org-home-file-path "Inbox")
  "* %?\n%i\nCREATED: %u" :prepend t)
 ("l" "Work log entry" entry (file+olp+datetree org-work-file-path "Log")
  "* %?\nCREATED: %u" :prepend t :jump-to-captured t)
 ("j" "Journal entry" entry (file+olp+datetree org-home-file-path "Journal")
  "* %?\nCREATED: %u" :prepend t :jump-to-captured t)
 ("w" "Work" entry
  (file+headline org-work-file-path "Inbox")
  "* %?\n%i\nCREATED: %u" :prepend t)))

(use-package org-gcal :defer t)

(use-package org-super-agenda :defer t)
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
        org-agenda-start-on-weekday nil
        org-super-agenda-groups '((:name "Today"
				:time-grid t
				:scheduled today)
			   (:name "Due today"
				:deadline today)
			   (:name "Important"
				:priority "A")
			   (:name "Overdue"
				:deadline past)
			   (:name "Due soon"
				:deadline future)
			   (:name "Waiting"
			       :todo "WAIT"))
        )
  :config
  (org-super-agenda-mode)
  )

(add-hook 'org-insert-heading-hook
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (org-expiry-insert-created))))

(use-package org-datetree :defer t)
(use-package org-reverse-datetree :defer t)

(defun mikkpr/org-refile-to-work-log (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file org-work-file-path "Log"
                                       :ask-always arg :prefer '("SCHEDULED" "CREATED_TIME" "CREATED_AT" "CLOSED")))

(defun mikkpr/org-refile-to-journal (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file org-home-file-path "Journal"
                                       :ask-always arg :prefer '("SCHEDULED" "CREATED_TIME" "CREATED_AT" "CLOSED")))

(set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.25)

(setq org-agenda-custom-commands
      '(("." "Overview (Work)"
         ((agenda ""
                  ((org-agenda-span 5)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-show-future-repeats 'next)
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-overriding-header "* Calendar\n")
                   (org-agenda-files (list org-work-file-path))))
          (todo ""
                ((org-agenda-overriding-header "\n* Open\n")
                 (org-agenda-block-separator nil)
                 (org-agenda-sorting-strategy '(todo-state-up priority-up timestamp-up habit-down))
                 (org-agenda-todo-ignore-scheduled 'all)
                 (org-agenda-files (list org-work-file-path))))
          ))
        ("," "Overview (Personal)"
         ((agenda ""
                  ((org-agenda-span 5)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-show-future-repeats 'next)
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-overriding-header "* Calendar\n")
                   (org-agenda-files (list org-home-file-path))))
          (todo ""
                ((org-agenda-overriding-header "\n* Open\n")
                 (org-agenda-block-separator nil)
                 (org-agenda-sorting-strategy '(todo-state-up priority-up timestamp-up habit-down))
                 (org-agenda-todo-ignore-scheduled 'all)
                 (org-agenda-files (list org-home-file-path))))
          ))
      ))

(use-package org-agenda-property :defer t)
(setq org-agenda-property-list '("status"))
(setq org-agenda-property-position 'where-it-fits)

(setq org-habit-show-habits-only-for-today t)

;; Spotify settings
(use-package spotify
  :defer t
  :config
        (setq spotify-transport 'dbus)
        (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)
  )

(setq
  projectile-project-search-path '("~/dev/"))

(add-to-list 'projectile-globally-ignored-files "npm-shrinkwrap.json")

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

(use-package discover :defer t)
(use-package makey :defer t)
(global-discover-mode 1)

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
