(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Needed to byte-compile
(require 'package)

(use-package tramp
  :config
  (connection-local-set-profile-variables 'remote-find
                                          '((find-program . "/usr/bin/find")))
  (connection-local-set-profiles
   '(:application tramp :machine "mangoh_vm") 'remote-find))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . 'dired-open-in-external-app))
  :config
  (defun dired-open-in-external-app (&optional file)
    "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
    (interactive)
    (let ( doIt
           (myFileList
            (cond
             ((string-equal major-mode "dired-mode") (dired-get-marked-files))
             ((not file) (list (buffer-file-name)))
             (file (list file)))))

      (setq doIt (if (<= (length myFileList) 5)
                     t
                   (y-or-n-p "Open more than 5 files? ") ) )

      (when doIt
        (cond
         ((string-equal system-type "windows-nt")
          (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
         ((string-equal system-type "darwin")
          (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
         ((string-equal system-type "gnu/linux")
          (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )
  )

(use-package appearance
  :ensure nil
  :bind
  ("C-M-<f11>" . 'appearance-toggle-mode))

(use-package gerrit-getter)

(use-package whitespace
  :ensure nil
  :config (global-whitespace-mode))

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode)
  )

(setq load-prefer-newer t)

(use-package vc-dir
  :ensure nil
  :defines
  vc-fileset
  vc-dir-mode-map
  :functions
  vc-dir-hide-up-to-date
  vc-dir-refresh
  :config
  ;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
  ;; reset, and r run git reset and checkout from head.
  (defun my-vc-git-command (verb fn)
    (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
           (backend (car fileset-arg))
           (files (nth 1 fileset-arg)))
      (if (eq backend 'Git)
          (progn (funcall fn files)
                 (message (concat verb " " (number-to-string (length files))
                                  " file(s).")))
        (message "Not in a vc git buffer."))))

  (defun my-vc-git-add (&optional revision vc-fileset comment)
    (interactive "P")
    (my-vc-git-command "Staged" 'vc-git-register))

  (defun my-vc-git-reset (&optional revision vc-fileset comment)
    (interactive "P")
    (my-vc-git-command "Unstaged"
                       (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

  (define-key vc-prefix-map [(r)] 'vc-revert-buffer)
  (define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
  (define-key vc-prefix-map [(a)] 'my-vc-git-add)
  (define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
  (define-key vc-prefix-map [(u)] 'my-vc-git-reset)
  (define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)

  ;; hide up to date files after refreshing in vc-dir
  (define-key vc-dir-mode-map [(g)]
    (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date))))

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style (trailing
                     missing-newline-at-eof
                     empty
                     space-after-tab
                     space-before-tab::space
                     space-before-tab
                     tab-mark))
  :config
  (global-whitespace-mode 1))

(use-package god-mode
  :ensure t
  :functions
  sebe/god-mode-insert-at-point
  sebe/god-mode-update-mode-line-and-cursor
  sebe/god-mode-toggle-on-overwrite
  :bind
  ("M-i" . god-mode-all)
  ;; ("<f2>" . sebe/god-mode-insert-at-point)
  :functions
  idle-timer-callback-god-mode
  idle-timer-start-god-mode
  idle-timer-stop-god-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  (setq idle-timer-god-mode-timer nil)

  ;; Callback function
  (defun idle-timer-callback-god-mode ()
    (message "god mode is (%s)" (setq god-global-mode t)))

  ;; Start function
  (defun idle-timer-start-god-mode ()
    (interactive)
    (when (timerp idle-timer-god-mode-timer)
      (cancel-timer idle-timer-god-mode-timer))
    (setq idle-timer-god-mode-timer
          (run-with-timer 15 nil #'idle-timer-callback-god-mode)))

  ;; stop function
  (defun idle-timer-stop-god-mode ()
    (interactive)
    (when (timerp idle-timer-god-mode-timer)
      (cancel-timer idle-timer-god-mode-timer))
    (setq idle-timer-god-mode-timer nil))

  :config
  (defun sebe/god-mode-insert-at-point (comment text)
    "Insert some text without exiting god mode"
    (interactive "P\nMText to insert: ")
    (insert text)
    (if comment
        (comment-line 1))
    )

  (defun sebe/god-mode-toggle-on-overwrite ()
    "Toggle god-mode on overwrite-mode."
    (message "Toggle god-mode")
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

;;   (defun sebe/god-mode-update-mode-line ()
;;     "Toggle between differnet colors of both the cursor and
;; the modeline when toggling god-mode"
;;     (cond
;;      (god-local-mode
      ;; (set-face-attribute 'mode-line nil
      ;;                     :foreground "cadet blue"
      ;;                     :background "black")
;;       (setq cursor-type 'box))
;;      (t
;;       (set-face-attribute 'mode-line nil
;;                           :foreground "aquamarine"
;;                           :background "dark slate gray")
;;       (setq cursor-type 'hollow))
;;      )
;;     )
;;   (add-hook 'post-command-hook 'sebe/god-mode-update-mode-line)
  (add-hook 'overwrite-mode-hook 'sebe/god-mode-toggle-on-overwrite)

  )

(use-package pulsar
  :ensure t
  :custom
  (pulsar-face 'pulsar-green)
  :config
  (pulsar-global-mode)
  )

(use-package general
  :ensure t
  :defines
  sebe/main-leader-key
  sebe/math-leader-key
  sebe/edit-leader-key
  sebe/find-file-leader-key
  sebe/window-leader-key
  sebe/projectile-leader-key
  :config
  (defconst sebe/main-leader-key "C-.")
  (defconst sebe/avy-leader-key "C-�")
  (defconst sebe/mode-leader-key "C-^")
  (defconst sebe/math-follow-key (concat sebe/main-leader-key " m"))
  (defconst sebe/edit-follow-key (concat sebe/main-leader-key " e"))
  (defconst sebe/find-file-follow-key (concat sebe/main-leader-key " f"))
  (defconst sebe/window-follow-key (concat sebe/main-leader-key " w"))
  (defconst sebe/projectile-follow-key (concat sebe/main-leader-key " p"))
  (defconst sebe/org-follow-key (concat sebe/main-leader-key " o"))
  (defconst sebe/helm-follow-key (concat sebe/main-leader-key " h"))
  (general-create-definer sebe/main-leader-definer
    :prefix sebe/main-leader-key)
  (general-create-definer sebe/avy-leader-definer
    :prefix sebe/avy-leader-key)
  (general-create-definer sebe/mode-leader-definer
    :prefix sebe/mode-leader-key)
  (general-create-definer sebe/math-follow-definer
    :prefix sebe/math-follow-key)
  (general-create-definer sebe/edit-follow-definer
    :prefix sebe/edit-follow-key)
  (general-create-definer sebe/find-file-follow-definer
    :prefix sebe/find-file-follow-key)
  (general-create-definer sebe/window-follow-definer
    :prefix sebe/window-follow-key)
  (general-create-definer sebe/projectile-follow-definer
    :prefix sebe/projectile-follow-key)
  (general-create-definer sebe/org-follow-definer
    :prefix sebe/org-follow-key)
  (general-create-definer sebe/helm-follow-definer
    :prefix sebe/helm-follow-key)

  (general-define-key
   "M-x" 'helm-M-x
   "M-:" 'helm-eval-expression)

  (general-define-key
   :prefix "C-x"
   "p" 'prev-window
   "O" 'other-frame
   "C-b" 'persp-ibuffer
   "b" 'helm-buffers-list
   "C-f" 'helm-find-files
   "C-n" (kbd "C-x C-<right>")
   "C-p" (kbd "C-x C-<left>")
   "C-k" 'kill-buffer-and-window)

  (sebe/main-leader-definer
    "r" 'revert-buffer
    "C-f" 'ffap
    "d l" 'kill-whole-line)

  (sebe/avy-leader-definer
   "C-c" 'avy-goto-char-2
   "C-w" 'avy-goto-word-1
   "C-l" 'avy-goto-line
   "C-y" 'avy-copy-line
   "C-m" 'avy-move-line
   )

  (sebe/mode-leader-definer
    "C-o" 'outline-minor-mode
    "C-v" 'view-mode
   )

  (sebe/math-follow-definer
   "+" 'org-increase-number-at-point
   "-" 'org-decrease-number-at-point)

  (sebe/edit-follow-definer
   "s" 'flyspell-auto-correct-word
   "�" 'replicate-line
   "w" 'fixup-whitespace
   )

  (sebe/find-file-follow-definer
   "e" (lambda ()
         (interactive)
         (find-file "~/.emacs"))
   "d" (lambda ()
         (interactive)
         (find-file "~/AppData/Local/dhcpsrv2.5.2/dhcpsrv.ini"))
   )

  (sebe/window-follow-definer
   "s" 'toggle-window-split
   "f" 'fit-window-to-buffer)

  (sebe/projectile-follow-definer
   "s" 'helm-projectile-switch-project
   "f" 'helm-projectile-find-file
   "4 f" 'projectile-find-file-other-window
   "C-f" 'projectile-persp-switch-project
   "d" 'projectile-find-dir
   "4 d" 'projectile-find-dir-other-window
   "C-r" 'helm-projectile-recentf
   "r" 'projectile-dired
   "4 r" 'projectile-dired-other-window
   "g" 'projectile-vc)

  (sebe/org-follow-definer
   "s" 'org-store-link
   "a" 'org-agenda
   "c" 'org-capture
   "O" 'org-clock-out)

  (sebe/helm-follow-definer
   "g" 'helm-grep-do-git-grep
   "o" 'helm-occur)
  )

(use-package project
  :disabled t)

(use-package perspective
  :ensure t
  :custom (persp-mode-prefix-key (kbd "C-\""))
  :init (persp-mode))

;; Sometime when time is abundant, this could be fixed
;;
;; (use-package ibuffer-persp
;;   :ensure nil
;;   :after (perspective)
;;   :load-path "~/git/personal/config/.emacs.d/lisp/ibuffer-persp/ibuffer-persp.el"
;;   )

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  )


;; PATH modifictaions ==========================================================
;; Pointing to the right find.exe

;; FIX THE FIND PROGRAM
(cond
 ((string= (system-name) "LT-JRW6NN3")
  (setq find-program "\"c:\\Program Files\\Git\\usr\\bin\\find.exe\"")
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name explicit-shell-file-name)))

;; COMPLETION SYSTEM
;; Might need to comment this out, as the setup is not done

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  )

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-gtags
  :defer t
  :functions
  helm-gtags-dwim
  helm-gtags-pop-stack
  helm-gtags-previous-history
  helm-gtags-next-history
  :bind
  (:map helm-gtags-mode-map
        ("M-." . 'helm-gtags-dwim)
        ("M-," . 'helm-gtags-pop-stack)
        ("M-�" . 'helm-gtags-select)
        ("C-c <" . 'helm-gtags-previous-history)
        ("C-c >" . 'helm-gtags-next-history))
  :hook
  (c-mode . helm-gtags-mode)
  (c++-mode . helm-gtags-mode)
  (asm-mode . helm-gtags-mode)
  :custom
  (helm-gtags-auto-update t))

(use-package company
  ;; :hook
  ;; (c-mode-hook . company-mode)
  ;; (c++-mode-hook . company-mode)
  :bind ("C-M-i" . 'helm-company)
  :defines
  company-backends
  :custom
  (company-backends (delete 'company-semantic company-backends))
  :config
  ;; (setq company-backends (delete 'company-semantic company-backends))
  (global-company-mode 1)
  )

(use-package cc-mode
  :ensure nil
  :hook
  (c-mode . (lambda () (c-guess)))
  :config
  (define-key c-mode-map  [(tab)] 'c-indent-line-or-region)
  (define-key c++-mode-map  [(tab)] 'c-indent-line-or-region))

(use-package emacs-lisp-mode
  :hook
  (emacs-lisp-mode . helm-gtags-mode))

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))

;; On save
;; (add-hook 'write-file-functions 'delete-trailing-whitespace)

;; Long line behaviour
(set-default 'truncate-lines t)

;; UI appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(display-time-mode 0)

;; line numbers
(global-display-line-numbers-mode t)

(defcustom display-line-numbers-exempt-modes
  '(eshell-mode
    shell-mode
    ansi-term-mode
    tex-mode latex-mode
    org-mode
    help-mode
    eww-mode
    compilation-mode
    markdown-mode)
  "Major modes on which to disable line numbers"
  :require 'display-line-numbers
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'"
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; Show lines and column on modeline
(line-number-mode 1)
(column-number-mode 1)

;; Display time on mode line
(setq display-time-24hr-format t)
(display-time-mode t)

;; Window management =================================================

(defun setup-display-buffer-base-action (frame)
  "Set the default way that windows open based on the proportions
for the screen

If the workarea is wider than it is high, then open to the right,
else below. Takes the frame to change as argument
"
  (let ((current_direction (if (>
                                (frame-outer-width frame)
                                (frame-outer-height frame))
                               (quote right)
                             (quote bottom))))
    (setq
     display-buffer-base-action
     `((display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-in-direction)
       (reusable-frames . t)
       (mode Info-mode
             dired-mode
             compilation-mode
             help-mode)
       (direction . ,current_direction)))))

(add-hook 'window-size-change-functions 'setup-display-buffer-base-action)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun prev-window ()
	(interactive nil)
	(other-window -1))

(setq window-min-height 10)
(setq window-min-width 80)

;; version control helper ============================================

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; ====== HOTKEYS ====================================================

(defun replicate-line (&optional number-of-yanks)
  "Kill a whole line from anywhere in it then yank it `number-of-yanks' times"
  ;; Getting the arguments in line
  (interactive "^p")
  (or (+ number-of-yanks 1) (setq number-of-yanks 2))

  ;; Killing a line and yanking it according to arguments
  (kill-whole-line 0)
  (dotimes (i number-of-yanks)
    ;;(message "yank %d" i)
    (yank)
    (newline))
  (yank))

;; Electric pairing (parenthesis, brackets etc)
(setq electric-pairs '(
(?\( . ?\))
(?\[ . ?\])
(?\{ . ?\})
(?\" . ?\")
(?\' . ?\')
))
(electric-pair-mode t)

;; ================= MODE SPECIFICS ============================================

;; c mode ======================================================================

;; dtrt-indent =================================================================
(use-package dtrt-indent
  :disabled t
  :ensure nil
  :config (dtrt-indent-global-mode t)
  )


;; Markdown mode ===============================================================
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :hook (markdown-mode . flyspell-mode)
  :init (setq markdown-command "multimarkdown"))

;; Olivetti mode ===============================================================
(use-package olivetti
  :ensure t
  :demand t
;;  :hook (org-capture-mode)
  :functions olivetti-set-width
  :config (olivetti-set-width 100)
  (auto-fill-mode 1)
  )

;; Org mode ====================================================================

(use-package flyspell
  :hook
  (vc-git-log-edit-mode . flyspell-mode)
  :config
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (cond
   ((string= (system-name) "LT-JRW6NN3")
    (setq ispell-program-name "~/AppData/Local/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")))
  )

(use-package org
  :ensure t
  :defines
  org-default-todo-file
  org-default-journal-file
  org-default-books-file
  org-default-jira-file
  :functions
  sebe/get-prop-ID-from-jira-buf
  org-property-values
  org-clock-auto-clockout-insinuate
  :hook
  (org-mode . flyspell-mode)
  (org-mode . auto-fill-mode)
  (org-capture-mode . olivetti-mode)
  (org-capture-mode . (lambda ()
                        (god-local-mode 0)))
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
      "* TODO %?\n  %i\n  %a")

     ("n" "Notes" entry (file+function org-default-notes-file org-goto)
      "* %?")

     ("c" "Clocking" plain (clock) "%?" :unnarrowed t)

     ("j" "Journal")

     ("js" "Start day" entry (file+olp+datetree
                              org-default-journal-file)
      "* [%<%H:%M>] Started\n\nChecklist\n\
- [ ] Set location%?\n\
- [ ] Report time\n\
- [ ] News\n\
- [ ] Mail/Meetings\n\
- [ ] Agenda\n\
- [ ] Jira\n\
- [-] Gerrit" :clock-in t :clock-keep t :unnarrowed t)

     ("jq" "Quit day" entry (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Quit\n%?"
      :immediate-finish t)

     ("jp" "Pause" entry (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Paused\n%i" :clock-in t :clock-keep t)

     ("jj" "Work on Jira" entry (file+function org-default-jira-file
                                               org-goto)
      "* %<%y-%m-%d %A>\n\n%?" :clock-in t :clock-keep t )
     ("jJ" "New Jira" entry (file+function org-default-jira-file
                                           org-goto)
      "* %^{Jira title}
:PROPERTIES:
:JIRA:  [[https://jira.hms.se/browse/A%^{Jira Number}][A%\2]]
:ID: %\2
:END:\n%?")

     ("jl" "General Log" entry (file+olp+datetree
                                org-default-journal-file)
      "* [%<%H:%M>] Log
%^{PROJECT}p
%(sebe/org-capture-template-workon-jira)%?")

     ("jm" "Meeting " entry (file+olp+datetree
                             org-default-journal-file)
      "* [%<%H:%M>][%^{Minutes}m] Meeting%?\n%^{TOPIC}p%^{PROJECT}p")

     ("b" "Book" entry (file+olp+datetree
                        org-default-books-file)
      "* [%<%H:%M>]
Book: %^{Book-title}p
Pages: %^{first page}-%?
Take-aways: ")))
  (org-agenda-files (list org-default-journal-file
                          org-default-notes-file
                          org-default-todo-file
                          org-default-jira-file))
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-todo-keyword-faces '(("TODO" . org-todo)
                            ("IN-PROGRESS". org-in-progress)
                            ("DONE" . org-done)))
  (org-use-speed-commands t)
  (org-clock-continuously t)
  :init
  (setq org-directory "~/Documents/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-todo-file (concat org-directory "/todos.org"))
  (setq org-default-journal-file (concat org-directory "/journal.org"))
  (setq org-default-books-file (concat org-directory "/books.org"))
  (setq org-default-jira-file (concat org-directory "/jira.org"))

  :config
  (defun sebe/get-prop-ID-from-jira-buf ()
    "Specific funtion for getting the IDs from the jira.org buffer.
This is for easy linking"
    (interactive)
    (let ((buf-name "jira.org"))
      (if (bufferp (get-buffer buf-name))
          (with-current-buffer buf-name
            (org-property-values "ID"))
        (message "Jira buffer is not open"))))
  (defun sebe/org-capture-template-workon-jira ()
    "Function returning a string to be inserted as the template for a
                        workon jira capture"
    (let ((jira-id (completing-read "Jira ID: "
                                    (sebe/get-prop-ID-from-jira-buf))))
      (if (string= jira-id "")
          (format "")
        (format "Working on[[id:%1$s][%1$s]]." jira-id))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)))
  (defface org-in-progress
    '((((class color) (min-colors 88) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 88) (background dark))
       :foreground "medium turquoise"))
    "Face for TODO-tasks tagged with IN-PROGRESS"
    :group 'org-faces)
  (org-clock-auto-clockout-insinuate)
  )

;; Tex mode ====================================================================

(defun tex-init-addons ()
  "Unnecessary function to start latex in visual line mode"
  ()
  (visual-line-mode t)
  (turn-on-reftex)
  (highlight-regexp "\\\\todo\\([swq]\\|.+?}\\)" 'hi-yellow))

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . tex-init-addons)
  :defines TeX-arg-input-file-search
  :functions TeX-add-symbols
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '("../references.bib"))
  (setq TeX-arg-input-file-search nil)
  :config
  (TeX-add-symbols
   '("todo" [ "Options" ] "What")
   '("acrshort" "idn") ;; These could likely be optimized with a list of the available acronyms which exists in LaTeX.el...
   '("acrlong" "idn")
   '("newacronym" "idn" "Short" "Long")
   '("SI" "Number" "Units")
   )
  )

;; DocView Mode ================================================================

(use-package doc-view
  :hook
  (doc-view-mode . auto-revert-mode)
  (doc-view-mode . blink-cursor-mode))


;; LSP mode ====================================================================

(use-package lsp-mode
  :hook
  (python-mode . lsp-mode)
  :commands
  lsp
  lsp-find-definition
  lsp-find-references
  :bind
  (:map lsp-mode-map
        ("M-." . 'lsp-find-definition)
        ("M-," . 'lsp-find-references))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-diagnostics-provider :none)
  )

;; Python mode =================================================================
(use-package python
  :interpreter "ipython"
  :custom
  (python-shell-interpreter-args "-i")
  :hook
  (python-mode . yas-minor-mode)
  :config)

(use-package company-jedi
  :disabled t
  :after python
  :hook (python-mode . jedi:setup)
  :config
  (setq jedi:complete-on-dot nil)
  (add-to-list 'company-backends 'company-jedi)
  )

(use-package elpy
  :disabled t
  :ensure t
  :defer t
  :bind
  (:map python-mode-map
        ("M-." . 'elpy-goto-definition)
        ("M-," . 'elpy-goto-assignment))
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq 'elpy-rpc-python-command "c:/Users/sebbe/AppData/Local/Programs/Python/Python39/python.exe"))

(use-package py-autopep8
  :disabled t
  :ensure t
  :defer t
  :hook (elpy-mode . py-autopep8-mode)
  :disabled t
  :ensure t
  :hook (python-mode . pipenv-mode))

;; Matlab/octave mode ==========================================================

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Compilation mode ============================================================
(add-hook 'compilation-mode-hook (lambda()
                                   (visual-line-mode t)))

;; ================= GENERAL PURPOSE FUNCTIONS =================================

(defun next-double-window ()
    (interactive nil)
        (other-window 2))

(defun comment-arrow (&optional endline)
  "Funciton for making an arrow ending at col 80 or col endline"
  (interactive "P")
  (or endline (setq endline 80))
  (let (col_diff '(- endline (current-column)))
  (if (< col_diff 4)
      (error "No place for arrow ")
    (insert comment-start "<")
    (insert (make-string (- col_diff 3) ?=)))
  ))

(defun sebe/re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun sebe/print-oneline-git-commit ()
    "Function fore returning the current commit hash and short
                         description"
    (replace-regexp-in-string "\n\\'" ""
                              (shell-command-to-string "git log -1 --oneline")))

;; ================= GLOBAL VARIABLES ==========================================

(cond
 ((string= (system-name) "LT-JRW6NN3")
  (setq exec-path '("c:/Users/sebe/bin"
                    "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI"
                    "C:/Program Files (x86)/VMware/VMware Workstation/bin/"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Python38/"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/"
                    "C:/Emacs/emacs-28.1/bin"
                    "C:/Program Files (x86)/Plantronics/Spokes3G/"
                    "C:/Program Files/iperf-3.1.3-win64"
                    "c:/Program Files/libMultiMarkdown 6.6.0/bin"
                    "C:/WINDOWS/system32/config/systemprofile/scripts"
                    "C:/WINDOWS/system32/config/systemprofile/bin"
                    "C:/Program Files/gs/gs10.00.0/bin"
                    "C:/Program Files/Git/cmd"
                    "C:/Program Files/Git/usr/bin"
                    "C:/WINDOWS/system32"
                    "C:/WINDOWS"
                    "C:/WINDOWS/System32/Wbem"
                    "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin"
                    "C:/Program Files (x86)/CMake/bin"
                    "C:/Program Files/Git/mingw64/bin"
                    "C:/Program Files/nodejs/"
                    "C:/WINDOWS/System32/WindowsPowerShell/v1.0/"
                    "C:/Program Files/Microsoft VS Code/bin"
                    "C:/Program Files/PowerShell/7/"
                    "C:/Users/sebe/AppData/Local/glo668wb/bin"
                    "C:/Program Files/doxygen/bin"
                    "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Python38/"
                    "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/"
                    "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps"
                    "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/"
                    "C:/Program Files (x86)/Nmap"
                    "C:/Users/sebe/AppData/Roaming/npm"
                    "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))))

(setq enable-remote-dir-locals t)


;; ================= MODELINE MANAGEMENT =======================================

(setq-default mode-line-format
              '("%e"
                sen-modeline-remote
                sen-modeline-modal
                " "
                sen-modeline-git-branch
                sen-modeline-buffer
                " C%c  "
                sen-modeline-major-mode
                " "
                sen-modeline-perspective
                prot-modeline-narrow
                prot-modeline-align-right
                sen-modeline-org-clock
                sen-modeline-clock))

(defface sen-modeline-remote-indicator
  '((default :inherit bold)
    (t :inherit bold :background "orange3"))
  "Face for modeline remote indicator."
  :group 'sen-modeline-faces)

(defface sen-modeline-highlight
    '((default :inherit bold)
    (t :inherit bold :background "orange3"))
  "Face for modeline highlighting."
  :group 'sen-modeline-faces)

(defface sen-modeline-modal-indicator
  '((default :inherit bold)
    (t :inherit bold :background "navy"))
  "Face for modal mode indicator."
  :group 'sen-modeline-faces)

(defface sen-modeline-buffer-modified
  '((t :inherit mode-line-emphasis
       :foreground "grey90"))
  "Face for buffer notifying about unsaved changes."
  :group 'sen-modeline-faces)

(defface sen-modeline-narrow
  '((t :foreground "white"))
  "Face for modal mode indicator."
  :group 'sen-modeline-faces)

(defvar-local sen-modeline-remote
  '(:eval
    (when (file-remote-p default-directory)
      (propertize " @ "
                  'face 'sen-modeline-remote-indicator)))
  "Mode line construct to indicate editing remote files")

(defvar-local sen-modeline-buffer
  '(:eval (propertize (concat (buffer-name) " ")
                      'face (if (buffer-modified-p)
                                'sen-modeline-buffer-modified
                              'mode-line-emphasis))))

(defun sen-modeline-modal-mode ()
  "Return a special string based on if a `god-local-mode' or
`view-mode' is active"
  (cond
   (god-local-mode " G ")
   (view-mode " V ")
   (t "")))

(defvar-local sen-modeline-modal
  '(:eval
    (propertize (sen-modeline-modal-mode)
                'face 'sen-modeline-modal-indicator))
  "String variable with the current editing mode representation")

(defun sen-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local sen-modeline-major-mode
  '(:eval (sen-modeline-major-mode-name))
  "Variable containing the representation of the current major mode
as a string")

(defun sen-modeline-get-git-branch (file)
  "If applicable, return the git branch of the currently visited file"
  (let ((git-ref (vc-git--symbolic-ref file)))
    (if git-ref
        (concat git-ref ":"))))

(defvar-local sen-modeline-git-branch
  '(:eval
    (when (mode-line-window-selected-p)
      (sen-modeline-get-git-branch (buffer-file-name))))
  "Variable containing the branch of the currently visited buffer")

(defvar-local sen-modeline-perspective
  '(:eval
    (when (mode-line-window-selected-p)
      (format "[%s]" (persp-current-name)))))

(defvar-local sen-modeline-clock
  '(:eval
    (when (mode-line-window-selected-p)
      (format-time-string "%H:%M"))))

(defvar-local sen-modeline-org-clock
  '(:eval
    (when (mode-line-window-selected-p)
      org-mode-line-string)))

;;;; prot's stuff
(defvar-local prot-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'sen-modeline-narrow)))
  "Mode line construct to report the multilingual environment.")

(defun prot-modeline--right-align-rest ()
  "Return string if everything after `prot-modeline-align-right'."
  (format-mode-line
   `(""
     ,@(cdr (memq 'prot-modeline-align-right mode-line-format)))))

(defun prot-modeline--right-align-width ()
  "Return pixel width of `prot-modeline--right-align-rest'."
  (string-pixel-width (prot-modeline--right-align-rest)))

(defun prot-modeline--box-p ()
  "Return non-nil if the `mode-line' has a box attribute."
  (and (face-attribute 'mode-line :box)
       (null (eq (face-attribute 'mode-line :box) 'unspecified))))

;; NOTE 2023-07-13: I could also do what I am doing in
;; `fontaine--family-list-variable-pitch' and check if the family is a
;; member of those, but I don't need that as I always inherit
;; `variable-pitch' in my themes instead of hardcoding the family.
(defun prot-modeline--variable-pitch-p ()
  "Return non-nil if the `mode-line' inherits `variable-pitch'."
  (when-let* ((mode-line-inherit (face-attribute 'mode-line :inherit))
              ((string-match-p "variable-pitch" (symbol-name mode-line-inherit)))
              (family-face (face-attribute mode-line-inherit :inherit))
              (variable-pitch
               (if (listp family-face)
                   (memq 'variable-pitch family-face)
                 (eq 'variable-pitch family-face))))
    variable-pitch))

;; I just came up with this experimentally, but I am not sure if it is
;; the best approach.
(defun prot-modeline--magic-number ()
  "Return constant for use in `prot-modeline-align-right'."
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local prot-modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       (let ((box-p (prot-modeline--box-p))
             (variable-pitch-p (prot-modeline--variable-pitch-p))
             (magic-number (prot-modeline--magic-number)))
         `(space
           :align-to
           (- right
              right-fringe
              right-margin
              ,(ceiling
                (prot-modeline--right-align-width)
                (string-pixel-width (propertize "m" 'face 'mode-line)))
              ,(cond
                ;; FIXME 2023-07-13: These hardcoded numbers are
                ;; probably wrong in some case.  I am still testing.
                ((and variable-pitch-p box-p)
                 (* magic-number 0.5))
                ((and (not variable-pitch-p) box-p)
                 (* magic-number 0.25))
                ((and variable-pitch-p (not box-p))
                 0)
                ;; No box, no variable pitch, but I am keeping it as
                ;; the fallback for the time being.
                (t (* magic-number -0.1))))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(dolist (make-var-risky '(sen-modeline-remote
                          sen-modeline-modal
                          sen-modeline-buffer
                          sen-modeline-major-mode
                          sen-modeline-git-branch
                          sen-modeline-perspective
                          prot-modeline-narrow
                          prot-modeline-align-right
                          sen-modeline-clock
                          sen-modeline-org-clock))
  (put make-var-risky 'risky-local-variable t))


;; ================= SPECIAL PURPOSE FIXES =====================================


(setq sentence-end-double-space nil)

;; ;; Workaround to make the keyboard work again?! Avoiding dead keys
;; (define-key key-translation-map [dead-grave] "`")
;; (define-key key-translation-map [dead-acute] "'")
;; (define-key key-translation-map [dead-circumflex] "^")
;; (define-key key-translation-map [dead-diaeresis] "\"")
;; (define-key key-translation-map [dead-tilde] "~")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "#d0d0d0"])
 '(auto-save-default nil)
 '(company-idle-delay nil)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-enabled-themes '(manoj-dark))
 '(dired-listing-switches "-alh")
 '(display-buffer-alist
   '(("\\*Help\\*"
      (display-buffer-same-window))
     ("\\*Compilation.*" display-buffer-same-window)
     ("\\*Find.*" display-buffer-same-window)
     ("\\*Customize" display-buffer-same-window)
     ("\\*vc-.*" display-buffer-same-window)))
 '(display-buffer-base-action '(display-buffer-in-side-window (side . right)))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(elpy-formatter 'autopep8)
 '(elpy-get-info-from-shell t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-autodoc elpy-module-sane-defaults))
 '(elpy-project-root-finder-functions
   '(elpy-project-find-projectile-root elpy-project-find-git-root))
 '(elpy-rpc-python-command
   "c:/Users/sebbe/AppData/Local/Programs/Python/Python39/python.exe")
 '(elpy-syntax-check-command "pycodestyle")
 '(exec-path
   '("c:/Users/sebe/bin" "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI" "C:/Program Files (x86)/VMware/VMware Workstation/bin/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/" "C:/Emacs/emacs-28.1/bin" "C:/Program Files (x86)/Plantronics/Spokes3G/" "C:/Program Files/iperf-3.1.3-win64" "C:/WINDOWS/system32/config/systemprofile/scripts" "C:/WINDOWS/system32/config/systemprofile/bin" "C:/Program Files/gs/gs10.00.0/bin" "C:/Program Files/Git/cmd" "C:/Program Files/Git/usr/bin" "C:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Program Files (x86)/CMake/bin" "C:/Program Files/Git/mingw64/bin" "C:/Program Files/nodejs/" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/PowerShell/7/" "C:/Users/sebe/AppData/Local/glo668wb/bin" "C:/Program Files/doxygen/bin" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Program Files/Python/Python310/Scripts/" "C:/Program Files/Python/Python310/" "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/" "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps" "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/" "C:/Program Files (x86)/Nmap" "C:/Users/sebe/AppData/Roaming/npm" "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))
 '(fringe-mode 0 nil (fringe))
 '(ibuffer-saved-filter-groups '(("no-helm" ("no-helm" (not name . "\\*helm")))))
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(inhibit-startup-screen t)
 '(ispell-program-name "~/AppData/Local/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
 '(kill-buffer-delete-auto-save-files t)
 '(mode-line-format
   '((:eval display-time-string)
     "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-auto-compile
     (vc-mode vc-mode)
     mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps nil)
 '(outline-minor-mode-prefix " ")
 '(package-selected-packages
   '(helm-projectile avy use-package pulsar helm-lsp lsp-mode elpy projectile-ripgrep light-mode flycheck persp-projectile general company-jedi helm-tramp py-autopep8 olivetti projectile perspective magit god-mode pipenv helm auctex))
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "*__pycache__"))
 '(projectile-project-search-path '("~/git/"))
 '(python-check-command "pyflakes.exe")
 '(python-skeleton-autoinsert t)
 '(safe-local-variable-values
   '((org-goto-max-level . 2)
     (org-todo-keywords
      (sequence "TODO" "IN-PROGRESS" "DONE"))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN-PROGRESS" "|" "DONE")))))
 '(same-window-regexps nil)
 '(scroll-margin 5)
 '(show-paren-mode t)
 '(smerge-command-prefix "\33")
 '(split-height-threshold nil)
 '(split-width-threshold 160))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "LightSkyBlue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
;;  '(compilation-info ((t (:foreground "LightPink4" :weight bold))))
;;  '(compilation-warning ((t (:foreground "Orange4" :weight bold))))
;;  '(cursor ((t (:background "lavender"))))
;;  '(custom-button ((t (:background "seashell" :foreground "black" :box (:line-width (2 . 2) :style pressed-button)))))
;;  '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow4" :weight bold))))
;;  '(font-lock-comment-delimiter-face ((t (:foreground "dark sea green"))))
;;  '(font-lock-comment-face ((t (:foreground "peach puff" :slant oblique))))
;;  '(font-lock-constant-face ((t (:foreground "SkyBlue1" :weight bold))))
;;  '(font-lock-doc-face ((t (:foreground "spring green" :slant oblique))))
;;  '(font-lock-function-name-face ((t (:foreground "light blue" :weight bold))))
;;  '(font-lock-keyword-face ((t (:foreground "cyan3"))))
;;  '(font-lock-string-face ((t (:foreground "lavender"))))
;;  '(font-lock-type-face ((t (:foreground "light steel blue" :slant italic))))
;;  '(helm-selection ((t (:extend t :distant-foreground "black" :box (:line-width (2 . 2) :color "grey75" :style released-button) :weight bold))))
;;  '(helm-source-header ((t (:extend t :background "#22083397778B" :foreground "white" :weight bold :family "Sans Serif"))))
;;  '(hi-yellow ((t (:background "orange4" :foreground "black"))))
;;  '(mode-line ((t (:inherit mode-line-buffer-id :background "sky blue" :foreground "Blue" :slant normal :height 0.95))))
;;  '(mode-line-buffer-id ((t (:inherit mode-line :slant italic :weight bold :height 1.0))))
;;  '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
;;  '(mode-line-inactive ((t (:background "black" :foreground "light blue" :box nil :weight light :height 0.9))))
;;  '(region ((t (:extend t :background "steel blue"))))
;;  '(widget-field ((t (:background "gray15"))))
;;  '(window-divider ((t (:foreground "black")))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:inherit default :background "black"))))
 '(mode-line ((t (:foreground "cadet blue" :height 1.0))))
 '(mode-line-active ((t (:inherit mode-line :background "grey15" :box (:line-width (1 . 5) :color "grey15" :style flat-button)))))
 '(mode-line-inactive ((t (:background "grey8" :foreground "grey80" :box (:line-width (2 . 5) :color "grey8" :style flat-button) :weight light :height 1.0)))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "LightSkyBlue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
 '(compilation-info ((t (:foreground "LightPink4" :weight bold))))
 '(compilation-warning ((t (:foreground "Orange4" :weight bold))))
 '(cursor ((t (:background "lavender"))))
 '(custom-button ((t (:background "seashell" :foreground "black" :box (:line-width (2 . 2) :style pressed-button)))))
 '(diff-header ((t (:extend t :background "grey90"))))
 '(diff-refine-added ((t (:background "dark green" :inherit diff-refine-changed))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "dark red"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow4" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "dark sea green"))))
 '(font-lock-comment-face ((t (:foreground "peach puff" :slant oblique))))
 '(font-lock-constant-face ((t (:foreground "SkyBlue1" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "spring green" :slant oblique))))
 '(font-lock-function-name-face ((t (:foreground "light blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "cyan3"))))
 '(font-lock-string-face ((t (:foreground "lavender"))))
 '(font-lock-type-face ((t (:foreground "light steel blue" :slant italic))))
 '(helm-selection ((t (:extend t :distant-foreground "black" :box (:line-width (2 . 2) :color "grey75" :style released-button) :weight bold))))
 '(helm-source-header ((t (:extend t :background "#22083397778B" :foreground "white" :weight bold :family "Sans Serif"))))
 '(hi-yellow ((t (:background "orange4" :foreground "black"))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "black" :foreground "light blue" :box nil :weight light :height 0.9))))
 '(persp-selected-face ((t (:inherit font-lock-constant-face :weight normal))))
 '(region ((t (:extend t :background "purple4"))))
 '(widget-field ((t (:background "gray15"))))
 '(window-divider ((t (:foreground "black")))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
