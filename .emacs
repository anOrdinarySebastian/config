(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa". "https://melpa.org/packages/"))

;; Adding path to all my own packages
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; Installing straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package dired
  :ensure nil
  :functions
  dired-open-in-external-app
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
          (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) myFileList))
         ((string-equal system-type "darwin")
          (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  myFileList))
         ((string-equal system-type "gnu/linux")
          (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) myFileList)))))))

(use-package appearance
  :disabled t
  :ensure nil
  :functions
  appearance-toggle-mode
  :bind
  ("C-M-<f11>" . 'appearance-toggle-mode))

(use-package gerrit-getter
  ;; Don't forget to make the symbolic link
  :load-path "lisp/gerrit-el"
  :ensure nil)

(use-package vc-git
  :ensure nil
  :demand t
  :functions
  vc-git--symbolic-ref)

(use-package vc-dir
  :ensure nil
  :demand t ;; This is needed as we're using vc in the modeline
  :defines
  vc-fileset
  vc-dir-mode-map
  :functions
  my-vc-git-command
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
  :config
  (global-whitespace-mode 1))

(use-package god-mode
  :ensure t
  :functions
  sebe/god-mode-insert-at-point
  sebe/god-mode-update-mode-line-and-cursor
  sebe/god-mode-toggle-on-overwrite
  idle-timer-callback-god-mode
  idle-timer-start-god-mode
  idle-timer-stop-god-mode
  :defines
  god-mode-alist
  :bind
  (:map god-local-mode-map
        ("z" . #'repeat)
        ("i" . #'sebe/god-mode-insert-at-point))
  :custom
  (god-mode-alist '((nil . "C-") ("g" . "M-") ("h" . "C-M-") ("H" . "s-")))
  :init
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
        (comment-line 1)))

  (defun sebe/god-mode-toggle-on-overwrite ()
    "Toggle god-mode on overwrite-mode."
    (message "Toggle god-mode")
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

  (add-hook 'overwrite-mode-hook 'sebe/god-mode-toggle-on-overwrite)
  (setq god-mode-enable-function-key-translation nil)
  (add-to-list 'god-exempt-major-modes 'diff-mode))

(use-package pulsar
  :disabled t
  :ensure t
  :custom
  (pulsar-face 'pulsar-green)
  :config
  (pulsar-global-mode))

(use-package avy
  :ensure t)

(use-package general
  :ensure t
  :after copilot
  :defines
  sebe/main-leader-key
  sebe/math-leader-key
  sebe/edit-leader-key
  sebe/find-file-leader-key
  sebe/window-leader-key
  sebe/projectile-leader-key

  sebe/math-follow-definer
  sebe/edit-follow-definer
  sebe/find-file-follow-definer
  sebe/window-follow-definer
  sebe/projectile-follow-definer
  sebe/helm-follow-definer
  sebe/org-follow-definer
  sebe/yas-follow-definer
  sebe/copilot-follow-definer
  :config
  (defconst sebe/main-leader-key "C-.")
  (defconst sebe/avy-leader-key "C-ö")
  (defconst sebe/mode-leader-key "C-^")
  (defconst sebe/math-follow-key (concat sebe/main-leader-key " m"))
  (defconst sebe/edit-follow-key (concat sebe/main-leader-key " e"))
  (defconst sebe/find-file-follow-key (concat sebe/main-leader-key " f"))
  (defconst sebe/window-follow-key (concat sebe/main-leader-key " w"))
  (defconst sebe/projectile-follow-key (concat sebe/main-leader-key " p"))
  (defconst sebe/org-follow-key (concat sebe/main-leader-key " o"))
  (defconst sebe/helm-follow-key (concat sebe/main-leader-key " h"))
  (defconst sebe/yas-follow-key (concat sebe/main-leader-key " y"))
  (defconst sebe/copilot-follow-key (concat sebe/main-leader-key " c"))
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
  (general-create-definer sebe/yas-follow-definer
    :prefix sebe/yas-follow-key)
  (general-create-definer sebe/copilot-follow-definer
    :prefix sebe/copilot-follow-key)

  (general-define-key
   "M-x" 'helm-M-x
   "M-i" 'god-mode-all
   "C-M-i" 'helm-company
   "C-M-S-O" 'join-line
   "TAB" 'indent-for-tab-command)

  (general-define-key
   :prefix "C-x"
   "p" 'prev-window
   "O" 'other-frame
   "C-b" 'persp-ibuffer
   "b" 'helm-mini
   "C-f" 'helm-find-files
   "C-n" (kbd "C-x C-<right>")
   "C-p" (kbd "C-x C-<left>")
   "C-k" 'kill-buffer-and-window)

  (sebe/main-leader-definer
    "r" 'revert-buffer
    "C-f" 'ffap
    "d l" 'kill-whole-line
    "C-d" 'define-it-at-point)

  (sebe/avy-leader-definer
    "C-c" 'avy-goto-char-2
    "C-w" 'avy-goto-word-1
    "C-l" 'avy-goto-line
    "C-y" 'avy-copy-line
    "C-m" 'avy-move-line
    "C-k" 'avy-kill-whole-line)

  (sebe/mode-leader-definer
    "C-o" 'outline-minor-mode
    "C-v" 'view-mode)

  (sebe/math-follow-definer
    "+" 'org-increase-number-at-point
    "-" 'org-decrease-number-at-point)

  (sebe/edit-follow-definer
    "s" 'flyspell-auto-correct-word
    "S" 'ispell-buffer
    "ö" 'replicate-line
    "w" 'fixup-whitespace
    "a" 'align
    "r" 'rotate-word-at-point)

  (sebe/find-file-follow-definer
    "b" (lambda ()
          (interactive)
          (find-file "~/.bashrc"))
    "d" (lambda ()
          (interactive)
          (find-file "~/AppData/Local/dhcpsrv2.5.2/dhcpsrv.ini"))
    "e" (lambda ()
          (interactive)
          (find-file "~/.emacs")))

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
    "g" 'helm-projectile-grep
    "t" 'projectile-test-project
    "c" 'projectile-compile-project)

  (sebe/org-follow-definer
    "s" 'org-store-link
    "a" (lambda ()
          (interactive )
          (org-agenda nil "i"))
    "A" 'org-agenda
    "c" 'org-capture
    "O" 'org-clock-out
    "g" 'org-clock-goto)

  (sebe/helm-follow-definer
    "g" 'helm-grep-do-git-grep
    "o" 'helm-occur)

  (sebe/yas-follow-definer
    "n" 'yas-new-snippet
    "i" 'yas-insert-snippet
    "v" 'yas-visit-snippet-file)

  (sebe/copilot-follow-definer
    :keymaps 'copilot-mode-map
    "c" 'copilot-complete
    "p"  'copilot-panel-complete
    "n"  'copilot-next-completion))

(use-package perspective
  :ensure t
  :custom (persp-mode-prefix-key (kbd "C-\""))
  :init (persp-mode))

(use-package project
  :disabled t)

(use-package projectile
  :ensure t
  :custom
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

(use-package persp-projectile
  :after projectil
  :ensure t)


;; PATH modifictaions ==========================================================

(cond
 ((string= (system-name) "LT-JRW6NN3")
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name explicit-shell-file-name)))

;; COMPLETION SYSTEM
;; Might need to comment this out, as the setup is not done

(use-package copilot
  ;; Repo link https://github.com/zerolfx/copilot.el
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist"
                                                             "*.el"))
  :demand t
  :ensure nil
  :functions
  copilot--overlay-visible
  copilot-accept-completion
  :config
  (general-define-key "TAB" (general-predicate-dispatch 'indent-for-tab-command
                              (copilot--overlay-visible) 'copilot-accept-completion))
  :hook
  (c-ts-mode   . copilot-mode)
  (c++-ts-mode . copilot-mode)
  (bash-ts-mode . copilot-mode)
  (python-ts-mode . copilot-mode))

(use-package helm
  :ensure t
  :config
  (helm-mode 1))

(use-package compile
  :ensure nil
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-gtags
  :ensure t
  :defer t
  :functions
  helm-gtags-dwim
  helm-gtags-pop-stack
  helm-gtags-previous-history
  helm-gtags-next-history
  :bind
  (:map helm-gtags-mode-map
        ("M-."   . 'helm-gtags-dwim)
        ("M-,"   . 'helm-gtags-pop-stack)
        ("M-å"   . 'helm-gtags-select)
        ("C-c <" . 'helm-gtags-previous-history)
        ("C-c >" . 'helm-gtags-next-history))
  :hook
  (c-mode      . helm-gtags-mode)
  (c-ts-mode   . helm-gtags-mode)
  (c++-mode    . helm-gtags-mode)
  (c++-ts-mode . helm-gtags-mode)
  (asm-mode    . helm-gtags-mode)
  :custom
  (helm-gtags-auto-update t))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package helm-company
  :ensure t
  :after company)

(use-package cc-mode
  :ensure nil
  :config
  (general-define-key "TAB" (general-predicate-dispatch 'c-indent-line-or-region
                              (copilot--overlay-visible) 'copilot-accept-completion)
                      :keymaps 'c-mode-base-map)
  :hook
  (c-mode . (lambda () (c-guess)))
  (c++--mode . (lambda () (c-guess))))


(use-package cmake-ts-mode
  :mode "\\CMake\\'")

;; Indentation
(setq standard-indent 2)
(setq sh-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list '(2 4 6))

(use-package smart-tabs-mode
  :ensure t
  :hook
  (c-ts-mode . (lambda () (setq indent-tabs-mode t)))
  (c++-ts-mode . (lambda () (setq indent-tabs-mode t)))
  :config
  (smart-tabs-add-language-support c++-ts c++-ts-mode-hook
    ((c-indent-line-or-region . c-basic-offset)))
  (smart-tabs-add-language-support c-ts c-ts-mode-hook
    ((c-indent-line-or-region . c-basic-offset)))
  (smart-tabs-insinuate 'c-ts 'c++-ts))

;; Long line behaviour
(set-default 'truncate-lines t)

;; UI appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(display-time-mode 0)


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

  ;; Killing a line and yanking it according to arguments
  (kill-whole-line 0)
  (dotimes (i number-of-yanks)
    (yank)
    (newline))
  (yank))

;; Electric pairing (parenthesis, brackets etc)
(setq electric-pairs '((?\( . ?\))
                       (?\[ . ?\])
                       (?\{ . ?\})
                       (?\" . ?\")
                       (?\' . ?\')))
(electric-pair-mode t)

;; ================= MODE SPECIFICS ============================================

;; c mode ======================================================================

;; dtrt-indent =================================================================
(use-package dtrt-indent
  :disabled t
  :ensure nil
  :config (dtrt-indent-global-mode t))


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
  ;; :hook (org-capture-mode)
  :functions olivetti-set-width
  :config (olivetti-set-width 100)
  (auto-fill-mode 1))



;; Flyspell mode ===============================================================
(use-package flyspell
  :hook
  (vc-git-log-edit-mode . flyspell-mode)
  :config
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (cond
   ((string= (system-name) "LT-JRW6NN3")
    (setq ispell-program-name "~/AppData/Local/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe"))))

;; Org mode ====================================================================
(use-package org
  :ensure t
  :demand t
  :defines
  org-default-todo-file
  org-default-journal-file
  org-default-books-file
  org-default-jira-file
  :functions
  sebe/get-prop-ID-from-jira-buf
  org-property-values
  org-clock-auto-clockout-insinuate
  :bind
  (:map org-mode-map
        ("C-f" . 'forward-char)
        ("C-b" . 'backward-char))
  :hook
  (org-mode         . flyspell-mode)
  (org-mode         . auto-fill-mode)
  (org-capture-mode . olivetti-mode)
  (org-capture-mode . (lambda ()
                        (god-local-mode 0)))
  (org-mode         . org-add-electric-pairs)
  :custom
  (org-agenda-files (list org-default-journal-file
                          org-default-notes-file
                          org-default-todo-file
                          org-default-jira-file))
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-todo-keyword-faces '(("TODO" . org-todo)
                            ("IN-PROGRESS". org-in-progress)
                            ("DONE" . org-done)))
  (org-use-speed-commands t)
  (org-clock-continuously nil)
  (org-capture-templates
   '(("t" "Todo"
      entry
      (file+headline org-default-todo-file "Tasks")
      "* TODO %?\n  %i\n  %a")

     ("n" "Notes"
      entry
      (file+function org-default-notes-file org-goto)
      "* %?")

     ("c" "Clocking"
      plain
      (clock)
      "%?"
      :unnarrowed t)

     ("j" "Journal")

     ("js" "Start day"
      entry
      (file+olp+datetree org-default-journal-file)
      (function start-day-template)
      :clock-in t
      :unnarrowed t)

     ("jq" "Quit day"
      entry
      (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Quit\n%?"
      :immediate-finish t)

     ("jp" "Pause"
      entry
      (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Paused\nGoing for %?"
      :clock-in t
      :clock-keep t)

     ("jj" "Work on Jira"
      entry
      (file+function org-default-jira-file org-goto)
      "* %<%y-%m-%d %A>\n\n%?"
      :clock-in t
      :clock-keep t)

     ("jJ" "New Jira"
      entry
      (file+function org-default-jira-file org-goto)
      (function new-jira-template))

     ("jl" "General Log"
      entry
      (file+olp+datetree org-default-journal-file)
      (function general-log-template))

     ("jL" "General Log, clocked"
      entry
      (file+olp+datetree org-default-journal-file)
      (function general-log-template)
      :clock-in t
      :clock-keep t)

     ("jm" "Meeting "
      entry
      (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>][%^{Minutes}m] %?meeting\n%^{TOPIC}p%^{PROJECT}p")

     ("b" "Book"
      entry
      (file+olp+datetree org-default-books-file)
      (function book-template))))
  :init
  (setq org-directory "~/Documents/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (defcustom  org-default-todo-file (concat org-directory "/todos.org")
    "File where undated TODOs reside"
    :type '(file :must-match t)
    :group 'org-capture)
  (defcustom  org-default-journal-file (concat org-directory "/journal.org")
    "File to note down what happens during the day"
    :type '(file :must-match t)
    :group 'org-capture)
  (defcustom org-default-books-file (concat org-directory "/books.org")
    "File to note down thoughts from reading books"
    :type '(file :must-match t)
    :group 'org-capture)
  (defcustom org-default-jira-file (concat org-directory "/jira.org")
    "File to note down information about specific jira issues"
    :type '(file :must-match t)
    :group 'org-capture)

  ;; Yanky way of writing text return functions for
  ;; `org-capture-templates' to have a cleaner alist
  (defun start-day-template () nil
         "\
* [%<%H:%M>] Started %(make-string 48 ?-)

Checklist
- [ ] Set location%?
- [ ] Report time
- [ ] News
- [ ] Mail/Meetings
- [ ] Agenda
- [ ] Jira
- [-] Gerrit")

  (defun new-jira-template () nil
         "\
* IN-PROGRESS %^{Jira title}
:PROPERTIES:
:JIRA:  [[https://jira.hms.se/browse/A%^{Jira Number}][A%\\2]]
:ID: %\\2
:END:\n%?")

  (defun general-log-template () nil
         "\
* [%<%H:%M>] Log
%^{PROJECT}p
%(sebe/org-capture-template-workon-jira)%?")

  (defun book-template () nil
         "\
* [%<%H:%M>]%^{Book-title}p
Pages: %^{first page}-
Take-aways: %?")

  (defvar org-electric-pairs '((?/ . ?/) (?= . ?=) (?* . ?*) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")
  :config
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

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
        (format "Working on [[id:%1$s][%1$s]]." jira-id))))

  (defface org-in-progress
    '((((class color) (min-colors 88) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 88) (background dark))
       :foreground "medium turquoise"))
    "Face for TODO-tasks tagged with IN-PROGRESS"
    :group 'org-faces)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell  . t)))

  (org-clock-auto-clockout-insinuate))
(use-package org-jira
  :ensure t
  :after org
  :defines
  org-default-jira-files
  :init
  (defcustom org-default-jira-files (concat org-directory "/jira-api")
    "File to note down information about specific jira issues"
    :type '(file :must-match t)
    :group 'org-capture)
  :config
  ;; TODO: fix this
  (setq org-agenda-files (nconc org-agenda-files (list org-default-jira-files)))

  (setq org-jira-working-dir org-default-jira-files)
  (setq jiralib-url "https://hms-networks.atlassian.net")

  (defconst org-jira-progress-issue-flow
    '(("TODO" . "Selected for Development")
      ("Selected for Development" . "In Progress")
      ("In Progress" . "In Review")
      ("In Review" . "Done"))))


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
  (c-mode      . lsp-mode)
  (c-ts-mode   . lsp-mode)
  (c++-mode    . lsp-mode)
  (c++-ts-mode . lsp-mode)
  :defines
  lsp-diagnostics-provider
  lsp-enable-symbol-highlighting
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
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd --log=verbose")
                    ;; :initialization-options "--log=verbose"
                    :major-modes '(c-ts-mode c-mode c++-ts-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-bolt-ii))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :initialization-options ""
                    :major-modes '(python-mode python-ts-mode)
                    :remote? t
                    :server-id 'pylsp-ep)))

(use-package helm-lsp
  :disabled t
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; Python mode =================================================================
(use-package python
  :interpreter "ipython"
  :custom
  (python-shell-interpreter-args "-i")
  :init
  (defcustom python-executable-version "Python38"
    "The python version that will be used if nothing else is specified"
    :type '(string)
    :group 'python))

(use-package py-autopep8
  :after python
  :ensure t

  :hook (python-mode . py-autopep8-mode)
  :hook (python-mode . pipenv-mode))

;; Bash mode ==========================================================
(use-package sh-mode
  :mode
  ("\\.bats\\'" . bash-ts-mode)
  ("\\.sh\\'" . bash-ts-mode))

;; Matlab/octave mode ==========================================================
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; Compilation mode ============================================================
(add-hook 'compilation-mode-hook (lambda()
                                   (visual-line-mode t)))

;; YAS mode ====================================================================
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; unicode-fonts ===============================================================
(use-package unicode-fonts
  :disabled t
  :ensure nil
  :config
  (unicode-fonts-setup))

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
                    "C:/Emacs/emacs-29.1/bin"
                    "C:/Program Files/iperf-3.1.3-win64"
                    "c:/Program Files/libMultiMarkdown6.6.0/bin"
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
                    (format "C:/Users/sebe/AppData/Local/Programs/Python/%s/Scripts/" python-executable-version)
                    (format "C:/Users/sebe/AppData/Local/Programs/Python/%s/" python-executable-version)
                    "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/"
                    "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps"
                    "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/"
                    "C:/Program Files (x86)/Nmap"
                    "C:/Users/sebe/AppData/Roaming/npm"
                    "c:/Emacs/emacs-29.1/libexec/emacs/29.1/x86_64-w64-mingw32"))))

(setq enable-remote-dir-locals t)
;; Tree sitter grammars: https://github.com/emacs-tree-sitter/tree-sitter-langs
(setq major-mode-remap-alist
      '((js-json-mode . json-ts-mode)
        (python-mode  . python-ts-mode)
        (c-mode       . c-ts-mode)
        (sh-mode      . bash-ts-mode)))
(setq sentence-end-double-space nil)
(setq dired-dwim-target t)

;; ================= MODELINE MANAGEMENT =======================================

(defface sen-modeline-remote-indicator
  '((default :inherit bold)
    (t :inherit bold :background "yellow" :foreground "orange3"))
  "Face for modeline remote indicator."
  :group 'sen-modeline-faces)

(defface sen-modeline-highlight
  '((default :inherit bold)
    (t :inherit bold :background "orange3"))
  "Face for modeline highlighting."
  :group 'sen-modeline-faces)

(defface sen-modeline-modal-indicator
  '((default :inherit bold)
    (t :inherit bold :background "deep sky blue" :foreground "navy"))
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
        (sen-modeline-get-git-branch (buffer-name))))
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
    (round height (* m-width (* height m-width 0.00035)))))

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


;; ================= SPECIAL PURPOSE FIXES =====================================

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
 '(company-clang-arguments '("--log=verbose"))
 '(company-clang-executable "/usr/bin/clangd")
 '(company-idle-delay nil)
 '(connection-local-criteria-alist
   '(((:machine "bolt_vm")
      bolt_vm-vars)
     ((:application eshell :protocol "plinkx" :machine "bolt_vm")
      autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"plinkx\"\ :machine\ \"bolt_vm\"\))
     ((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((bolt_vm-vars
      (company-gtags--executable-connection . "/bin/global"))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"plinkx\"\ :machine\ \"bolt_vm\"\)
      (eshell-path-env-list "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"))
     (eshell-connection-default-profile
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
 '(copilot-idle-delay 60)
 '(custom-enabled-themes '(manoj-dark))
 '(define-it-show-google-translate nil)
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
 '(echo-keystrokes 0.5)
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
 '(even-window-sizes 'width-only)
 '(exec-path
   '("c:/Users/sebe/bin" "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI" "C:/Program Files (x86)/VMware/VMware Workstation/bin/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/" "C:/Emacs/emacs-28.1/bin" "C:/Program Files (x86)/Plantronics/Spokes3G/" "C:/Program Files/iperf-3.1.3-win64" "C:/WINDOWS/system32/config/systemprofile/scripts" "C:/WINDOWS/system32/config/systemprofile/bin" "C:/Program Files/gs/gs10.00.0/bin" "C:/Program Files/Git/cmd" "C:/Program Files/Git/usr/bin" "C:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Program Files (x86)/CMake/bin" "C:/Program Files/Git/mingw64/bin" "C:/Program Files/nodejs/" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/PowerShell/7/" "C:/Users/sebe/AppData/Local/glo668wb/bin" "C:/Program Files/doxygen/bin" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Program Files/Python/Python310/Scripts/" "C:/Program Files/Python/Python310/" "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/" "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps" "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/" "C:/Program Files (x86)/Nmap" "C:/Users/sebe/AppData/Roaming/npm" "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-mode-line '(:eval (nil)))
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(global-whitespace-mode t)
 '(google-translate-default-target-language "sv")
 '(helm-candidate-separator "--------------------------------------")
 '(helm-gtags-auto-update t)
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
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
 '(line-spacing 0.2)
 '(lsp-auto-guess-root t)
 '(lsp-clangd-binary-path "/usr/bin/clangd")
 '(lsp-clangd-version "11.0.0")
 '(lsp-clients-clangd-args '("--header-insertion-decorators=0" "--log=verbose"))
 '(lsp-clients-clangd-executable "/usr/bin/clangd")
 '(lsp-idle-delay 0.0)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-emphasis-markers t)
 '(org-outline-path-complete-in-steps nil)
 '(outline-minor-mode-prefix [3 0])
 '(package-selected-packages
   '(unicode-fonts yasnippet-snippets helm-projectile avy use-package pulsar helm-lsp lsp-mode elpy projectile-ripgrep light-mode flycheck persp-projectile general company-jedi helm-tramp py-autopep8 olivetti projectile perspective magit god-mode pipenv helm auctex))
 '(projectile-dynamic-mode-line nil)
 '(projectile-enable-cmake-presets t)
 '(projectile-git-submodule-command "nil")
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "*__pycache__"))
 '(projectile-project-search-path '("~/git/"))
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-track-known-projects-automatically nil)
 '(python-check-command "pyflakes.exe")
 '(python-skeleton-autoinsert t)
 '(safe-local-variable-values
   '((projectile-indexing-method . hybrid)
     (projectile-indexing-method quote hybrid)
     (org-goto-max-level . 2)
     (org-todo-keywords
      (sequence "TODO" "IN-PROGRESS" "DONE"))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN-PROGRESS" "|" "DONE")))))
 '(same-window-buffer-names '("*Help*" "*info*" "*compilation*" "*eww*" "*Occur*"))
 '(scroll-margin 5)
 '(show-paren-mode t)
 '(smerge-command-prefix "\33")
 '(split-height-threshold nil)
 '(split-width-threshold 140)
 '(switch-to-prev-buffer-skip-regexp '("^\\*"))
 '(truncate-lines t)
 '(whitespace-global-modes '(prog-mode))
 '(whitespace-style
   '(face trailing spaces missing-newline-at-eof empty space-before-tab::tab space-before-tab space-mark tab-mark))
 '(window-divider-default-bottom-width 15)
 '(window-divider-default-right-width 15)
 '(window-divider-mode t)
 '(window-min-width 70))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "LightSkyBlue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
 '(company-tooltip ((t (:background "black"))))
 '(compilation-info ((t (:foreground "LightPink4" :weight bold))))
 '(compilation-warning ((t (:foreground "Orange4" :weight bold))))
 '(cursor ((t (:background "lavender"))))
 '(custom-button ((t (:background "seashell" :foreground "black" :box (:line-width (2 . 2) :style pressed-button)))))
 '(diff-header ((t (:inherit diff-file-header :extend t :weight normal))))
 '(diff-refine-added ((t (:background "dark green" :inherit diff-refine-changed))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "dark red"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow4" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "dark sea green"))))
 '(font-lock-comment-face ((t (:foreground "peach puff" :slant oblique))))
 '(font-lock-constant-face ((t (:foreground "SkyBlue1" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "spring green" :slant oblique))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "light blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "cyan3"))))
 '(font-lock-string-face ((t (:foreground "lavender"))))
 '(font-lock-type-face ((t (:foreground "light steel blue" :slant italic))))
 '(fringe ((t (:inherit default :background "black"))))
 '(helm-candidate-number ((t (:extend t :background "dark slate blue" :foreground "black"))))
 '(helm-match ((t (:extend t :foreground "light cyan"))))
 '(helm-selection ((t (:extend t :distant-foreground "black" :box (:line-width (2 . 2) :color "grey75" :style released-button) :weight bold))))
 '(helm-source-header ((t (:extend t :background "#22083397778B" :foreground "white" :weight bold :family "Sans Serif"))))
 '(hi-yellow ((t (:background "orange4" :foreground "black"))))
 '(mode-line ((t (:foreground "cadet blue" :height 1.0))))
 '(mode-line-active ((t (:inherit mode-line :background "grey15" :box (:line-width (1 . 5) :color "grey15" :style flat-button)))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "grey8" :foreground "grey50" :box (:line-width (2 . 5) :color "grey8" :style flat-button) :weight light :height 1.0))))
 '(org-level-1 ((t (:extend t :foreground "deep sky blue" :weight bold))))
 '(org-level-2 ((t (:inherit org-level-1 :extend nil :foreground "dodger blue"))))
 '(org-level-3 ((t (:inherit org-level-1 :extend nil :foreground "royal blue"))))
 '(org-level-4 ((t (:inherit org-level-1 :extend nil :foreground "slate blue"))))
 '(org-mode-line-clock ((t (:foreground "medium sea green" :underline t))))
 '(persp-selected-face ((t (:inherit font-lock-constant-face :weight normal))))
 '(region ((t (:extend t :background "purple4"))))
 '(whitespace-empty ((t (:extend t :background "grey80" :foreground "firebrick"))))
 '(whitespace-hspace ((t (:inherit default :foreground "grey20"))))
 '(whitespace-indentation ((t (:background "light yellow" :foreground "firebrick"))))
 '(whitespace-space ((t (:inherit whitespace-hspace))))
 '(widget-field ((t (:background "gray15"))))
 '(window-divider ((t (:foreground "black"))))
 '(window-divider-first-pixel ((t (:inherit window-divider))))
 '(window-divider-last-pixel ((t (:inherit window-divider)))))

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
