;; -*- eval: (outline-minor-mode); eval: (outline-show-only-headings)-*-*

;; To make it easier to use the same configuration for several
;; computers it's nice to know what can be run on which computer.
;; Currently, the only limiting computer is my work computer, so that
;; gets a variable
(setq sen/work-computer-system-name "LT-JRW6NN3")

(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa". "https://melpa.org/packages/"))

(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))

;; Adding path to all my own packages
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; See if there is a work-config.el file that can be loaded
(setq sen/work-config-file-name "init-work")
(unless (load sen/work-config-file-name t)
  (message "Didn't load a work config"))


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
  w32-shell-execute
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


(use-package smerge-mode
  :ensure nil
  :defines smerge-mode
  :demand t)

(use-package vc-git
  :ensure nil
  :after vc-dir ol
  :hook
  (vc-git-log-edit-mode . auto-fill-mode)
  (vc-git-log-edit-mode . flyspell-mode)
  (vc-git-log-edit-mode . (lambda ()
                            (god-local-mode -1)))
  :defines
  vc-fileset
  vc-dir-mode-map
  :functions
  org-link-set-parameters
  vc-git--symbolic-ref
  my-vc-git-command
  vc-dir-hide-up-to-date
  vc-dir-refresh
  log-view-msg-prev
  sen/vc-git-push-gerrit
  sen/vc-git-cherry-pick
  sen/vc-git-checkout
  sen/vc-git-fetch-all
  sen/vc-git-fixup
  sen/log-view-hash-on-line
  org-git-store-link
  org-git-export
  org-git-open
  :custom
  ;; This doesn't seem to apply :/
  (vc-git-root-log-format
   '("%h..: %an %ad%d %s" "^\\(?:[*/\\| ]+ \\)?\\(?1:[0-9a-z]+\\)..: \\(?3:.*?\\)[ \11]+\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\(?2:([^)]+)\\)?"
     ((1 'log-view-message)
      (2 'change-log-list nil lax)
      (3 'change-log-name)
      (4 'change-log-date))))
  :init
  (unless (boundp 'vc-fileset)
    (setq vc-fileset nil))
  :config
  (defun my-vc-git-command (verb fn)
    (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
           (backend (car fileset-arg))
           (files (nth 1 fileset-arg)))
      (if (eq backend 'Git)
          (progn (funcall fn files)
                 (message (concat verb " " (number-to-string (length files))
                                  " file(s).")))
        (message "Not in a vc git buffer."))))

  ;; Stole these fine pieces of code from here: https://nothingissimple.ablatedsprocket.com/posts/git-links-in-org.html
  ;; The code for exporting, opening and storing are all from the same place
  (defun org-git-export (link description format _)
    "Export a Git link from Org files."
    (let* ((parts (split-string link
                                "::"))
           (path (car parts))
           (commit-hash (cadr parts))
           (translated-link))
      ;; Translate SSH link to HTML if domain is in ol-git-forges.
      (let ((repo-url (vc-git-repository-url path)))
        (message "Repo: %s" repo-url)
        (when (string-prefix-p "https://" repo-url)
          (setq translated-link (file-name-concat repo-url
                                                  (when commit-hash
                                                    (concat "commit/"
                                                            commit-hash))))))
      (pcase format
        (`html (if translated-link
                   (format "<a href=\"%s\">%s</a>"
                           (or translated-link
                               (file-name-concat (vc-git-repository-url path)
                                                 (when commit-hash
                                                   (concat "commit/" commit-hash))))
                           description)
                 (format "<b>%s</b>" description)))
        (_ (or description link)))))

  (defun org-git-open (path _)
  "Visit the Git repository at PATH.  Open the commit if provided."
  (let* ((parts (split-string path "::"))
         (repo-path (car parts))
         (commit-hash (cadr parts))
         (vc-git-log-switches nil))
    (vc-dir repo-path)
    (when commit-hash
      (vc-print-root-log 1 commit-hash))))

  (defun org-git-store-link (&optional _interactive?)
  "Store a link to a Git commit."
  (when (or (eq `vc-git-log-view-mode major-mode)
            (eq `vc-dir-mode major-mode))
    (save-excursion
      (let ((repo-uri (vc-git-root default-directory))
            (description (file-name-sans-extension
                          (file-name-nondirectory
                           (vc-git-repository-url default-directory))))
            (commit-message ""))
        (when (eq `vc-git-log-view-mode major-mode)
          (beginning-of-line)
          ;; Check if in root log mode or in regular log mode.
          ;; Return the match of the appropriate commit hash face
          (let ((match (text-property-search-forward 'face 'log-view-message t)))
            (when match
              (setq repo-uri
                    (concat repo-uri
                            "::"
                            (buffer-substring-no-properties (prop-match-beginning match)
                                                            (prop-match-end match)))))
              (let* ((eol (progn
                            (end-of-line)
                            (point))))
                     (setq commit-message (buffer-substring-no-properties (previous-property-change eol)
                                                                          eol))))
            (let ((match (if (string= (thing-at-point 'word t) "commit")
                             (text-property-search-forward 'face 'change-log-acknowledgment t)
                           (text-property-search-backward 'face 'change-log-acknowledgment t))))
              (when match
                (setq repo-uri
                      (concat repo-uri
                              "::"
                              (buffer-substring-no-properties (prop-match-beginning match)
                                                              (prop-match-end match))))
              ;; Try to find the first indented piece of text
              (search-forward-regexp (rx line-start (1+ (syntax -))
                                         (group " " (* not-newline)))
                                     nil t)
              (setq commit-message (buffer-substring-no-properties (match-beginning 1)
                                                              (match-end 1))))
            (setq description
                  (concat description ":" commit-message))))

        (org-link-store-props
         :type "git"
         :link (format "git:%s"
                       repo-uri)
         :description description)))))

  (defun sen/log-view-hash-on-line ()
    "Get the has on the current line"
    (let ((commit-hash ""))
      (save-excursion
        (goto-char (pos-bol))
        ;; first check if there is an * or | as the first character on
        ;; the line, then we can assume the line is minimized
        (unless (looking-at "^[*|]")
          ;; Else me must first go to a "commit line"
          (log-view-msg-prev))
        ;; Then we can search for the hash
        (re-search-forward "\\(?1:[0-9a-z]+\\)..:" nil t)
        (setq commit-hash (match-string 1)))
      commit-hash))

  (defun sen/vc-git-fixup ()
    "Create a fixup commit related to the one under point"
    (interactive)
    (let ((fixup-switch
           (format "--fixup=%s" (sen/log-view-hash-on-line))))
      (vc-git-command nil 0 nil  "commit" fixup-switch)))

  (defun sen/vc-git-cherry-pick ()
    "Cherry pick the commit under point"
    (interactive)
      (vc-git-command nil 0 nil  "cherry-pick" (sen/log-view-hash-on-line)))

  (defun sen/vc-git-checkout ()
    "Check out the commit under point"
    (interactive)
    (vc-retrieve-tag
     (vc-root-dir)
     (sen/log-view-hash-on-line)))

  (defun sen/vc-git-push-gerrit ()
    (interactive)
    (my-vc-git-command
     "Pushed to Gerrit"
     (lambda (files) (vc-git-command nil 0 files "push" "origin" "HEAD:refs/for/master"))))

  (defun sen/vc-git-fetch-all ()
    "Run the command `git fetch --all'"
    (interactive)
    (my-vc-git-command
     "Fetching --all"
     (lambda (files) (vc-git-command nil 0 nil "fetch" "--all"))))

  (defun my-vc-git-add (&optional revision vc-fileset comment)
    (interactive "P")
    (my-vc-git-command
     "Staged"
     'vc-git-register))

  (defun my-vc-git-reset (&optional revision vc-fileset comment)
    (interactive "P")
    (my-vc-git-command
     "Unstaged"
     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

  (org-link-set-parameters "git"
                         :follow #'org-git-open
                         :export #'org-git-export
                         :store #'org-git-store-link)
  :bind
  (:map vc-git-log-view-mode-map
        ("r" . 'vc-revert)
        ("a" . 'my-vc-git-add)
        ("u" . 'my-vc-git-reset)
        ("F" . 'sen/vc-git-fixup)
        ("f" . 'sen/vc-git-fetch-all)
        ("C" . 'sen/vc-git-checkout)
        ("P" . 'sen/vc-git-cherry-pick)))

(use-package vc-dir
  :ensure nil
  :demand t ;; This is needed as we're using vc in the modeline)
  :defines
  vc-git-log-view-mode-map
  :functions
  my-vc-git-add
  my-vc-git-reset
  :bind
   (:map vc-dir-mode-map
        ("P" . 'sen/vc-git-push-gerrit)))

(use-package whitespace
  :ensure nil
  :config
  (global-whitespace-mode 1))

(use-package god-mode
  :ensure t
  :after general
  :functions
  sebe/god-mode-pause-in-comment-region
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
        ("i" . #'god-mode-all))
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
  (defun sebe/god-mode-pause-in-comment-region ()
    "Try to find out if point is within a comment region and disable
god-mode if that is the case"
    (unless (window-minibuffer-p)
     (if (nth 4 (syntax-ppss))
         (god-local-mode-pause)
       (god-local-mode-resume))))

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
  (add-hook 'post-command-hook #'sebe/god-mode-pause-in-comment-region)
  (setq god-mode-enable-function-key-translation nil)
  (add-to-list 'god-exempt-major-modes 'diff-mode)
  (general-define-key "SPC" (general-key-dispatch 'self-insert-command
                              :timeout 0.5
                              "SPC" 'god-mode-all)))

(use-package pulsar
  :disabled t
  :ensure t
  :custom
  (pulsar-face 'pulsar-green)
  :config
  (pulsar-global-mode))

(use-package avy
  :ensure t
  :config
  (defun sen/avy-goto-line (&optional end-of-line)
    "Use the universal arugment to go to beginning or end of line"
    (interactive "P")
    (if end-of-line
        (avy-goto-end-of-line)
      (avy-goto-line))))

(use-package general
  :ensure t
  :defines
  sebe/main-leader-key
  sebe/math-leader-key
  sebe/edit-leader-key
  sebe/find-file-leader-key
  sebe/window-leader-key
  sebe/projectile-leader-key
  :functions
  sebe/main-leader-definer
  sebe/math-follow-definer
  sebe/edit-follow-definer
  sebe/find-file-follow-definer
  sebe/window-follow-definer
  sebe/projectile-follow-definer
  sebe/helm-follow-definer
  sebe/org-follow-definer
  sebe/yas-follow-definer
  sebe/mode-leader-definer
  sebe/avy-leader-definer
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

  (general-define-key
   "C-S-N" 'scroll-up-line
   "C-S-P" 'scroll-down-line
   "M-x" 'helm-M-x
   "M-i" 'god-mode-all
   "C-M-i" 'helm-company
   "C-M-S-O" 'join-line
   "C-M-z" 'global-text-scale-adjust
   "TAB" 'indent-for-tab-command)

  (general-define-key
   :prefix "M-g"
   "i" 'helm-imenu)

  (general-define-key
   :prefix "C-c"
   "c" 'org-capture)

  (general-define-key
   :prefix "C-x"
   "p" 'prev-window
   "O" 'other-frame
   "C-b" 'persp-ibuffer
   "b" 'helm-mini
   "C-c" (lambda ()
           (interactive )
           (org-agenda nil "i"))
   "C-f" 'helm-find-files
   "C-n" (kbd "C-x C-<right>")
   "C-p" (kbd "C-x C-<left>")
   "C-k" 'kill-buffer-and-window)

  (sebe/main-leader-definer
    "r" 'revert-buffer
    "g" 'helm-ls-git
    "C-f" 'ffap
    "d l" 'kill-whole-line
    "C-d" 'define-it-at-point)

  (sebe/avy-leader-definer
    "C-w" 'avy-goto-char-timer
    "C-l" 'sen/avy-goto-line
    "C-y" 'avy-copy-line
    "C-m" 'avy-move-line
    "C-k" 'avy-kill-whole-line
    "C-e" 'avy-flycheck-goto-error)

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
          (find-file "~/.emacs"))
    "j" (lambda ()
          (interactive)
          (find-file org-default-journal-file)))

  (sebe/window-follow-definer
    "s" 'toggle-window-split
    "f" 'fit-window-to-buffer)

  (sebe/projectile-follow-definer
    "f" 'helm-projectile
    "4 f" 'projectile-find-file-other-window
    "C-f" 'projectile-persp-switch-project
    "d" 'projectile-find-dir
    "4 d" 'projectile-find-dir-other-window
    "C-r" 'helm-projectile-recentf
    "r" 'projectile-dired
    "4 r" 'projectile-dired-other-window
    "g" 'helm-projectile-grep
    "t" 'projectile-test-project
    "c" 'projectile-compile-project
    "b" 'helm-browse-project)

  (sebe/org-follow-definer
    "s" 'org-store-link
    "a" (lambda ()
          (interactive )
          (org-agenda nil "i"))
    "j" 'org-jira-get-issues
    "J" 'org-jira-get-issue
    "A" 'org-agenda
    "O" 'org-clock-out
    "g" 'org-clock-goto)

  (sebe/helm-follow-definer
    "g" 'helm-grep-do-git-grep
    "o" 'helm-occur
    "b" 'helm-bookmarks)

  (sebe/yas-follow-definer
    "n" 'yas-new-snippet
    "i" 'yas-insert-snippet
    "v" 'yas-visit-snippet-file))

(use-package perspective
  :ensure t
  :custom (persp-mode-prefix-key (kbd "C-\""))
  :init (persp-mode))

(use-package outline-minor-mode
  :hook
  (python-mode . outline-minor-mode)
  (bash-ts-mode . outline-minor-mode))

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

;; COMPLETION SYSTEM
;; Might need to comment this out, as the setup is not done

(use-package copilot
  ;; Only install copilot if running on work PC
  :if (string= (system-name) sen/work-computer-system-name)
  :after general
  :defines
  sebe/copilot-follow-definer
  :ensure nil
  ;; Repo link https://github.com/zerolfx/copilot.el
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist"
                                                             "*.el"))
  :functions
  copilot--overlay-visible
  copilot-accept-completion
  sebe/copilot-follow-definer
  :config
  (defconst sebe/copilot-follow-key
    (concat sebe/main-leader-key " c"))
  (general-create-definer sebe/copilot-follow-definer
    :prefix sebe/copilot-follow-key)
  (sebe/copilot-follow-definer
     :keymaps 'copilot-mode-map
     "c" 'copilot-complete
     "p"  'copilot-panel-complete
     "n"  'copilot-next-completion
     "TAB" (general-predicate-dispatch 'indent-for-tab-command
             (copilot--overlay-visible) 'copilot-accept-completion))
  :hook
  (c-ts-mode   . copilot-mode)
  (c++-ts-mode . copilot-mode)
  (bash-ts-mode . copilot-mode)
  (python-ts-mode . copilot-mode))


(use-package helm
  :ensure t
  :functions helm-mode
  ;; It seems like these the following section only appends to the custom interface, that's why the many nil values
  :custom-face
  (helm-selection ((t (:background ,(ef-themes-with-colors bg-added)))))
  (helm-M-x-short-doc ((t (:foreground "DimGray" :box nil))))
  (helm-candidate-number ((t (:inherit ef-themes-mark-select))))
  (helm-source-header ((t (:inherit ef-themes-mark-other :extend t :weight bold :height 1.3))))
  (helm-buffer-directory ((t (:inherit ef-themes-mark-other))))
  (helm-ff-directory ((t (:extend t :foreground ,(ef-themes-with-colors green-cooler) :background nil))))
  (helm-ff-dotted-directory ((t (:foreground ,(ef-themes-with-colors green-cooler) :background ,(ef-themes-with-colors bg-alt)))))
  (helm-ff-dotted-symlink-directory ((t (:inherit helm-ff-dotted-directory :foreground "DarkOrange"))))
  (helm-ff-file ((t (:inherit ef-themes-fixed-pitch))))
  (helm-candidate-number ((t (:inherit ef-themes-heading-4 :foreground nil :background nil))))
  (helm-source-header ((t (:inherit ef-themes-search-rx-group-3 :foreground nil :background nil :family nil))))
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
  helm-gtags-select
  ;; helm-gtags-dwim
  ;; helm-gtags-pop-stack
  ;; helm-gtags-previous-history
  ;; helm-gtags-next-history
  :bind
  (:map helm-gtags-mode-map
        ;; ("M-."   . 'helm-gtags-dwim)
        ;; ("M-,"   . 'helm-gtags-pop-stack)
        ("M-å"   . 'helm-gtags-select))
        ;; ("C-c <" . 'helm-gtags-previous-history)
        ;; ("C-c >" . 'helm-gtags-next-history))
  :hook
  (c-mode          . helm-gtags-mode)
  (c-ts-mode       . helm-gtags-mode)
  (c++-mode        . helm-gtags-mode)
  (c++-ts-mode     . helm-gtags-mode)
  (asm-mode        . helm-gtags-mode)
  (bash-ts-mode    . helm-gtags-mode)
  (emacs-lisp-mode . helm-gtags-mode)
  (python-ts-mode  . helm-gtags-mode)
  :custom
  (helm-gtags-auto-update t))

(use-package helm-ls-git
  :ensure t
  :config
  (setq helm-ls-git-branches-show-all t))

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
  :init
  (setq markdown-command "multimarkdown")

  (defvar markdown--first-displayable-cache (make-hash-table :test #'equal))

  (defun markdown--first-displayable (seq)
    "Return the first displayable character or string in SEQ.
SEQ may be an atom or a sequence."
    (let ((c (gethash seq markdown--first-displayable-cache t)))
      (if (not (eq c t))
          c
        (puthash seq
                 (let ((seq (if (listp seq) seq (list seq))))
                   (cond ((stringp (car seq))
                          (cl-find-if
                           (lambda (str)
                             (and (mapcar #'char-displayable-p (string-to-list str))))
                           seq))
                         ((characterp (car seq))
                          (cl-find-if #'char-displayable-p seq))))
                 markdown--first-displayable-cache)))))

;; Olivetti mode ===============================================================
(use-package olivetti
  :disabled nil
  :ensure t
  :demand t
  ;; :hook (org-capture-mode)
  :functions olivetti-set-width
  :config (olivetti-set-width 100)
  (auto-fill-mode 1))



;; Fly* mode ===============================================================
(use-package flyspell
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  :config
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (cond
   ((string= (system-name) sen/work-computer-system-name)
    (setq ispell-program-name "~/AppData/Local/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe"))))

(use-package flycheck
  :ensure t
  :hook
  (python-ts-mode . flycheck-mode)
  (bash-ts-mode . flycheck-mode))


(use-package elec-pair
  :custom
  (electric-pair-pairs (append electric-pair-pairs
                               '((?\( . ?\))
                                 (?\[ . ?\])
                                 (?\{ . ?\})
                                 (?\" . ?\")
                                 (?\' . ?\'))))
  :config
(electric-pair-mode)
)


;; Org mode ====================================================================
(use-package org
  :ensure t
  :demand t
  :defines
  org-default-todo-file
  org-default-journal-file
  org-default-books-file
  :functions
  org-property-values
  org-clock-auto-clockout-insinuate
  :bind
  (:map org-mode-map
        ("C-f" . 'forward-char)
        ("C-b" . 'backward-char))
  :hook
  (org-mode  . org-add-electric-pairs)
  (org-mode         . flyspell-mode)
  (org-mode         . auto-fill-mode)
  (org-mode         . visual-line-mode)
  (org-capture-mode . olivetti-mode)
  (org-capture-mode . (lambda ()
                        (god-local-mode 0)))

  :custom
  (org-directory "~/Documents/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files (list org-default-journal-file
                          org-default-notes-file
                          org-default-todo-file))
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (org-todo-keyword-faces '(("TODO" . org-todo)
                            ("SELECTED-FOR-DEVELOPMENT" . org-todo)
                            ("IN-PROGRESS". org-in-progress)
                            ("IN-REVIEW" . org-in-review)
                            ("BLOCKED" . org-block)
                            ("DONE" . org-done)
                            ("PENDING-RELEASE" . org-done)))
  (org-use-speed-commands t)
  (org-clock-continuously nil)
  :init
  (defface org-in-progress
    '((((class color) (min-colors 88) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 88) (background dark))
       :foreground "medium turquoise"))
    "Face for TODO-tasks tagged with IN-PROGRESS"
    :group 'org-faces)

  (defcustom  org-default-todo-file (concat org-directory "/todos.org")
    "File where undated TODOs reside"
    :type '(file :must-match t)
    :group 'org-capture)
  (defcustom  org-default-journal-file (concat org-directory "/journal.org")
    "File to note down what happens during the day"
    :type '(file :must-match t)
    :group 'org-capture)

  (defvar org-electric-pairs '((?/ . ?/) (?= . ?=) (?* . ?*) (?~ . ?~) (?+ . ?+))
    "Electric pairs for org-mode.")
  :config
  ;; To eager load org-mode
  (with-temp-buffer (org-mode))

  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs
                                                 org-electric-pairs)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell  . t)))

  (org-clock-auto-clockout-insinuate)

  ;;; To save the clock history across Emacs sessions, use
  (if (file-exists-p org-clock-persist-file)
    ;; (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
    (shell-command (concat "touch " org-clock-persist-file))))


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
  :custom (TeX-master . nil)
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '("../references.bib"))
  (setq TeX-arg-input-file-search nil))

;; DocView Mode ================================================================
(use-package doc-view
  :hook
  (doc-view-mode . auto-revert-mode)
  (doc-view-mode . blink-cursor-mode))

;; LSP mode ====================================================================
(use-package lsp-mode
  :hook
  (python-mode
   c-mode c-ts-mode
   c++-mode c++-ts-mode)
  :functions
  lsp-tramp-connection
  make-lsp-client
  lsp-register-client
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
  :hook (python-ts-mode . py-autopep8-mode))

;; Bash mode ==========================================================
(use-package pyvenv
  :after python
  :ensure t
)

(use-package sh-mode
  :hook (bash-ts-mode . flymake-mode)
  :mode
  ("\\.bats\\'" . bash-ts-mode)
  ("\\.sh\\'" . bash-ts-mode))

;; Matlab/octave mode ==========================================================
(use-package combobulate
  ;; Checking out doesn't work for some reason, there seems to something that
  ;; just doesn't work. I managed to fix it manually though
  ;; ---
  ;; Repo link https://github.com/mickeynp/combobulate
  ;; :straight (:host github :repo "mickeynp/combobulate" :files ("dist"
  ;;                                                              "*.el"))
  :ensure nil
  :load-path "lisp/combobulate"
  :hook
  ;; (c-ts-mode   . combobulate)
  ;; (c++-ts-mode . combobulate)
  ;; (bash-ts-mode . combobulate)
  (python-ts-mode . combobulate-mode)
  )

;; YAS mode ====================================================================
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; ================= GENERAL PURPOSE FUNCTIONS =================================
(use-package which-key
  :custom
  (which-key-idle-delay 2.5)
  :config
  (which-key-mode 1))

(use-package rainbow)

(use-package ef-themes
  ;; :disabled t
  :ensure t
  :demand t
  :custom
  (ef-themes-disable-other-themes t)
  (ef-themes-to-toggle '(ef-bio ef-elea-light))
  :config
  (ef-themes-load-theme 'ef-bio))

;; Check if on the work laptop
(cond
 ((string= (system-name) sen/work-computer-system-name)
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name explicit-shell-file-name)))

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
(setq enable-remote-dir-locals t)
;; Tree sitter grammars: https://github.com/emacs-tree-sitter/tree-sitter-langs
(setq major-mode-remap-alist
      '((js-json-mode . json-ts-mode)
        (python-mode  . python-ts-mode)
        (c-mode       . c-ts-mode)
        (sh-mode      . bash-ts-mode)
        (css-mode     . css-ts-mode)
        (js-mode      . js-ts-mode)))
(setq sentence-end-double-space nil)
(setq dired-dwim-target t)
(setq backup-directory-alist `(("." . "~/.saves")))

;; ================= MODELINE MANAGEMENT =======================================

;; Indentation
(setq standard-indent 2)
(setq sh-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list '(2 4 6))
;; Long line behaviour
(set-default 'truncate-lines t)

;; UI appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(display-time-mode 0)

;; Only show line numbers for derivatives of prog-mode
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

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)))
;; Add margins for text mode
(add-hook 'text-mode-hook (lambda ()
                            (setq left-margin-width 3)))

;; Display time on mode line
(add-hook 'compilation-mode-hook (lambda()
                                   (visual-line-mode t)))
(add-hook 'window-size-change-functions 'setup-display-buffer-base-action)



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
  "Face for narrow mode indicator."
  :group 'sen-modeline-faces)

(defface sen-modeline-warning
  '((t :background "red" :foreground "lemon chiffon"))
  "Face for warning indication."
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
  (propertize
   (capitalize (string-replace "-mode" "" (symbol-name major-mode)))
   'face 'italic))

(defvar-local sen-modeline-major-mode
    '(:eval (sen-modeline-major-mode-name))
  "Variable containing the representation of the current major mode
as a string")

(defun sen-modeline-get-git-branch (file)
  "If applicable, return the git branch of the currently visited file"
  (let ((git-ref (vc-git--symbolic-ref file)))
    (if git-ref
        (concat
         (if smerge-mode
             "!")
         (if (not (vc-up-to-date-p file))
             (propertize git-ref 'face 'vc-dir-status-edited)
           git-ref)
         ":"))))

(defvar-local sen-modeline-git-branch
    '(:eval
      (when (mode-line-window-selected-p)
        (sen-modeline-get-git-branch (buffer-name))))
  "Variable containing the branch of the currently visited buffer")

(defun sen-modeline-get-perspective-text ()
  "Return the name of the perspective, colorize if it differs from
the project root

This will be helpful when browsing files with similar names that
exist in different repositories"
  ;; Compare `projectile-project-root' to `persp-current-name'
  ;; If they aren't the same then color the text red)
  )

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

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "#d0d0d0"])
 '(auto-save-default nil)
 '(avy-background t)
 '(avy-flyspell-correct-function 'flyspell-auto-correct-word)
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
 '(custom-safe-themes
   '("ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "ca13c52ee7cb7392aefe5b9d4c9a61aa0bb54ed4845d54d4f0b4b2c98a3614df"
     "114a8f7143a01e8ba083700f4dfaab333de3e2866c968b3849a1c5fef00e3c08"
     "05747b91f589bdb7ce9f93849a96f6cf8ee26fe479c5f1964d91c4a5a81d36c2"
     default))
 '(define-it-show-google-translate nil)
 '(delete-selection-mode t)
 '(desktop-save-mode nil)
 '(dired-listing-switches "-alh")
 '(display-buffer-alist
   '(("\\\\*Help\\\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (nil))
     ("\\\\*Compilation.*" display-buffer-same-window
      (nil))
     ("\\\\*Find.*" display-buffer-same-window
      (nil))
     ("\\\\*vc-dir\\\\*" display-buffer-in-side-window
      (side . left)
      (window-width . 70))
     ("\\\\*vc-change-log\\\\*" display-buffer-in-side-window
      (side . bottom)
      (window-height . 25))
     ("CAPTURE-" display-buffer-pop-up-window
      (nil))))
 '(display-buffer-base-action '(display-buffer-in-side-window (side . right)))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(echo-keystrokes 0.5)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(enable-recursive-minibuffers t)
 '(even-window-sizes 'width-only)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-mode-line '(:eval (nil)))
 '(gnus-article-truncate-lines nil)
 '(google-translate-default-target-language "sv")
 '(helm-M-x-always-save-history t)
 '(helm-M-x-show-short-doc t)
 '(helm-buffer-max-length 30)
 '(helm-candidate-separator "--------------------------------------")
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source nil)
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
 '(kill-buffer-delete-auto-save-files t)
 '(line-spacing 0.2)
 '(lsp-auto-guess-root t)
 '(lsp-clangd-binary-path "clangd")
 '(lsp-clangd-version "11.0.0")
 '(lsp-clients-clangd-args '("--header-insertion-decorators=0" "--log=verbose"))
 '(lsp-clients-clangd-executable "clangd")
 '(lsp-idle-delay 0.0)
 '(lsp-pylsp-plugins-mccabe-enabled nil)
 '(lsp-pylsp-plugins-pycodestyle-enabled t)
 '(lsp-pylsp-plugins-pydocstyle-enabled nil)
 '(minibuffer-electric-default-mode t)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-emphasis-markers t)
 '(org-jira-custom-jqls
   '((:jql "project in (AWIRT, A7750, A7739, GPDEV, 7750-R2D2-WEB, A7757) ORDER BY updated DESC, created DESC" :limit 100 :filename "the-jira-digest")))
 '(org-jira-default-jql
   "assignee = currentUser() and resolution = unresolved ORDER BY\12  priority DESC, created ASC")
 '(org-jira-use-status-as-todo t)
 '(org-outline-path-complete-in-steps nil)
 '(outline-minor-mode-cycle t)
 '(outline-minor-mode-prefix [3 0])
 '(package-selected-packages
   '(unicode-fonts yasnippet-snippets helm-projectile avy use-package pulsar helm-lsp lsp-mode projectile-ripgrep light-mode flycheck persp-projectile general company-jedi helm-tramp py-autopep8 olivetti projectile perspective magit god-mode pipenv helm auctex))
 '(persp-show-modestring nil)
 '(persp-state-default-file "~/.emacs.d/.perspective")
 '(projectile-dynamic-mode-line nil)
 '(projectile-enable-cmake-presets t)
 '(projectile-git-submodule-command "nil")
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "*__pycache__"))
 '(projectile-project-search-path '("~/git/"))
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-track-known-projects-automatically nil)
 '(python-check-command "pyflakes.exe")
 '(python-fill-docstring-style 'symmetric)
 '(python-skeleton-autoinsert t)
 '(recenter-positions '(top middle bottom))
 '(request-backend 'url-retrieve)
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "./wireless-build.sh")
     (projectile-project-compilation-cmd . "python build.py")
     (auto-fill-mode)
     (org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("IN-PROGRESS" . org-in-progress)
        ("IN-REVIEW" . org-scheduled)
        ("BLOCKED" . org-block)
        ("DONE" . org-done)))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "BLOCKED" "|" "DONE")))
     (org-todo-keywords
      (sequence "TODO" "IN-PROGRESS" "IN-REVIEW")
      (sequence "BLOCKED" "DONE"))
     (org-todo-keywords
      (sequence "TODO" "IN-PROGRESS" "IN-REVIEW")
      ("BLOCKED" "DONE"))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "BLOCKED" "DONE")))
     (org-goto-max-level . 3)
     (org-jira-mode . t)
     (projectile-indexing-method . hybrid)
     (projectile-indexing-method quote hybrid)
     (org-goto-max-level . 2)
     (org-todo-keywords
      (sequence "TODO" "IN-PROGRESS" "DONE"))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN-PROGRESS" "|" "DONE")))))
 '(same-window-buffer-names '("*Help*" "*info*" "*compilation*" "*eww*" "*Occur*"))
 '(shell-command-prompt-show-cwd t)
 '(show-paren-mode t)
 '(smerge-command-prefix "\33")
 '(split-height-threshold nil)
 '(split-width-threshold 140)
 '(switch-to-prev-buffer-skip-regexp '("^\\*"))
 '(truncate-lines t)
 '(vc-git-log-switches "--all")
 '(whitespace-global-modes '(prog-mode))
 '(whitespace-style
   '(face trailing spaces missing-newline-at-eof empty space-before-tab::tab space-before-tab space-mark tab-mark))
 '(window-divider-default-bottom-width 15)
 '(window-divider-default-right-width 15)
 '(window-divider-mode t)
 '(window-min-width 70))
