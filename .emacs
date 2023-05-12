
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Needed to byte-compile
(require 'use-package)

(use-package appearance
  :ensure nil
  :bind
  ("C-M-<f11>" . 'appearance-toggle-mode))

(use-package gerrit-getter)

(use-package smart-mode-line
  :ensure nil)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode)
  )

(setq load-prefer-newer t)

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

  ;; (defun sebe/god-mode-update-cursor ()
  ;;   (setq cursor-type (if (or (god-local-mode buffer-read-only) 'box 'bar))))

  (defun sebe/god-mode-update-mode-line ()
    "Toggle between differnet colors of both the cursor and
the modeline when toggling god-mode"
    (cond
     (god-local-mode
      (set-face-attribute 'mode-line nil
                          :foreground "cadet blue"
                          :background "black")
      (setq cursor-type 'box))
     (t
      (set-face-attribute 'mode-line nil
                          :foreground "aquamarine"
                          :background "dark slate gray")
      (setq cursor-type 'bar))
     )
    )
  (add-hook 'post-command-hook 'sebe/god-mode-update-mode-line)
  (add-hook 'overwrite-mode-hook 'sebe/god-mode-toggle-on-overwrite)

  )

(use-package pulsar
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
  sebe/window-leader-key
  sebe/projectile-leader-key
  :config
  (defconst sebe/main-leader-key "C-.")
  (defconst sebe/math-leader-key (concat sebe/main-leader-key " m"))
  (defconst sebe/edit-leader-key (concat sebe/main-leader-key " e"))
  (defconst sebe/window-leader-key (concat sebe/main-leader-key " w"))
  (defconst sebe/projectile-leader-key (concat sebe/main-leader-key " p"))
  (defconst sebe/org-leader-key (concat sebe/main-leader-key " o"))
  (defconst sebe/helm-leader-key (concat sebe/main-leader-key " h"))
  (general-create-definer sebe/main-leader-definer
    :prefix sebe/main-leader-key)
  (general-create-definer sebe/math-leader-definer
    :prefix sebe/math-leader-key)
  (general-create-definer sebe/edit-leader-definer
    :prefix sebe/edit-leader-key)
  (general-create-definer sebe/window-leader-definer
    :prefix sebe/window-leader-key)
  (general-create-definer sebe/projectile-leader-definer
    :prefix sebe/projectile-leader-key)
  (general-create-definer sebe/org-leader-definer
    :prefix sebe/org-leader-key)
  (general-create-definer sebe/helm-leader-definer
    :prefix sebe/helm-leader-key)

  (general-define-key
   "C-�" 'replicate-line
   "M-x" 'helm-M-x)

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
    "f" 'fixup-whitespace
    "d l" 'kill-whole-line)

  (sebe/math-leader-definer
    "+" 'org-increase-number-at-point
    "-" 'org-decrease-number-at-point)
  (sebe/edit-leader-definer
    "e" (lambda ()
          (interactive)
          (find-file "~/.emacs"))
    "s" 'flyspell-auto-correct-word)
  (sebe/window-leader-definer
    "s" 'toggle-window-split
    "f" 'fit-window-to-buffer)

  (sebe/projectile-leader-definer
    "f" 'projectile-find-file
    "C-f" 'projectile-persp-switch-project
    "d" 'projectile-find-dir)

  (sebe/org-leader-definer
    "s" 'org-store-link
    "a" 'org-agenda)

  (sebe/helm-leader-definer
    "g" 'helm-grep-do-git-grep
    "o" 'helm-occur)
  )

(use-package project
  :disabled t)

(use-package perspective
  :ensure t
  :custom (persp-mode-prefix-key (kbd "C-@"))
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
  :functions
  sebe/projectile-find-or-switch
  :bind
  ("<f4>" . 'sebe/projectile-find-or-switch)
  :init (projectile-mode)
  (setq projectile-enable-caching nil)
  :functions
  projectile-persp-switch-project
  :config
  (defun sebe/projectile-find-or-switch (switch)
    "Find or switch depending on universal argument"
    (interactive "P")
    (if switch
        (projectile-persp-switch-project)
      (projectile-find-file)))
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

(use-package helm-gtags
  :defer t
  :functions
  helm-gtags-dwim
  helm-gtags-pop-stack
  helm-gtags-previous-history
  helm-gtags-next-history
  :bind
  ("M-." . 'helm-gtags-dwim)
  ("M-," . 'helm-gtags-pop-stack)
  ("M-�" . 'helm-gtags-select)
  ("C-c <" . 'helm-gtags-previous-history)
  ("C-c >" . 'helm-gtags-next-history)
  :hook
  (c-mode . helm-gtags-mode)
  (c++-mode . helm-gtags-mode)
  (asm-mode . helm-gtags-mode)
  )

(use-package company
  ;; :hook
  ;; (c-mode-hook . company-mode)
  ;; (c++-mode-hook . company-mode)
  :bind ("C-M-i" . 'helm-company)
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-mode-map  (kbd "C-M-i") 'company-complete)
  )

(use-package cc-mode
    :config
    (define-key c-mode-map  [(tab)] 'c-indent-line-or-region)
    (define-key c++-mode-map  [(tab)] 'c-indent-line-or-region)
  )

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))

;; On save
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; Long line behaviour
(set-default 'truncate-lines t)

;; UI appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)

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
  "Kill a whole line from anywhere in it then yank it 'number of yank' times"
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
  :config (olivetti-set-width 80)
  (auto-fill-mode 1)
  )

;; Org mode ====================================================================

(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (cond
   ((string= (system-name) "LT-JRW6NN3")
    (setq ispell-program-name "~/AppData/Local/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")))
  )

(use-package org
  :ensure t
  :bind
  ("M-�" . 'org-capture)
  :hook
  (org-mode . flyspell-mode)
  (org-capture-mode . olivetti-mode)
  (org-capture-mode . auto-fill-mode)
  (org-capture-mode . (lambda ()
                        (god-local-mode 0)))
  :init

  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
      "* TODO %?\n  %i\n  %a")

     ("n" "Notes" entry (file+function org-default-notes-file org-goto)
      "* %?")

     ("j" "Journal")

     ("js" "Start day" entry (file+olp+datetree
                              org-default-journal-file)
       "* [%<%H:%M>] Started\n\nChecklist\n\
- [ ] Report time%?\n\
- [ ] News\n\
- [ ] Mail\n\
- [ ] Jira\n\
- [ ] Gerrit"
       :clock-in t
       :clock-keep t)

     ("jq" "Quit day" entry (file+olp+datetree org-default-journal-file)
      (function (lambda ()
        (org-clock-out)
         "* [%<%H:%M>] Quit\n%?"))
      )
     ("jp" "Pause" entry (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Paused\n%?")

     ("jr" "Resume" entry (file+olp+datetree org-default-journal-file)
      "* [%<%H:%M>] Resumed\n"
      :immediate-finish t)

     ("jj" "Jira Journal" entry (file+olp+datetree
                                org-default-journal-file)
      "* [%<%H:%M>] Log\n%^{PROJECT}p%^{JIRA}p%?")

     ("jl" "General Log" entry (file+olp+datetree
                                org-default-journal-file)
      "* [%<%H:%M>] Log\n%^{PROJECT}p%?")

     ("jm" "Meeting Journal "entry (file+olp+datetree
                                   org-default-journal-file)
      "* [%<%H:%M>] Meeting [%^{Minutes}m]\n%^{TOPIC}p%^{PROJECT}p%?")

     ("b" "Book" entry (file+olp+datetree
                        org-default-books-file)
      "* [%<%H:%M>]
Book: %^{Book-title}p
Pages: %^{first page}-%^{last page}
Take-aways: %?")
     )
   )
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     )
   )
  (setq org-directory "~/Documents/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-todo-file (concat org-directory "/todos.org"))
  (setq org-default-journal-file (concat org-directory "/journal.org"))
  (setq org-default-books-file (concat org-directory "/books.org"))
  (setq org-agenda-files (list org-directory))
  (setq org-use-speed-commands t)
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
  :hook (doc-view-mode . (lambda () (auto-revert-mode))))


;; LSP mode ====================================================================

(use-package lsp-mode
  :hook
  (python-mode . lsp)
  :commands
  lsp
  lsp-find-definition
  lsp-find-references
  :bind
  ("M-." . 'lsp-find-definition)
  ("M-," . 'lsp-find-references)
  )

;; Python mode =================================================================
(use-package python
  :interpreter "ipython"
  :custom
  (python-shell-interpreter-args "-i")
  ;; :hook
  ;; (python-mode . elpy-enable)
  :config )

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
  ("M-." . 'elpy-goto-definition)
  ("M-," . 'elpy-goto-assignment)
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq 'elpy-rpc-python-command "c:/Users/sebbe/AppData/Local/Programs/Python/Python39/python.exe"))

(use-package py-autopep8
  :ensure t
  :defer t
  :hook (elpy-mode . py-autopep8-mode))

(use-package pipenv
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

;; ================= GLOBAL VARIABLES ==========================================

(cond
 ((string= (system-name) "LT-JRW6NN3")
  (setq exec-path '("c:/Users/sebe/bin" "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI" "C:/Program Files (x86)/VMware/VMware Workstation/bin/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/" "C:/Emacs/emacs-28.1/bin" "C:/Program Files (x86)/Plantronics/Spokes3G/" "C:/Program Files/iperf-3.1.3-win64" "C:/WINDOWS/system32/config/systemprofile/scripts" "C:/WINDOWS/system32/config/systemprofile/bin" "C:/Program Files/gs/gs10.00.0/bin" "C:/Program Files/Git/cmd" "C:/Program Files/Git/usr/bin" "C:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Program Files (x86)/CMake/bin" "C:/Program Files/Git/mingw64/bin" "C:/Program Files/nodejs/" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/PowerShell/7/" "C:/Users/sebe/AppData/Local/glo668wb/bin" "C:/Program Files/doxygen/bin" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/" "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps" "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/" "C:/Program Files (x86)/Nmap" "C:/Users/sebe/AppData/Roaming/npm" "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))))


;; ================= SPECIAL PURPOSE FIXES =====================================

;; ;; Workaround to make the keyboard work again?! Avoiding dead keys
;; (define-key key-translation-map [dead-grave] "`")
;; (define-key key-translation-map [dead-acute] "'")
;; (define-key key-translation-map [dead-circumflex] "^")
;; (define-key key-translation-map [dead-diaeresis] "\"")
;; (define-key key-translation-map [dead-tilde] "~")

;; ================= NO TOUCH - EMACS CONFIG ===================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "#d0d0d0"])
 '(auto-save-default nil)
 '(company-idle-delay nil)
 '(custom-enabled-themes '(manoj-dark))
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
   '("c:/Users/sebe/bin" "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI" "C:/Program Files (x86)/VMware/VMware Workstation/bin/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/" "C:/Emacs/emacs-28.1/bin" "C:/Program Files (x86)/Plantronics/Spokes3G/" "C:/Program Files/iperf-3.1.3-win64" "C:/WINDOWS/system32/config/systemprofile/scripts" "C:/WINDOWS/system32/config/systemprofile/bin" "C:/Program Files/gs/gs10.00.0/bin" "C:/Program Files/Git/cmd" "C:/Program Files/Git/usr/bin" "C:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Program Files (x86)/CMake/bin" "C:/Program Files/Git/mingw64/bin" "C:/Program Files/nodejs/" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/PowerShell/7/" "C:/Users/sebe/AppData/Local/glo668wb/bin" "C:/Program Files/doxygen/bin" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/" "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps" "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/" "C:/Program Files (x86)/Nmap" "C:/Users/sebe/AppData/Roaming/npm" "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))
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
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps nil)
 '(package-selected-packages
   '(pulsar mediawiki helm-lsp lsp-mode elpy projectile-ripgrep light-mode flycheck persp-projectile general company-jedi helm-tramp py-autopep8 olivetti projectile perspective magit god-mode pipenv helm auctex))
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "*__pycache__"))
 '(projectile-project-search-path '("~/git/"))
 '(python-check-command "pyflakes.exe")
 '(python-skeleton-autoinsert t)
 '(same-window-regexps nil)
 '(show-paren-mode t)
 '(smerge-command-prefix "")
 '(split-height-threshold nil)
 '(split-width-threshold 160))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "LightSkyBlue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
 '(compilation-info ((t (:foreground "LightPink4" :weight bold))))
 '(compilation-warning ((t (:foreground "Orange4" :weight bold))))
 '(cursor ((t (:background "lavender"))))
 '(custom-button ((t (:background "seashell" :foreground "black" :box (:line-width (2 . 2) :style pressed-button)))))
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
 '(mode-line ((t (:inherit mode-line-buffer-id :background "sky blue" :foreground "Blue" :slant normal :height 0.95))))
 '(mode-line-buffer-id ((t (:inherit mode-line :slant italic :weight bold :height 1.0))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "black" :foreground "light blue" :box nil :weight light :height 0.9))))
 '(region ((t (:extend t :background "steel blue"))))
 '(widget-field ((t (:background "gray15"))))
 '(window-divider ((t (:foreground "black")))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
