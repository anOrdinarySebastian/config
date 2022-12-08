
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

;; Needed to byte-compile
(require 'use-package)

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
  :init
  (setq god-mode-enable-function-key-translation nil)
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
			                    :foreground "lavender"
			                    :background "gray10")
      (setq cursor-type 'box))
     (t
      (set-face-attribute 'mode-line nil
                          :foreground "dark slate gray"
                          :background "snow")
      (setq cursor-type 'bar))
     )
    )
  (add-hook 'post-command-hook 'sebe/god-mode-update-mode-line)
  )

(use-package perspective
  :ensure t
  :bind ("C-x C-b" . persp-list-buffers)
  :custom (persp-mode-prefix-key (kbd "C-@"))
  :init (persp-mode))

(use-package projectile
  :ensure t
  :bind
  ("<f4>" . 'projectile-find-file)
  ("C-<f4>" . 'projectile-find-file)
  :init (projectile-mode)
  (setq projectile-enable-caching nil))


;; PATH modifictaions ==========================================================
;; Pointing to the right find.exe

(setq find-program "\"c:\\Program Files\\Git\\usr\\bin\\find.exe\"")
;; (setq explicit-shell-file-name "bash")
;; (setq shell-file-name explicit-shell-file-name)
;; (add-to-list 'exec-path "BASH")

;; COMPLETION SYSTEM
;; Might need to comment this out, as the setup is not done

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  :bind
  ;; ("M-x" . 'helm-M-x)
  )

(use-package helm-gtags
  :defer t
  :bind
  ("M-." . 'helm-gtags-dwim)
  ("M-," . 'helm-gtags-pop-stack)
  ("C-c <" . 'helm-gtags-previous-history)
  ("C-c >" . 'helm-gtags-next-history)
  ;; :bind ("C-å" . 'helm-gtags-find-tag)
  :hook
  (c-mode-hook . helm-gtags-mode)
  (c++-mode-hook . helm-gtags-mode)
  (asm-mode-hook . helm-gtags-mode)
  )

(use-package company
    :hook
    (c-mode-hook . company-mode)
    (c++-mode-hook . company-mode)
    :config
    (setq company-backends (delete 'company-semantic company-backends))
    )

(use-package cc-mode
    :config
  (define-key c-mode-map  (kbd "C-M-i") 'company-complete)
  (define-key c-mode-map  [(tab)] 'c-indent-line-or-region)
  (define-key c++-mode-map  (kbd "C-M-i") 'company-complete)
  (define-key c++-mode-map  [(tab)] 'c-indent-line-or-region)
  )

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))

;; On save
(add-hook 'write-file-functions 'delete-trailing-whitespace)


;; could be bad, will not let you save at all, until you correct the error
 ;; (add-hook 'emacs-lisp-mode-hook
 ;;  (lambda ()
 ;;   (add-hook 'write-file-functions
 ;;    'check-parens)))

;; Long line behaviour
(set-default 'truncate-lines t)

;; UI appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 0)

;; On startup
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

;; Nice to have with horizontal screen
;; (add-hook 'emacs-startup-hook 'toggle-window-split)

(defun prev-window ()
	(interactive nil)
	(other-window -1))

;; Window management ===========================================================

;;(display-buffer-base-action ) ;; Check help for this function
;; display-buffer-alist <- customize this to set buffers that should use the same window
(setq window-min-height 10)
(setq window-min-width 80)

;; version control helper

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
;; global keymaps
;; Window management
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-x C-n") (kbd "C-x C-<right>"))
(global-set-key (kbd "C-x C-p") (kbd "C-x C-<left>"))
(global-set-key (kbd "s-m") 'toggle-frame-maximized)
(global-set-key (kbd "C-x C-b") 'persp-ibuffer)

;; Editing
(global-set-key (kbd "C-ö") 'replicate-line)

;; Buffers
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

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
  (yank)
  )

;; line numbers
(global-display-line-numbers-mode t)

(defcustom display-line-numbers-exempt-modes
  '(eshell-mode
    shell-mode
    ansi-term-mode
    tex-mode latex-mode
    org-mode
    help-mode)
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
  :ensure t
  :config (dtrt-indent-global-mode t)
  )


;; Markdown mode ===============================================================
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Olivetti mode ===============================================================
(use-package olivetti
  :ensure t
  :demand t
;;  :hook (org-capture-mode)
  :functions (olivetti-set-width)
  :config (olivetti-set-width 90)
  (auto-fill-mode 1)
  )

;; Org mode ====================================================================

(use-package org
  :ensure t
  :bind
  ("M-§" . 'org-capture)
  :hook
  (org-capture-mode . olivetti-mode)
  (org-capture-mode . auto-fill-mode)
  (org-capture-mode . (lambda ()
                        (god-local-mode 0)))
  :init
  (defun sebe/org-capture-jira-completion ()
    "Function returning a string based on the current jira issues described in the
`sebe/current-jira-issues' list. Hopefully, this can be extended with the option
of adding new issues too"
    (interactive)
    (completing-read "Jira issue: " 'sebe/current-jira-issues ) ;; Look at the info node 21.6.2
    )
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Notes" entry (file+function org-default-notes-file sebe/org-capture--notes)
      "* %?")
     ("j" "Journal" entry (file+olp+datetree
                           org-default-journal-file)
      "* [%<%H:%M>] %?")
     ("J" "Jira Journal" entry (file+olp+datetree
                           org-default-journal-file)
      "* [%<%H:%M>] %(sebe/org-capture-jira-completion)\n%?")
     ("b" "Book" entry (file+olp+datetree
                        org-default-books-file)
      "* [%<%H:%m>]
Book: %^{Book title}
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

  (defun sebe/org-capture--notes ()
    "Function to be used as a capture template for notes

     If the heading exists then the point will be placed there, if not then
     the argument will be used to create a new heading"
    (interactive)
    ;; This call returns a list of all the level two headings in the file
    ;; The problem with following call in an org file is that after the plain text, there are some font definitions and stuff
    ;;    which messes up the string -> not a valid input argument
    ;;(call-interactively sebe/re-seq (rx (= 2 "*") (* blank) (group (* (syntax w)))) (buffer-substring (point-min) (point-max)))
    (let ((head (read-string "Desiered heading: ")))
      (unless (re-search-forward
               (rx (= 2 "*") (* blank)
                   (group (literal head))) nil t)
        (goto-char (point-max))
        (insert (format "** %s" head))))
    )
  )

;; Tex mode ====================================================================

(defun tex-init-addons ()
  "Unnecessary function to start latex in visual line mode"
  ()
  (visual-line-mode t)
  (turn-on-reftex)
  (highlight-regexp "\\\\todo\\([swq]\\|.+?}\\)" 'hi-yellow)
  ;;(add-to-list 'electric-pairs '(?$ . ?$))
  )

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

;; Python mode =================================================================
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")


(use-package elpy
  :ensure t
  :defer t
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (advice-add 'python-mode :before 'elpy-enable))

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

;; Maybe this should just part of the org startup, but it doens't quite fit in
;; Createing the list
(setq sebe/current-jira-issues (list))
;; Adding entry to the list
(setq sebe/current-jira-issues (cons "A7714-1109"
                                     sebe/current-jira-issues))
(add-to-list 'sebe/current-jira-issues "A7750-402")

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
 '(custom-enabled-themes '(manoj-dark))
 '(display-buffer-alist
   '(("\\*Help\\*"
      (display-buffer-same-window))
     ("\\*Compilation.*" display-buffer-same-window)
     ("\\*Find.*" display-buffer-same-window)
     ("\\*Customize" display-buffer-same-window)
     ("\\*vc-.*" display-buffer-same-window)))
 '(display-buffer-base-action '((display-buffer-below-selected)))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(eldoc-echo-area-prefer-doc-buffer nil)
 '(elpy-formatter 'autopep8)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-autodoc elpy-module-sane-defaults))
 '(elpy-project-root-finder-functions
   '(elpy-project-find-projectile-root elpy-project-find-git-root))
 '(elpy-rpc-python-command
   "c:/Users/sebbe/AppData/Local/Programs/Python/Python39/python.exe")
 '(elpy-syntax-check-command "pycodestyle")
 '(exec-path
   '("c:/Users/sebe/bin" "c:/Program Files/ImageMagick-7.1.0-Q16-HDRI" "C:/Program Files (x86)/VMware/VMware Workstation/bin/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/libs/" "C:/Emacs/emacs-28.1/bin" "C:/Program Files (x86)/Plantronics/Spokes3G/" "C:/Program Files/iperf-3.1.3-win64" "C:/WINDOWS/system32/config/systemprofile/scripts" "C:/WINDOWS/system32/config/systemprofile/bin" "C:/Program Files/gs/gs10.00.0/bin" "C:/Program Files/Git/cmd" "C:/Program Files/Git/usr/bin" "C:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Program Files (x86)/CMake/bin" "C:/Program Files/Git/mingw64/bin" "C:/Program Files/nodejs/" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/PowerShell/7/" "C:/Users/sebe/AppData/Local/glo668wb/bin" "C:/Program Files/doxygen/bin" "C:/Program Files (x86)/GNU Tools ARM Embedded/4.9 2015q2/bin" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/Scripts/" "C:/Users/sebe/AppData/Local/Programs/Python/Python38/"  "C:/Users/sebe/AppData/Local/Programs/Python/Launcher/" "C:/Users/sebe/AppData/Local/Microsoft/WindowsApps" "C:/Users/sebe/AppData/Local/Programs/MiKTeX/miktex/bin/x64/" "C:/Program Files (x86)/Nmap" "C:/Users/sebe/AppData/Roaming/npm" "c:/Emacs/emacs-28.1/libexec/emacs/28.1/x86_64-w64-mingw32"))
 '(fringe-mode 0 nil (fringe))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(py-autopep8 olivetti projectile perspective elpy magit god-mode pipenv helm auctex jedi))
 '(projectile-project-search-path '("~/git/"))
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
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow4" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "peach puff" :slant oblique))))
 '(font-lock-constant-face ((t (:foreground "SkyBlue1" :weight bold))))
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
 '(widget-field ((t (:background "gray15"))))
 '(window-divider ((t (:foreground "black")))))
