
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
;;	 (add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;;(load "powershell.el")

(setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
(setq shell-file-name explicit-shell-file-name)
(add-to-list 'exec-path "C:\\Program Files\\Git\\bin\\bash.exe")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "#d0d0d0"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (irony jupyter markdown-mode)))
 '(package-selected-packages (quote (auctex jedi markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Consolas"))))

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))

;; On save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(set-default 'truncate-lines t)

(defun prev-window ()
	(interactive nil)
	(other-window -1))

(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-c o") 'next-double-window)
(global-set-key (kbd "C-c d") 'kill-whole-line)
(global-set-key (kbd "C-x C-n") (kbd "C-x C-<right>"))
(global-set-key (kbd "C-x C-p") (kbd "C-x C-<left>"))

;; Line highlighting and line number show
;; (global-hl-line-mode t)
;; global-display-line-number only works in emacs version > 26.1
;;(global-linum-mode 0) ;; remove ugly linums
(global-display-line-numbers-mode t)

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

;; ================= MODE SPECIFICS ==========================

;; C mode
(defun mp-add-c-keys ()
  (local-set-key "\C-cc" 'compile)
  (local-set-key "\C-cr" 'gdb))

(add-hook 'c-mode-hook 'mp-add-c-keys)

;; Verilog mode
(setq verilog-indent-level 2)

;; Tex mode

(setq reftex-plug-into-AUCTeX t)

(defun tex-init-addons ()
  "Unnecessary function to start latex in visual line mode"
  ()
  (visual-line-mode t)
  (turn-on-reftex)
  )

(add-hook 'TeX-mode-hook 'tex-init-addons)

;; Markdown mode

(defun md-insert-dash-line (char_to_insert)
  (interactive)
  (insert (make-string fill-column 'char_to_insert)))

(defun mp-add-md-keys ()
  (local-set-key "\C-cl" (md-insert-dash-line ?-))
  (local-set-key "\C-c\C-l" (kbd "\C-u70=")))

;;(add-hook 'markdown-mode-hook 'mp-add-md-keys)

(remove-hook 'markdown-mode-hook 'delete-trailing-whitespace t)
(add-hook 'markdown-mode-hook
          (function
           (lambda
             ()
             (interactive)
             (define-key  markdown-mode-map (kbd "<S-return>")
               (lambda ()(interactive)
                 (insert "  ")
                 (newline))
               )
             )
           )
          'mp-add-md-keys
          )

;; Python mode
;; Jedi for code suggestions
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (function (lambda()
                                        (setq indent-tabs-mode nil
                                              tab-width 2))))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;;(setq jedi:complete-on-dot t)  ; optional

;; Verilog mode
(setq verilog-indent-level 2)


(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(defun next-double-window ()
    (interactive nil)
        (other-window 2))
