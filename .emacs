
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
;;	 (add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;;(load "powershell.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "#d0d0d0"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(package-selected-packages (quote (irony jupyter markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; C stuff
(defun mp-add-c-keys ()
  (local-set-key "\C-cc" 'compile)
  (local-set-key "\C-cr" 'gdb))

(add-hook 'c-mode-hook 'mp-add-c-keys)

;;(add-hook 'c-mode-hook
;;          (lambda ()
;;            (local-set-key "\C-cc" 'compile)))
;;(add-hook 'c-mode-hook
;;          (lambda ()
;;            (local-set-key "\C-cr" 'gdb)))

;; Verilog stuff
(setq verilog-indent-level 2)

;; Markdown stuff

(defun md-insert-dash-line (char_to_insert)
  (interactive)
  (insert (make-string fill-column 'char_to_insert)))

(defun mp-add-md-keys ()
  (local-set-key "\C-cl" (md-insert-dash-line ?-))
  (local-set-key "\C-c\C-l" (kbd "\C-u70=")))

(add-hook 'markdown-mode-hook 'mp-add-md-keys)

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

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(defun next-double-window ()
    (interactive nil)
        (other-window 2))
