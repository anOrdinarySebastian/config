(defvar appearance-mode-light 1)
(defvar appearance-mode-dark 0)
(defvar appearance-mode appearance-mode-dark
  "The current state of the appearance. Default is dark mode")

(defun appearance-toggle-mode ()
  "Function for toggling between dark and light mode"
  (interactive)
  (if (eq appearance-mode appearance-mode-dark)
      (appearance-set-light-mode)
    (appearance-set-dark-mode)))

(defun appearance-set-light-mode ()
  "Function for changing settings to light mode appearance "
  (interactive)
  (setq appearance-mode appearance-mode-light)
  (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "grey10" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
 '(compilation-info ((t (:foreground "LightPink4" :weight bold))))
 '(compilation-warning ((t (:foreground "Orange4" :weight bold))))
 '(cursor ((t (:background "cyan"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow4" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "dark sea green"))))
 '(font-lock-comment-face ((t (:foreground "grey70" :slant oblique))))
 '(font-lock-constant-face ((t (:foreground "SkyBlue1" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "spring green" :slant oblique))))
 '(font-lock-function-name-face ((t (:foreground "dark blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "cyan3"))))
 '(font-lock-string-face ((t (:foreground "grey30"))))
 '(font-lock-type-face ((t (:foreground "light steel blue" :slant italic))))
 '(helm-selection ((t (:extend t :distant-foreground "black" :box (:line-width (2 . 2) :color "grey75" :style released-button) :weight bold))))
 '(helm-source-header ((t (:extend t :background "#22083397778B" :foreground "white" :weight bold :family "Sans Serif"))))
 '(hi-yellow ((t (:background "orange4" :foreground "black"))))
 '(mode-line ((t (:inherit mode-line-buffer-id :background "sky blue" :foreground "Blue" :slant normal :height 0.95))))
 '(mode-line-buffer-id ((t (:inherit mode-line :slant italic :weight bold :height 1.0))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "black" :foreground "light blue" :box nil :weight light :height 0.9))))
 '(widget-field ((t (:background "white"))))
 '(window-divider ((t (:foreground "black"))))))

(defun appearance-set-dark-mode ()
  "Function for changing settings to light mode"
  (interactive)
  (setq appearance-mode appearance-mode-dark)
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
 '(widget-field ((t (:background "gray15"))))
 '(window-divider ((t (:foreground "black"))))))

(provide 'appearance)
