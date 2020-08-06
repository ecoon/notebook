;; turn off the stupid splash screen
(setq inhibit-startup-screen t)

;; comments are always appreciated
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; flyspell
(setq ispell-silently-savep t)
(global-set-key (kbd "M-#") 'flyspell-auto-correct-word)

 
;; misc.el zap-up-to-char
(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; MELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; org-mode
(require 'ox-rst)

;; C/ C++
(setq c++-mode-hook
      '(lambda () "Defaults for C++ mode." (setq fill-column 79)))

;; -- set functions to limit argument indentation after long function names
(defconst my-c-lineup-maximum-indent 30)

(defun my-c-lineup-arglist (langelem)
  (let ((ret (c-lineup-arglist langelem)))
    (if (< (elt ret 0) my-c-lineup-maximum-indent)
        ret
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) 8))))))

(defun my-indent-setup ()
  (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
   '(c-lineup-gcc-asm-reg my-c-lineup-arglist)))

(defun my-c-mode-common-hook ()
  (google-set-c-style)
  (my-indent-setup)
  ;; my customizations for all of c-mode and related modes
  (setq-default c-basic-offset 2)
  (c-toggle-hungry-state)
;;  (c-set-offset 'substatement-open 0)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq-default whitespace-line-column 80 whitespace-style '(face lines-tail))
(add-hook 'c++-mode #'whitespace-mode)
(add-hook 'c-mode #'whitespace-mode)

(setq python-mode-hook
      '(lambda () "Default for Python mode." (setq fill-column 79)))
(add-hook 'python-mode #'whitespace-mode)

;; powerline
;;(require 'powerline)
;;(powerline-default-theme)
(require 'spaceline-config)
(spaceline-emacs-theme)

;; project management
(setq projectile-keymap-prefix (kbd "M-p"))
(require 'projectile)
(projectile-global-mode)

;; ivy for swiper and cleaner searching wiht regex
(require 'ivy)
(require 'avy)

(defun tv/swiper-backward (&optional initial-input)
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . tv/ivy-recompute-index-swiper-backward))))
    (swiper initial-input)))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
(setq ivy-count-format "(%d/%d) ")
(setq swiper-goto-start-of-match t)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-x y") 'counsel-yank-pop)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-<return>") 'ivy-immediate-done)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char)
(counsel-projectile-mode)

;; tabs suck
(setq-default indent-tabs-mode nil)

;; randomly stolen stuff to work with iterm2
(defun get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	"~/"
      (file-name-directory filename))))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"pushd %s\" \n" (get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

(global-set-key (kbd "M-=") 'iterm-goto-filedir-or-home)


;; eshell customization
(setq eshell-cmpl-cycle-completions nil)

;; clang-format
(load "/usr/local/Cellar/clang-format/2019-05-14/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)


;; adaptive-wrap
(setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

