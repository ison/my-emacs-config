(setq inhibit-startup-message t)

(server-start)

(global-set-key (kbd "<S-s-return>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; no new frames on file clicking
(setq ns-pop-up-frames nil)

;; dragging file to buffer opens file.
(define-key global-map [ns-drag-file] 'ns-find-file)

;; turn off indent-tabs-mode
(setq-default indent-tabs-mode nil)

;; load path
(setq load-path (append '("~/.emacs.d/elisp/"
                          ;; "~/.emacs.d/elisp/color-theme-6.6.0/"
                          ;; "~/.emacs.d/elisp/color-theme-wombat/"
                          "~/.emacs.d/elisp/auto-complete/"
                          "~/.emacs.d/elisp/auto-complete-clang/"
                          "~/.emacs.d/elisp/auto-complete-etags/"
                          "~/.emacs.d/elisp/etags-table/"
                          "~/.emacs.d/elisp/slime/"
                          "~/.emacs.d/elisp/swank-js/"
                          ;; "~/.emacs.d/elisp/php-mode/"
                          "~/.emacs.d/elisp/php-mode-1.5.0/"
                          "~/.emacs.d/elisp/php-completion/"
                          "~/.emacs.d/elisp/js2-mode/"
                          ;; "~/.emacs.d/elisp/org-mode/lisp"
                          ;; "~/.emacs.d/elisp/org-mode/contrib/lisp/"
                          "~/.emacs.d/elisp/predictive/"
                          "~/.emacs.d/elisp/predictive/latex/"
                          "~/.emacs.d/elisp/predictive/texinfo/"
                          "~/.emacs.d/elisp/predictive/html/"
                          "~/.emacs.d/elisp/rinari/"
                          "~/.emacs.d/elisp/rhtml/"
                          "~/.emacs.d/elisp/epresent/"
                          "~/.emacs.d/elisp/multi-web-mode/"
                          "~/.emacs.d/elisp/matlab-emacs/")
                        load-path))

;; (let ((default-directory  "~/.emacs.d/elisp/"))
;;      (normal-top-level-add-subdirs-to-load-path))

;; add /usr/local/bin/ to exec-path.  seems to be missing it by default.
(add-to-list 'exec-path "/usr/local/bin/")

;; get PATH env var from bashrc
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))

;; info path
(setq Info-default-directory-list
      (append '("~/.info/")
              Info-default-directory-list))

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

;; machine generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(org-agenda-files (quote ("~/.orgfiles/id29/embrocation.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; themes
;; (require 'color-theme)
;; (require 'color-theme-wombat)
;; ;; (load "~/.emacs.d/elisp/color-theme-wombat+-0.0.2/color-theme-wombat+")
;; (eval-after-load "color-theme"
;;   '(lambda ()
;;      (color-theme-initialize)
;;      (color-theme-wombat)))
(setq custom-safe-themes t)
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'tabmow t nil)

;; get rid of useless shit
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; turn on useful shit
(global-hl-line-mode t)
(setq show-paren-style 'parenthesis)
(column-number-mode t)

;; marmalade/elpa
;; (require 'package)
(eval-after-load "package"
 '(progn
    (add-to-list 'package-archives
            '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (package-initialize)))

;; transparencies
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;; no trailing whitespaces
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; ido-mode
(setq ido-auto-merge-delay-time 99999)
(setq ido-enable-flex-matching t)
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))
(global-set-key (kbd "C-c p") 'ido-find-file-in-tag-files)
;; (defun ido-goto-bookmark (bookmark)
;;   (interactive
;;    (list (bookmark-completing-read
;;           "Jump to bookmark"
;;           bookmark-current-bookmark)))
;;   (unless bookmark
;;     (error "No bookmark specified"))
;;   (let ((enable-recursive-minibuffers t)
;;         (filename (bookmark-get-filename bookmark)))
;;     (ido-set-current-directory
;;      (if (file-directory-p filename)
;;          filename
;;        (file-name-directory filename)))
;;     (setq ido-exit        'refresh
;;           ido-text-init   ido-text
;;           ido-rotate-temp t)
;;     (exit-minibuffer)))
;; (global-set-key (kbd "C-M-b") 'ido-goto-bookmark)

;; js2-mode
;; (setq js2-basic-offset 4)
;; (setq js2-cleanup-whitespace t)
;; (setq js2-missing-semi-one-line-override t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'js2-mode "js2-mode" "Major mode for editing JavaScript code." t nil)
(add-hook 'js2-mode-hook
	  (lambda ()
            (setq js2-basic-offset 2)
            (setq js2-cleanup-whitespace t)
            (setq js2-missing-semi-one-line-override t)
	    (define-key js2-mode-map (kbd "C-=") 'js2-mode-toggle-element)
	    (define-key js2-mode-map (kbd "C-+") 'js2-mode-toggle-hide-functions)))

;; battery mode
(setq battery-mode-line-format "+[%b%p%%]-")
(display-battery-mode t)

;; text-mode
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (auto-fill-mode 1)))
;; (add-hook 'text-mode-hook 'text-mode-hook-identify)

;; comment-uncomment
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
        (line-beginning-position) (line-end-position))
      (if (< (point) (mark))
          (comment-or-uncomment-region (point) (mark))
        (comment-or-uncomment-region (mark) (point)))))
(global-set-key (kbd "C-`") 'comment-or-uncomment-region-or-line)

;; cursor color
;; (add-to-list 'default-frame-alist '(cursor-color . "red"))

;; region color
;; (set-face-background 'region "royalblue3")

;; octave
(setq inferior-octave-program "/usr/local/bin/octave")
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-completion)
            (php-completion-mode t)
            (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
            (hs-minor-mode 1)))

;; hide-show
(defun toggle-hiding ()
  (interactive)
  (if (condition-case nil
	  (hs-toggle-hiding)
	(error t))
      (hs-show-all)))
(defun toggle-hiding-all ()
  (interactive)
  (setq hs-hide-state (not hs-hide-state))
  (if hs-hide-state (hs-hide-all) (hs-show-all)))
(add-hook 'hs-minor-mode-hook
	  (lambda ()
	    (defvar hs-hide-state nil "Current state of hideshow for toggling all.")
	    (define-key hs-minor-mode-map (kbd "C-=") 'toggle-hiding)
	    (define-key hs-minor-mode-map (kbd "C-+") 'toggle-hiding-all)))
(add-hook 'css-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'html-mode-hook 'hs-minor-mode)
;; (add-hook 'rhtml-mode-hook 'hs-minor-mode)
(add-hook 'objc-mode-hook 'hs-minor-mode)
;; (add-hook 'php-mode-hook 'hs-minor-mode)

;; auto-complete, auto-complete-clang
(require 'auto-complete-config)
(require 'auto-complete-clang)
(require 'auto-complete-etags)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict/")
(add-to-list 'ac-modes 'objc-mode)
(define-key ac-completing-map "\ESC/" 'ac-stop)
(add-to-list 'ac-sources 'ac-source-etags)
(setq ac-etags-use-document t)

;; etags table
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;; slime, slime-js
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-repl))
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'slime)
            (slime-setup '(slime-repl slime-js))
            (local-set-key [f5] 'slime-js-reload)
            (slime-js-minor-mode 1)))

;; auto-complete
;; (ac-config-default)
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(defun my-ac-php-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-php-completion))
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'php-mode-hook 'my-ac-php-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)

;; yasnippet
;; (require 'yasnippet)
;; (eval-after-load "yasnippet"
;;   '(lambda ()
;;      (yas/initialize)
;;      (yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets/")))
;; (add-hook 'rinari-minor-mode-hook
;;           '(lambda ()
;;              (setq yas/mode-symbol 'rails-mode)))
;; (add-hook 'rhtml-mode-hook
;;           '(lambda ()
;;              (setq yas/mode-symbol 'html-mode)))

;; ac-slime
;; (require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;; doc view
;; (setq auto-mode-alist (cons '("\\.pdf$" . doc-view-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.dvi$" . doc-view-mode) auto-mode-alist))

;; paredit
(defun turn-on-paredit ()
  (paredit-mode +1))
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'lisp-mode-hook 'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook 'turn-on-paredit)
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; org-mode
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook
          (lambda ()
            (load "epresent")
            (auto-fill-mode -1)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; bookmark
;; (defun bookmark-to-abbrevs ()
;;    "Create abbrevs based on `bookmark-alist'."
;;    (dolist (bookmark bookmark-alist)
;;    (let* ((name (car bookmark))
;;           (file (bookmark-get-filename name)))
;;      (define-abbrev fundamental-mode-abbrev-table name file))))
(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))
(global-set-key (kbd "C-x p d") 'bookmark-delete)

;; auctex
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list '("%pf" file "pdf" t) t)
     (add-to-list
      'TeX-command-list
      '("ps2pdf" "ps2pdf %f %pf"
        TeX-run-command t t :help "Convert .ps to .pdf") t)
     (add-to-list
      'TeX-command-list
      '("xpdf" "open -a /Applications/Preview.app %pf"
        TeX-run-command t t :help "Open .pdf file") t)
     (add-to-list
      'TeX-command-list
      '("adobe" "open -a /Applications/Adobe\\ Acrobat\\ 9\\ Pro/Adobe\\ Acrobat\\ Pro.app %pf"
        TeX-run-command t t :help "Open .pdf file in Adobe Acrobat") t)))

;; '(add-to-list 'TeX-expand-list '("%pf" file "pdf" t) t))
;; (eval-after-load "tex"
;;   '(add-to-list
;;     'TeX-command-list
;;     '("ps2pdf" "ps2pdf %f %pf"
;;       TeX-run-command t t :help "Convert .ps to .pdf") t))
;; (eval-after-load "tex"
;;   '(add-to-list
;;     'TeX-command-list
;;     '("xpdf" "open -a /Applications/Preview.app %pf"
;;       TeX-run-command t t :help "Open .pdf file") t))
;; (eval-after-load "tex"
;;   '(add-to-list
;;     'TeX-command-list
;;     '("adobe" "open -a /Applications/Adobe\\ Acrobat\\ 9\\ Pro/Adobe\\ Acrobat\\ Pro.app %pf"
;;       TeX-run-command t t :help "Open .pdf file in Adobe Acrobat") t))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; make sure menu bar is on.  seems to be off by default
            ;; for some reason.
            (menu-bar-mode 1)
            (setq TeX-PDF-mode nil)
            (require 'predictive)
            (predictive-mode t)))

;; nxhtml
;; (load "~/.emacs.d/elisp/nxhtml/autostart")

;; eshell
(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
	;; simply delete the region
	(delete-region (point-min) (point-max))))
;; To make eshell understand .pl perl extensions.
(add-hook 'eshell-named-command-hook 'n-eshell-exec-perl)
(defun n-eshell-exec-perl (command args)
  "04Dec2001 - sailor, to make eshell understand perl scripts."
  (if (string-match "^.+\.pl[ \t]*" command)
      (progn
         (setq args (cons command args))
         (setq args (cons "-S" args))
         (throw 'eshell-replace-command
                (eshell-parse-command "perl" args)))))

;; rhtml, rinari
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(autoload 'rhtml-mode "rhtml-mode" "Minor Mode for editing rhtml file in Emacs" t nil)
(add-hook 'rhtml-mode-hook
          (lambda ()
            (require 'rinari)
            (rinari-launch)))

;; macro-math
(defun macro-math-eval-region-or-line ()
  "Like macro-math-eval-region, but if there's no mark \(that means no
region\) apply macro-math-eval-region to the current line"
  (interactive)
  (if (not mark-active)
      (macro-math-eval-region
        (line-beginning-position) (line-end-position))
      (macro-math-eval-region (point) (mark))))
(defun macro-math-eval-and-round-region-or-line ()
  "Like macro-math-eval-and-round-region, but if there's no mark \(that means no
region\) apply macro-math-eval-and-round-region to the current line"
  (interactive)
  (if (not mark-active)
      (macro-math-eval-and-round-region
        (line-beginning-position) (line-end-position))
      (macro-math-eval-and-round-region (point) (mark))))
(global-set-key (kbd "C-c =") 'macro-math-eval-region-or-line)
(global-set-key (kbd "C-c ~") 'macro-math-eval-and-round-region-or-line)

;; quick access to refcard
(setq refcard-dir "~/.emacs.d/refcards/")
(defun open-refcard-dir-file (filename)
  "Opens reference card file located in <refcard-dir> given file name"
  (find-file (concat refcard-dir filename)))
(defun open-refcard-emacs ()
  "Opens refcard.pdf"
  (interactive)
  (open-refcard-dir-file "refcard.pdf"))
(defun open-refcard-org ()
  "Opens orgcard.pdf"
  (interactive)
  (open-refcard-dir-file "orgcard.pdf"))
(global-set-key (kbd "s-r") 'open-refcard-emacs)
(global-set-key (kbd "C-c s-o") 'open-refcard-org)

;; multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;idle-highlight-mode
(setq idle-highlight-global-timer (run-with-idle-timer 0.01 :repeat 'idle-highlight-word-at-point))

;; ispell
(setq ispell-program-name "aspell")

;; clojure-mode
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192") nil))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)

;; make M-w copy from point to end of line if region is not defined
(defun copy-region-or-line-as-kill ()
  "Copies region to kill ring if region is define else
copies the line from point to end of line."
  (interactive)
  (if (not mark-active)
      (copy-region-as-kill
       (point) (line-end-position))
    (copy-region-as-kill (point) (mark))))
(global-set-key (kbd "M-w") 'copy-region-or-line-as-kill)

(require 'windmove)
(windmove-default-keybindings 'super)

;; matlab-mode
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
(setq matlab-vers-on-startup t)
(add-hook 'matlab-mode-hook
          (lambda ()
            (matlab-shell)
            (split-window-horizontally)))
