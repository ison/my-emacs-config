;;; init.el
(setq inhibit-startup-message t)

(server-start)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defconst mlint-location '("/Applications/MATLAB_R2017a.app/bin/maci64/mlint" "mac/mlint"))

(global-set-key (kbd "<S-s-return>") 'toggle-fullscreen)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; default frame size
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 202))

;; default fill columns
(setq-default fill-column 99)

;; no new frames on file clicking
(setq ns-pop-up-frames nil)

;; dragging file to buffer opens file.
(define-key global-map [ns-drag-file] 'ns-find-file)

;; turn off indent-tabs-mode
(setq-default indent-tabs-mode nil)

;; load path
(setq load-path (append '("~/.emacs.d/elisp/"
                          ;"~/.emacs.d/elisp/matlab-mode/"
                          )
                        load-path))

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;; info path
(setq Info-default-directory-list
      (append '("~/.info/")
              Info-default-directory-list))

;; custom visible bell
(defun my-visible-bell ()
  (defvar-local mode-line-bgcolor (face-attribute 'mode-line :background))
  (set-face-background 'mode-line "red")
  (run-at-time "0.8 sec" nil
               '(lambda ()
                  (set-face-background 'mode-line mode-line-bgcolor))))
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (my-visible-bell))))

;; machine generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(js-indent-level 2)
 '(mlint-programs mlint-location)
 '(org-agenda-files
   (quote
    ("~/.orgfiles/lrc.org" "~/.orgfiles/embrocation.org")))
 '(package-selected-packages
   (quote
    (auctex-latexmk auctex-lua markdown-preview-mode markdown-mode matlab-mode zenburn-theme web-mode w3m sr-speedbar smex smart-mode-line-powerline-theme paredit osx-lib multi-web-mode memory-usage magithub macro-math levenshtein less-css-mode js2-mode ido-ubiquitous idle-highlight-mode idle-highlight icomplete+ hc-zenburn-theme git-rebase-mode git-commit-mode fuzzy flyspell-popup flyspell-lazy flycheck find-file-in-project exec-path-from-shell etags-table etags-select company-statistics company-math company-jedi company-cmake company-c-headers company-auctex cmake-mode browse-kill-ring auto-complete-octave auto-complete-etags auto-complete-clang anything-obsolete anything-match-plugin anything-config anything-complete anything ac-slime ac-cider-compliment))))
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
;; (setq custom-safe-themes t)
;; (setq custom-theme-directory "~/.emacs.d/themes/")
;;(load-theme 'tabmow t nil)
(defun custom-theme-init ()
  (progn
    (load-theme 'hc-zenburn t)
    ;; smart mode line
    (sml/setup)
    ;; power line
    (powerline-center-theme)
    ;; custom colors
    (set-face-background 'hl-line (face-attribute 'region :background))
    (set-face-background 'region (face-attribute 'isearch :background))))
(add-hook 'after-init-hook 'custom-theme-init)

;; get rid of useless shit
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; turn on useful shit
(global-hl-line-mode t)
(setq show-paren-style 'parenthesis)
(column-number-mode t)

;; marmalade/elpa
;; (require 'package)
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(global-set-key (kbd "C-c C-p") 'package-list-packages)

;;(Eval-After-Load "package"
;;  '(progn
;;     (setq package-enable-at-startup nil)
;;     (add-to-list 'package-archives
;;                  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;     (add-to-list 'package-archives
;;                  '("melpa" . "http://melpa.milkbox.net/packages/"))
;;     (package-initialize)))

;; path variable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; transparencies
(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 95 50))

;; no trailing whitespaces
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; ido-mode
(setq ido-auto-merge-delay-time 99999)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
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
;; (setq battery-mode-line-format "+[%b%p%%]-")
;; (display-battery-mode t)

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
;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; ;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (require 'php-completion)
;;             (php-completion-mode t)
;;             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
;;             (hs-minor-mode 1)))


;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))


(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))
(add-hook 'web-mode-hook 'my-web-mode-hook)

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
(add-hook 'web-mode-hook 'hs-minor-mode)
;; (add-hook 'rhtml-mode-hook 'hs-minor-mode)
(add-hook 'objc-mode-hook 'hs-minor-mode)
;; (add-hook 'php-mode-hook 'hs-minor-mode)

;; auto-complete, auto-complete-clang
;; (require 'auto-complete-config)
;; (setq ac-auto-start 2
;;       ac-override-local-map nil
;;       ac-use-menu-map t
;;       ac-candidate-limit 20)

;; (require 'auto-complete-clang)
;; (require 'auto-complete-etags)
;; ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150618.1949/dict/")
;; (add-to-list 'ac-modes 'objc-mode)
;; (define-key ac-completing-map "\ESC/" 'ac-stop)
;; (add-to-list 'ac-sources 'ac-source-etags)
;; (setq ac-etags-use-document t)


;; ;; (defun ac-octave-mode-setup ()
;; ;;   (setq ac-sources '(ac-source-octave)))
;; ;; (add-hook 'octave-mode-hook
;; ;;           '(lambda () (ac-octave-mode-setup)))

;; ;; etags table
;; (require 'etags-table)
;; (setq etags-table-search-up-depth 10)


;; ;; auto-complete
;; ;; (ac-config-default)
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;; (defun my-ac-php-mode-setup ()
;;   (add-to-list 'ac-sources 'ac-source-php-completion))
;; (defun my-ac-octave-mode-setup ()
;;   (setq ac-sources (append '(ac-source-octave) ac-sources)))
;; (defun my-ac-config ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   ;; (add-hook 'php-mode-hook 'my-ac-php-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (add-hook 'octave-mode-hook 'my-ac-octave-mode-setup)
;;   (add-hook 'matlab-mode-hook 'my-ac-octave-mode-setup)
;;   (global-auto-complete-mode t)
;;   )
;; (my-ac-config)


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


;; company
(require 'company)

;; to solve conflict with using tab for company completion and yasnippet
;; (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))
;; (defun do-yas-expand ()
;;     (let ((yas/fallback-behavior 'return-nil))
;;       (yas/expand)))
;; (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas/minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common)
;;             (indent-for-tab-command)))))

;; (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)
;; (require 'company-auctex)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-transformers 'company-sort-by-occurrence)
(add-hook 'after-init-hook 'company-statistics-mode)
;; (setq company-show-numbers t)

(defun my-latex-mode-company-setup ()
  (progn
    (company-auctex-init)
    (setq-local company-backends
                (append '(company-math-symbols-latex)
                        company-backends))))

;; (defun my-matlab-mode-company-setup ()
;;   (setq-local company-backends
;;               (append '(company-emacs)
;;                       company-backends)))

(add-to-list 'company-backends 'company-matlab-shell)

;; (require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; company backend
;; --------------
;; elisp
;; (require 'finder)


(defun company-elisp-finder-keyword-backend (command &optional arg &rest ign)
  "`company-backend' for finder-keywords."
  (cl-case command
    (prefix
     (and (require 'finder nil t)
          (or (company-grab ":group '\\(\\(\\sw\\|\\s_\\)*\\)" 1)
              (company-grab "Keywords:.*[ \t]+\\(\\(\\sw\\|\\s_\\)*\\)" 1))))
    (candidates (all-completions arg finder-known-keywords))
    (meta (cdr (assoc (intern arg) finder-known-keywords)))))

(defun my-elisp-mode-company-setup ()
  (setq-local company-backends
              (append '(company-elisp-finder-keyword-backend)
                      company-backends)))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-company-setup)
;; (add-to-list 'company-backends 'company-elisp-finder-keyword-backend)


;; flycheck
(require 'flycheck)
(global-flycheck-mode t)


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
(setq org-directory "~/.orgfiles")
(setq org-mobile-inbox-for-pull "~/.orgfiles/inbox.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
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
     ;; (add-to-list
      ;; 'TeX-command-list
      ;; '("ps2pdf" "ps2pdf %f %pf"
      ;;   TeX-run-command t t :help "Convert .ps to .pdf") t)
     (add-to-list
      'TeX-command-list
      '("xpdf" "open -a /Applications/Preview.app %pf"
        TeX-run-command t t :help "Open .pdf file") t)
     (add-to-list
      'TeX-command-list
      '("adobe" "open -a /Applications/Adobe\\ Acrobat\\ XI\\ Pro/Adobe\\ Acrobat\\ Pro.app %pf"
        TeX-run-command t t :help "Open .pdf file in Adobe Acrobat") t)
     (add-to-list
      'TeX-command-list
      '("Glossary" "makeglossaries %s"
        TeX-run-command t t :help "Creates glossary") t)
     (add-to-list
      'TeX-command-list
      '("LuaLaTex" "lualatex %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t"
        TeX-run-command nil (latex-mode doctex-mode) :help "Run LuaLaTeX") t)
     (setq
      TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
     (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
     (auctex-latexmk-setup)
     (setq auctex-latexmk-inherit-TeX-PDF-mode t)
     ;; (
     ;;    "lstlisting")
     ))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; make sure menu bar is on.  seems to be off by default
            ;; for some reason.
            (menu-bar-mode 1)
            (turn-on-auto-fill)
            (setq fill-column 99)
            ;; (setq TeX-PDF-mode nil)
            (flyspell-mode t)
            (yas-minor-mode t)))
            ;; (require 'predictive)
            ;; (predictive-mode t)))
(add-hook 'LaTeX-mode-hook 'my-latex-mode-company-setup)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

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


;; css
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;; idle-highlight-mode
;; (setq idle-highlight-global-timer (run-with-idle-timer 0.01 :repeat 'idle-highlight-word-at-point))

;; ispell
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-current-dictionary "american")
(global-set-key (kbd "C-c s") 'ispell-buffer)

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
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))
(setq matlab-vers-on-startup t)
(setq matlab-shell-command "/usr/local/bin/matlab")
;; '(matlab-shell-command "/usr/local/bin/matlab")
(load-library "matlab-load")
(matlab-cedet-setup)

;; (defun buffer-exists (buffname)
;;   (not (eq nil (get-buffer buffname))))
(defun matlab-shell-split-win ()
  "Starts MATLAB shell splitting the current window horizontally."
  (interactive)
  (progn
    (if (= (count-windows) 1)
        (split-window-horizontally))
    (if (matlab-shell-active-p)
        (if (not (string= (buffer-name) "*MATLAB*"))
            (progn
              (other-window 1)
              (switch-to-buffer "*MATLAB*")))
      (progn
        (other-window 1)
        (matlab-shell)
        (other-window -1)))))
(defun matlab-shell-exit-and-close-window ()
  "Exits MATLAB shell and closes the window"
  (interactive)
  (progn
    (walk-windows
     '(lambda (wx)
        (select-window wx)
        (if (string= (buffer-name) "*MATLAB*")
            (progn
              (if (matlab-shell-active-p)
                  (matlab-shell-exit)
                (kill-buffer nil))
              (if (> (count-windows) 1)
                  (delete-window))))))
    (if (matlab-shell-active-p)
        (progn
          (set-buffer "*MATLAB*")
          (matlab-shell-exit)
          ;; (previous-buffer)
          ))))
(global-set-key (kbd "C-c m") 'matlab-shell)
(global-set-key (kbd "C-c C-m") 'matlab-shell-split-win)
(global-set-key (kbd "C-c n") 'matlab-shell-exit-and-close-window)

(add-hook 'matlab-mode-hook
          '(lambda ()
             ;; (auto-complete-mode)
             (setq fill-column 95)
             ;; (local-set-key (kbd "C-<tab>") 'matlab-toggle-show-mlint-warnings)
             ;; (setq flycheck-matlab-mlint-executable (car mlint-location))
             ;; (local-set-key (kbd "C-c m") 'matlab-shell)
             ;; (local-set-key (kbd "C-c C-m") 'matlab-shell-split-win)
             ;; (local-set-key (kbd "C-c C-n") 'matlab-shell-exit-and-close-window)
             ))

;; (add-hook 'matlab-shell-mode-hook
;;           '(lambda ()
;;              (setq comint-input-ring-file-name "~/.matlab/R2014b/history.m")
;;              (comint-read-input-ring)))

;; (eval-after-load 'flycheck '(require 'flycheck-matlab-mlint))

;; (add-hook 'matlab-mode-hook
;;           '(lambda () (ac-octave-)))

;; (add-hook 'matlab-mode-hook
;;           (lambda ()
;;             (matlab-shell)
;;             (split-window-horizontally)))

;; less-css-mode
(autoload 'less-css-mode "less-css-mode" "Enter Less CSS mode." t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))


;; desktop save mode
;; (desktop-save-mode 1)


;; ESS
;; (load "ess-site")
;; ;; (eval-after-load "ESS"
;; ;;   '(progn
;; ;;      (load "ess-site")))
;; (put 'upcase-region 'disabled nil)

;;; cedet
(global-ede-mode 1)
;; (semantic-load-enable-excessive-code-helpers)      ; Enable prototype help and smart completion
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; Semantic
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-completions-mode)
(global-semantic-decoration-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)
;; (require 'sr-speedbar)
(global-set-key (kbd "C-c C-SPC") 'sr-speedbar-toggle)



(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-everywhere t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(savehist-mode 1)
