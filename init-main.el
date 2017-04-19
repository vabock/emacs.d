;;; init-main.el   -*- lexical-binding: t -*-

(defmacro append-to-list (to lst &optional last)
  `(set ,to ,(if last
                 `(append (symbol-value ,to) ,lst)
               `(append ,lst (symbol-value ,to)))))

;;; 新規フレームのデフォルト設定
(when (boundp 'window-system)
  (append-to-list 'default-frame-alist
                  '((width               . 120)	; フレーム幅(文字数)
                    (height              . 50))	; フレーム高(文字数)
                  )
  (if (eq window-system 'mac)
      (append-to-list 'default-frame-alist
                      '((top  . 0)
                        (left . 0)))))

(defconst my/bootstrap-packages '(use-package))
(defvar my/packages
  '(company
    auto-compile
    editorconfig
    ddskk
    rg
    wgrep
    nlinum
    yaml-mode
    markdown-mode))

(unless (eq system-type 'windows-nt)
  (append-to-list 'my/packages
                  `(flycheck
                    ,(unless (display-graphic-p) 'flycheck-tip)
                    flycheck-pos-tip
                    magit
                    multi-term)))

(add-to-list 'my/packages
             (cond
              ((eq window-system 'w32) 'twilight-theme)
              ;; ((eq window-system 'mac) 'solarized-theme)
              ((not (display-graphic-p)) 'color-theme-solarized)
              (t nil))
             t)

(require 'cl-lib)

(if (let ((cask-el "~/.cask/cask.el"))
      (or (and (file-exists-p cask-el)
               (require 'cask cask-el))
          (require 'cask nil 'noerror)))
    (progn
      (cask-initialize)
      (require 'pallet))
  ;; setup package.el
  (when (require 'package nil 'noerror)
    (setq package-user-dir "~/.emacs.d/elisp/elpa"
          package-enable-at-startup nil
          package--init-file-ensured t)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)
    (delete 'nil my/packages)
    (let ((packages (cl-remove-if #'package-installed-p
                                  (append my/bootstrap-packages my/packages))))
      (when packages
        (package-refresh-contents)
        (dolist (pkg packages)
          (package-install pkg))))))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(let ((theme-dir (expand-file-name "~/.emacs.d/elisp/solarized-emacs/")))
  (if (file-accessible-directory-p theme-dir)
      (add-to-list 'custom-theme-load-path theme-dir)))

(unless (or (ignore-errors (load-theme 'solarized-dark t)) ; GUI用'solarized-theme'
            (ignore-errors (load-theme 'twilight t)))      ;      'twilight-theme'
  (add-to-list 'default-frame-alist
               '(background-mode . dark))
  (load-theme 'solarized t))            ; コンソール用'color-theme-solarized'

(use-package my-nlinum
  :if (fboundp 'nlinum-mode)
  :commands my/global-nlinum-mode
  :init
  (custom-set-variables '(nlinum-format "%3d"))
  (add-hook 'after-init-hook 'my/global-nlinum-mode))

;; company-mode
(use-package company
  :defer t
  :if (fboundp 'global-company-mode)
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (unless (eq system-type 'darwin)
    (delete 'company-clang 'company-backends))
  
  (defun my/company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (defun my/company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-complete #'my/company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))

    (bind-keys :map company-active-map
               ("<tab>" . company-complete-common-or-cycle)
               ("S-TAB" . company-select-previous)))

  (my/company-ac-setup))

;; flycheck
(use-package flycheck
  :defer t
  :if (fboundp 'global-flycheck-mode)
  :functions (error-tip-delete-popup error-tip-popup-error-message)
  :init
  (unless (eq system-type 'darwin)
    (setq-default flycheck-disabled-checkers '(c/c++-clang)))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defun flycheck-cc-mode-checker-setup ()
    (cond
     ((derived-mode-p 'c-mode 'c++-mode)
      (cond
       ((executable-find "cppcheck")
        (flycheck-select-checker 'c/c++-cppcheck))
       ;; (setq flycheck-clang-include-path
       ;;       (list "/usr/local/Cellar/wxmac/3.0.2/lib/wx/include/osx_cocoa-unicode-3.0"
       ;;             "/usr/local/Cellar/wxmac/3.0.2/include/wx-3.0"))
       (t
        nil)))
     ((derived-mode-p 'python-mode)
      (flycheck-select-checker 'python-pylint))))
  (add-hook 'flycheck-before-syntax-check-hook #'flycheck-cc-mode-checker-setup)
  (bind-keys :map flycheck-mode-map
             ("C-c C-n" . flycheck-next-error)
             ("C-c C-p" . flycheck-previous-error))

  (flycheck-pos-tip-mode)

  (when (and (not (display-graphic-p))
             (require 'error-tip nil 'noerror))
    (defun flycheck-pos-tip-tty-popup (errors)
      (when errors
        (error-tip-delete-popup)
        (let ((msgs (mapcar #'flycheck-error-format-message-and-id errors)))
          (error-tip-popup-error-message msgs))))
    (custom-set-variables
     '(flycheck-pos-tip-display-errors-tty-function #'flycheck-pos-tip-tty-popup))))


(use-package typescript-mode
  :defer t
  :if (fboundp 'tide-setup)
  :init
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'eldoc-mode))

(use-package whitespace
  :defer t
  :diminish (whitespace-mode
             global-whitespace-mode
             whitespace-newline-mode)
  :init
  (add-hook 'after-init-hook #'global-whitespace-mode)
  :config
  (setq whitespace-style '(face           ; faceで可視化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
;;;                         tab-mark
                           ))

  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; 保存前に自動でクリーンアップ
  ;;(setq whitespace-action '(auto-cleanup))
  ;;(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color))


;; git-commit-mode
(use-package git-commit
  :defer t
  :if (fboundp 'global-git-commit-mode)
  :init
  (defun set-commit-log-encoding ()
    (if (string-match-p "^undecided" (symbol-name buffer-file-coding-system))
        (set-buffer-file-coding-system 'utf-8-unix)))
  (add-hook 'git-commit-setup-hook #'set-commit-log-encoding)
  (add-hook 'after-init-hook #'global-git-commit-mode))

;; ag
(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(defvar dropbox-path
  (cond
   ((eq system-type 'windows-nt)
    (let ((documents (concat (getenv "USERPROFILE") "/Documents/My Dropbox"))
          (json-path (concat (getenv "LOCALAPPDATA") "/Dropbox/info.json")))
      (if (file-accessible-directory-p documents)
          documents
        (require 'json)
        (cdr (assoc 'path (assoc 'personal (json-read-file json-path)))))))
   ((eq system-type 'darwin)
    (expand-file-name "~/Dropbox"))
   (t nil)))

;; SKK
(use-package skk
  :defer t
  :init
  (custom-set-variables '(skk-isearch-mode-enable t))

  (bind-keys*
   ("C-x C-j" . skk-mode)
   ("C-x j" . skk-auto-fill-mode))
   ;;("C-x t" . skk-tutorial)

  ;; Macの場合はAquaSKK内蔵のskkservを使う
  (when (eq window-system 'mac)
    (setq skk-server-host "127.0.0.1")
    (setq skk-server-portnum 1178))

  (setq skk-jisyo-code 'utf-8-unix)
  (if dropbox-path
      (setq skk-jisyo (concat dropbox-path "/skk-jisyo.utf8")))

  (unless (boundp 'skk-server-host)
    (let* ((large-jisyo (expand-file-name "~/.emacs.d/etc/skk/SKK-JISYO.L"))
           (cdb (concat large-jisyo ".cdb")))
      (if (file-exists-p cdb)
          (defvar skk-cdb-large-jisyo cdb)
        (defvar skk-large-jisyo large-jisyo))))

  :config
  ;; from skk-setup
  ;; Isearch setting.
  (defun skk-isearch-setup-maybe ()
    (when (or (eq skk-isearch-mode-enable 'always)
              (and (boundp 'skk-mode)
                   skk-mode
                   skk-isearch-mode-enable))
      (skk-isearch-mode-setup)))

  (defun skk-isearch-cleanup-maybe ()
    (when (and (featurep 'skk-isearch)
               skk-isearch-mode-enable)
      (skk-isearch-mode-cleanup)))

  (add-hook 'isearch-mode-hook #'skk-isearch-setup-maybe)
  (add-hook 'isearch-mode-end-hook #'skk-isearch-cleanup-maybe))

;; slime
(use-package slime
  :defer t
  :if (fboundp 'slime-mode)
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-banner slime-indentation))
  (setq slime-net-coding-system 'utf-8-unix)
  (defun slime-mode-setup ()
    (set-variable lisp-indent-function 'common-lisp-indent-function)
    (local-set-key (kbd "RET") 'newline-and-indent))
  (add-hook 'slime-mode-hook #'slime-mode-setup))


;; popwin
(when (require 'popwin nil 'noerror)
  ;; Apropos
  (push '("*slime-apropos*") popwin:special-display-config)
  ;; Macroexpand
  (push '("*slime-macroexpansion*") popwin:special-display-config)
  ;; Help
  (push '("*slime-description*") popwin:special-display-config)
  ;; Compilation
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
  ;; Cross-reference
  (push '("*slime-xref*") popwin:special-display-config)
  ;; Debugger
  (push '(sldb-mode :stick t) popwin:special-display-config)
  ;; REPL
  (push '(slime-repl-mode) popwin:special-display-config)
  ;; Connections
  (push '(slime-connection-list-mode) popwin:special-display-config))

;; coffee-script mode
(use-package coffee-mode
  :defer t
  :config
  (custom-set-variables '(coffee-tab-width 2)))

;; auto-compile-mode
(use-package auto-compile
  :defer t
  :if (fboundp 'auto-compile-on-save-mode)
  :init
  (add-hook 'after-init-hook #'auto-compile-on-save-mode))

(use-package editorconfig
  :defer t
  :if (fboundp 'editorconfig-mode)
  :diminish editorconfig-mode
  :init
  (add-hook 'after-init-hook #'editorconfig-mode))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

(defun my/buffer-menu-adv (args)
  (or (and (car args)
           args)
      '(1)))

(advice-add 'list-buffers :filter-args #'my/buffer-menu-adv)

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
