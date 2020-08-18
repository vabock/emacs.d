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
                        (left . 0))))
  (if (eq window-system 'w32)
      (setf (alist-get 'height default-frame-alist) 45)))

(defconst my/bootstrap-packages '(use-package diminish))
(defvar my/packages
  `(company
    auto-compile
    editorconfig
    ddskk
    rg
    wgrep
    ,(if (< emacs-major-version 26) 'nlinum)
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
              (t 'solarized-theme))
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
                 '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (let ((packages (cl-remove-if #'package-installed-p
                                  (append my/bootstrap-packages (delete 'nil my/packages)))))
      (when packages
        (package-refresh-contents)
        (dolist (pkg packages)
          (package-install pkg))))

    ;; リストをStatus降順でソートする
    (defun my/package-menu-sort-status-desc ()
      (setq tabulated-list-sort-key (cons "Status" t)))
    (add-hook 'package-menu-mode-hook #'my/package-menu-sort-status-desc)))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(let ((theme-dir (expand-file-name "~/.emacs.d/elisp/solarized-emacs/")))
  (if (file-accessible-directory-p theme-dir)
      (add-to-list 'custom-theme-load-path theme-dir)))

(or (ignore-errors (load-theme 'solarized-dark t))      ; 'solarized-theme'
    (ignore-errors (load-theme 'twilight t)))           ; 'twilight-theme'

(use-package my-nlinum
  :if (and (fboundp 'nlinum-mode)
           (< emacs-major-version 26))
  :commands my/global-nlinum-mode
  :init
  (custom-set-variables '(nlinum-format "%3d"))
  (add-hook 'after-init-hook #'my/global-nlinum-mode))

(use-package display-line-numbers
  :defer t
  :if (>= emacs-major-version 26)
  :init
  (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (defun my/display-line-numbers--turn-on ()
    "Turn on `display-line-numbers-mode'."
    (unless (or (minibufferp)
                (member major-mode '(shell-mode eshell-mode))
                (string-match "*" (buffer-name))
                (> (buffer-size) 3000000)
                ;; taken from linum.el
                (and (daemonp) (null (frame-parameter nil 'client))))
      (display-line-numbers-mode)))
  :config
  (fset 'display-line-numbers--turn-on 'my/display-line-numbers--turn-on))

;; company-mode
(use-package company
  :defer t
  :if (fboundp 'global-company-mode)
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (unless (eq system-type 'darwin)
    (setq-default company-backends (delete 'company-clang company-backends)))
  (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map))

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

(use-package magit-process
  :if (fboundp 'magit-version)
  :commands magit-process-file)

;; ag
(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(defvar dropbox-path
  (cond
   ((eq system-type 'windows-nt)
    (let ((userfolder (concat (getenv "USERPROFILE") "/Dropbox"))
          (json-path (concat (getenv "LOCALAPPDATA") "/Dropbox/info.json")))
      (cond
       ((file-accessible-directory-p userfolder)
        userfolder)
       ((file-exists-p json-path)
        (require 'json)
        (cdr (assoc 'path (assoc 'personal (json-read-file json-path)))))
       (t nil))))
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

(defun my/ignoring-dotfiles-f-n-completion (func &rest args)
  (let ((x (apply func args)))
    (if (and (listp x) (stringp (car x))
             (cdr x))
        (let ((result (cl-remove-if (lambda (f) (string-prefix-p "." f)) x)))
          (if (and (listp result) (cdr result))
              result
            x))
      x)))

(advice-add 'completion-file-name-table :around #'my/ignoring-dotfiles-f-n-completion)

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
