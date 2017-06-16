(setq inhibit-splash-screen t)
(setq load-prefer-newer t)

(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (when (file-accessible-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(setq default-directory "~/")

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (not window-system)
    (menu-bar-mode 0))

(global-linum-mode 1)
(column-number-mode 1)
(line-number-mode 0)
;; カーソル行を強調表示
(global-hl-line-mode (if window-system 1 0))
;;; ホイールマウス
(unless (fboundp 'track-mouse)
  (defun track-mouse (e)))
;(xterm-mouse-mode 1)
(require 'mouse)
(require 'mwheel)
(mouse-wheel-mode 1)
(setq mouse-wheel-follow-mouse t)
;;http://stackoverflow.com/questions/445873/emacs-mouse-scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; ファイル補完時に大小文字を区別しない
(setq completion-ignore-case t)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; ベルを無効にする
(setq ring-bell-function 'ignore)
;;(setq visible-bell t)

;; 最近使ったファイルメニューを追加
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 20)
(recentf-mode 1)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 対応する括弧を強調表示
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;(set-language-environment "japanese")
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(cond
 ((eq system-type 'windows-nt)
  (set-file-name-coding-system 'japanese-cp932)
  (load (expand-file-name "~/.emacs.d/init-w32")))
 ((eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (load (expand-file-name "~/.emacs.d/init-mac")))
 (t
  (set-file-name-coding-system 'utf-8)
  (define-key local-function-key-map "\033[37;6~" (kbd "C-%"))))

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;(keyboard-translate ?\C-h ?\C-?)
;(global-set-key "\C-h" nil)
;;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-xh" 'help-command)

(global-unset-key "\C-xm")

;;; vcを停止する
(setq vc-handled-backends nil)

;;; すべてのバッファを閉じるメニューアイテムを追加
(require 'cl-lib)
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (cl-remove-if (lambda (buf) (string-prefix-p "*" (buffer-name buf))) (buffer-list))))

(define-key-after menu-bar-file-menu [kill-all-buffer] '("Close all" . close-all-buffers) 'kill-buffer)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load (file-name-sans-extension custom-file) t)

(defmacro append-to-list (to lst)
  `(setq ,to (append ,lst ,to)))

;;; 新規フレームのデフォルト設定
(when (boundp 'window-system)
  (append-to-list default-frame-alist
                  '((width               . 120)	; フレーム幅(文字数)
                    (height              . 50))	; フレーム高(文字数)
                  )
  (if (eq window-system 'mac)
      (append-to-list default-frame-alist
                      '((top  . 0)
                        (left . 0)))))

(if (let ((cask-el "~/.cask/cask.el"))
      (or (and (file-exists-p cask-el)
               (require 'cask cask-el))
          (require 'cask nil 'noerror)))
    (progn
      (cask-initialize)
      (require 'pallet))
  (progn
    ;; setup package.el
    (when (require 'package nil 'noerror)
      (setq package-user-dir "~/.emacs.d/elisp/elpa")
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize))))

(if (require 'use-package nil 'noerror)
    (require 'bind-key)
  (defmacro use-package (&rest args)))

(let ((theme-dir (expand-file-name "~/.emacs.d/elisp/solarized-emacs/")))
  (if (file-accessible-directory-p theme-dir)
      (add-to-list 'custom-theme-load-path theme-dir)))

(unless (or (ignore-errors (load-theme 'solarized-dark t)) ; GUI用'solarized-theme'
            (ignore-errors (load-theme 'twilight t)))      ;      'twilight-theme'
  (add-to-list 'default-frame-alist
               '(background-mode . dark))
  (load-theme 'solarized t))            ; コンソール用'color-theme-solarized'

;; company-mode
(use-package company
  :defer t
  :if (fboundp 'global-company-mode)
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map))

;; flycheck
(use-package flycheck
  :defer t
  :if (fboundp 'global-flycheck-mode)
  :functions (error-tip-delete-popup error-tip-popup-error-message)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defun flycheck-cc-mode-checker-setup ()
    (cond
     ((derived-mode-p 'c-mode 'c++-mode)
      (cond
       ((eq system-type 'darwin)
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

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
