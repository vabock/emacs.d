(setq inhibit-splash-screen t)

(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (when (file-accessible-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

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

(set-language-environment "japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-file-name-coding-system
      (if (eq window-system 'w32)
          'japanese-cp932-dos
        'utf-8-unix))

;; Win32用設定読み込み
(if (eq window-system 'w32)
    (load (expand-file-name "~/.emacs.d/init-w32")))

;; NTEmacs以外の場合
(unless (eq system-type 'windows-nt)
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;(keyboard-translate ?\C-h ?\C-?)
;(global-set-key "\C-h" nil)
;;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-xh" 'help-command)

(global-unset-key "\C-xm")

;;; vcを停止する
(setq vc-handled-backends nil)

;;; すべてのバッファを閉じるメニューアイテムを追加
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(define-key-after menu-bar-file-menu [kill-all-buffer] '("Close all" . close-all-buffers) 'kill-buffer)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;;; 新規フレームのデフォルト設定
(if (boundp 'window-system)
    (setq default-frame-alist
          (append
           '((width               . 120)	; フレーム幅(文字数)
             (height              . 50))	; フレーム高(文字数)
           default-frame-alist)))

(if (require 'cask "~/.cask/cask.el" 'noerror)
    (cask-initialize)
  (progn
    ;; setup package.el
    (when (require 'package nil 'noerror)
      (setq package-user-dir "~/.emacs.d/elisp/elpa")
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize))))

;(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/solarized-emacs")
;(load-theme ‘solarized-dark t)
(unless (require 'solarized-dark-theme nil 'noerror)
  (when (locate-library "color-theme-twilight")
    (require 'color-theme)
    (color-theme-initialize)
    (require 'color-theme-twilight)
    (color-theme-twilight)))

(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
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
(setq whitespace-action '(auto-cleanup))
;(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(global-whitespace-mode 1)

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
                    :background my/bg-color)

;; SKK
(when (require 'skk-autoloads nil 'noerror)
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" 'skk-auto-fill-mode)
  (global-set-key "\C-xt" 'skk-tutorial)

  (let* ((large-jisyo (expand-file-name "~/.emacs.d/etc/skk/SKK-JISYO.L"))
         (cdb (concat large-jisyo ".cdb")))
    (if (file-exists-p cdb)
        (setq skk-cdb-large-jisyo cdb)
      (defvar skk-large-jisyo large-jisyo))))

;; auto-complete
(when (require 'auto-complete-config nil 'noerror)
  (ac-config-default)
  (set-face-background 'popup-tip-face "darkgray")
  (set-face-background 'ac-candidate-face "darkgray"))

;; slime
(when (require 'slime-autoloads nil 'noerror)
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-banner slime-indentation))
  (setq slime-net-coding-system 'utf-8-unix)
  (eval-after-load "slime"
    '(progn
       (require 'ac-slime)
       (add-hook 'slime-mode-hook 'set-up-slime-ac)
       (add-hook 'slime-mode-hook
                 (function (lambda ()
                             (set-variable lisp-indent-function 'common-lisp-indent-function)
                             (local-set-key (kbd "RET") 'newline-and-indent))))
       (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))))

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
(eval-after-load "coffee-mode"
  '(add-hook 'coffee-mode-hook
             #'(lambda ()
                 (and (set (make-local-variable 'tab-width) 2)
                      (set (make-local-variable 'coffee-tab-width) 2)))))

;; auto-compile-mode
;; http://emacswiki.org/emacs/AutoRecompile
;; https://github.com/tarsius/auto-compile
;(autoload 'auto-compile-mode "auto-compile" nil t)
(when (require 'auto-compile nil 'noerror)
;  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(require 'server)
(unless (server-running-p)
  (server-start))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
