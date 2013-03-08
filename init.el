(setq inhibit-splash-screen t)

(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (when (file-accessible-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(tool-bar-mode 0)
(global-linum-mode 1)
(column-number-mode 1)
(line-number-mode 0)
;; カーソル行を強調表示
(global-hl-line-mode 1)
;;; ホイールマウス
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

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;(keyboard-translate ?\C-h ?\C-?)
;(global-set-key "\C-h" nil)
;;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-xh" 'help-command)

;;; vcを停止する
(setq vc-handled-backends nil)

;;; すべてのバッファを閉じるメニューアイテムを追加
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(define-key-after menu-bar-file-menu [kill-all-buffer] '("Close all" . close-all-buffers) 'kill-buffer)

;;; 新規フレームのデフォルト設定
(if (boundp 'window-system)
    (setq default-frame-alist
          (append
           '((width               . 120)	; フレーム幅(文字数)
             (height              . 50))	; フレーム高(文字数)
           default-frame-alist)))

(when (locate-library "color-theme")
  (require 'color-theme)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (require 'color-theme-twilight)
       (color-theme-twilight))))

;; SKK
(require 'skk-autoloads)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

; (setq skk-large-jisyo (expand-file-name "~/.emacs.d/share/skk/SKK-JISYO.L"))
(let ((cdb (expand-file-name "~/.emacs.d/share/skk/SKK-JISYO.L.cdb")))
  (if (file-exists-p cdb)
      (setq skk-cdb-large-jisyo cdb)))

;; coffee-script mode
(autoload 'coffee-mode "coffee-mode" nil t)
;(require 'coffee-mode)
(setq coffee-tab-width 4)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; js2-mode
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; yaml-mode
;; https://github.com/yoshiki/yaml-mode
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; auto-compile-mode
;; http://emacswiki.org/emacs/AutoRecompile
;; https://github.com/tarsius/auto-compile
;(autoload 'auto-compile-mode "auto-compile" nil t)
(require 'auto-compile)
(auto-compile-global-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

(put 'narrow-to-region 'disabled nil)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: lisp
;;; mode: auto-compile
;;; End:
