(setq inhibit-splash-screen t)
(setq load-prefer-newer t)

(require 'cl-lib)

(let ((default-directory (expand-file-name (concat user-emacs-directory "elisp"))))
  (when (file-accessible-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (let ((elisp-dirs
           (mapcar #'car
                   (cl-remove-if (lambda (dir) (or (not (eq (cadr dir) t))
                                                   (string-prefix-p "." (car dir))))
                                 (directory-files-and-attributes default-directory)))))
      (if elisp-dirs
          (normal-top-level-add-to-load-path elisp-dirs)))))

(setq default-directory "~/")

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (not window-system)
    (menu-bar-mode 0))

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
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (cl-remove-if (lambda (buf) (string-prefix-p "*" (buffer-name buf))) (buffer-list))))

(define-key-after menu-bar-file-menu [kill-all-buffer] '("Close all" . close-all-buffers) 'kill-buffer)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load (file-name-sans-extension custom-file) t)

(load (expand-file-name "~/.emacs.d/init-main"))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
