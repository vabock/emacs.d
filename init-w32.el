(setq grep-use-null-device nil)

;; フォント設定
(when window-system
  (setq w32-use-w32-font-dialog nil)
  (setq-default line-spacing 1))

(my/build-fontset "JetBrains Mono Medium" 10 "BIZ UDゴシック"
                  '(japanese-jisx0212
                    japanese-jisx0213-2
                    japanese-jisx0213.2004-1
                    katakana-jisx0201))

(add-to-list 'face-font-rescale-alist
             '(".*BIZ UDゴシック.*" . 1.1))

;;; IME の設定
;;; パッケージの読み込み、初期化は環境設定用init-*.elの後に実行されるので注意
(defun my/ime-setup ()
  "ime setup for windows"
  (when (fboundp 'w32-ime-initialize)
    (if (fboundp 'tr-ime-advanced-install)
        (tr-ime-advanced-install))
    (setq default-input-method "W32-IME")
    (setq-default w32-ime-mode-line-state-indicator "[--]") ;; おこのみで
    (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")) ;; おこのみで
    (w32-ime-initialize)
    (global-set-key [M-kanji] 'ignore)))

(add-hook 'after-init-hook #'my/ime-setup)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
