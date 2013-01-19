;; フォント設定
(setq w32-enable-synthesized-fonts t)
(setq w32-use-w32-font-dialog nil)
(setq-default line-spacing 1)

(when t
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 120)

  (dolist (target '(japanese-jisx0212
                    japanese-jisx0213-2
                    japanese-jisx0213.2004-1
                    katakana-jisx0201))
    (set-fontset-font (frame-parameter nil 'font)
                      target
                      (font-spec :family "ＭＳ ゴシック" :registry "unicode-bmp" :lang 'ja))))

;;; IME の設定
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]") ;; おこのみで
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")) ;; おこのみで
(w32-ime-initialize)
(defun nop () (interactive) t)
(global-set-key (kbd "M-<kanji>") 'nop)

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: lisp
;;; mode: auto-compile
;;; End:
