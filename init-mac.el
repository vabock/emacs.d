;; (let* ((size 16)
;;        (asciifont "Ricty") ; ASCII fonts
;;        (jpfont "Ricty") ; Japanese fonts
;;        (h (* size 10))
;;        (fontspec (font-spec :family asciifont))
;;        (jp-fontspec (font-spec :family jpfont)))
;;   (set-face-attribute 'default nil :family asciifont :height h)
;;   (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
;;   (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
;;   (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
;;   (set-fontset-font nil '(#x0080 . #x024F) fontspec)
;;   (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
(set-face-attribute 'default nil :family "Menlo" :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0213.2004-1
                  (font-spec :family "Ricty"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (font-spec :family "Ricty"))

(add-to-list 'face-font-rescale-alist
             '(".*Ricty.*" . 1.2))
