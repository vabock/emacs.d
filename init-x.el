(let ((_fontset (create-fontset-from-ascii-font "Inconsolata-13:weight=normal:slant=normal" nil "inconsolata13"))
      (ricty (font-spec :family "Ricty")))
  (dolist (target '(japanese-jisx0213.2004-1
                    katakana-jisx0201))
    (set-fontset-font _fontset target ricty nil 'append))

  (set-face-font 'default _fontset)
  (add-to-list 'default-frame-alist (cons 'font _fontset)))
