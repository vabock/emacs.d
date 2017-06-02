(let ((local "/usr/local/bin"))
  (setenv "PATH" (concat local ":" (getenv "PATH")))
  (setq exec-path (cons local exec-path)))

(when (boundp 'window-system)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(hyper z)] 'undo)
  (global-set-key [(hyper u)] 'revert-buffer))

(let ((_fontset (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlo"))
      (ricty (font-spec :family "Ricty")))
  (dolist (target '(japanese-jisx0213.2004-1
                    katakana-jisx0201))
    (set-fontset-font _fontset target ricty nil 'append))

  (set-face-font 'default _fontset)
  (add-to-list 'default-frame-alist (cons 'font _fontset)))

(add-to-list 'face-font-rescale-alist
             '(".*Ricty.*" . 1.2))
