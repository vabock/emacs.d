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

(my/build-fontset "Menlo" 14 "Ricty"
                  '(japanese-jisx0213.2004-1
                    katakana-jisx0201))

(add-to-list 'face-font-rescale-alist
             '(".*Ricty.*" . 1.2))
