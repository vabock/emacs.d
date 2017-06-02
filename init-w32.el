(setq grep-use-null-device nil)

(let ((cygwin "c:/cygwin64/bin"))
  (setq exec-path (cons cygwin exec-path))
  (setenv "PATH" (concat cygwin ";" (getenv "PATH"))))

(setenv "SHELL" "sh")
(setq shell-file-name "sh")
(setq shell-command-switch "-c")

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'| sed -e 's/:/\\n/g' | cygpath -m -f - | tr '\\n' ';'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; フォント設定
(setq w32-use-w32-font-dialog nil)
(setq-default line-spacing 1)

(let ((_fontset (create-fontset-from-ascii-font "Consolas-12:weight=normal:slant=normal" nil "consolas12"))
      (msgothic (font-spec :family "ＭＳ ゴシック" :registry "unicode-bmp" :lang 'ja)))
  (dolist (target '(japanese-jisx0212
                    japanese-jisx0213-2
                    japanese-jisx0213.2004-1
                    katakana-jisx0201))
    (set-fontset-font _fontset target msgothic nil 'append))

  (set-face-font 'default _fontset)
  (add-to-list 'default-frame-alist (cons 'font _fontset)))

;;; IME の設定
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]") ;; おこのみで
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")) ;; おこのみで
(w32-ime-initialize)
(global-set-key (kbd "M-<kanji>") (lambda () (interactive) t))

;; start-process での起動時に、fakecygpty.exe を経由させたいプログラム名を列挙する
;; suffix に .exe が付くコマンドは、その suffix を記載しないこと
(setq fakecygpty-program-list '("sh" "bash" "zsh" "ssh" "scp" "rsync" "sftp" "irb" "psql" "sqlite3"))

;; start-process での起動時に、fakecygpty.exe を経由させたくないプロセスが走るバッファ名を列挙する
(setq fakecygpty-exclusion-buffer-name-list '("*grep*"))

;; fakecygpty-program-list に登録されているプログラムを fakecygpty.exe 経由で起動する
(defadvice start-process (around ad-start-process-to-fake last activate)
  (let ((buffer-name (if (bufferp (ad-get-arg 1))
                         (buffer-name (ad-get-arg 1))
                       (ad-get-arg 1))))
    (if (and (member (replace-regexp-in-string "\\.exe$" "" (file-name-nondirectory (ad-get-arg 2)))
                     fakecygpty-program-list)
             (not (member buffer-name fakecygpty-exclusion-buffer-name-list)))
        (progn
          (ad-set-args 3 (cons (ad-get-arg 2) (ad-get-args 3)))
          (ad-set-arg 2 "fakecygpty")
          ad-do-it)
      ad-do-it)))

;;; Local Variables:
;;; coding: utf-8-unix
;;; mode: emacs-lisp
;;; End:
