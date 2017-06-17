;;; my-nlinum.el
;;; Code:

(require 'nlinum)

(defvar my/global-nlinum-mode nil)

(define-globalized-minor-mode my/global-nlinum-mode nlinum-mode
  (lambda () (unless (or (minibufferp)
                         (member major-mode '(shell-mode eshell-mode))
                         (string-match "*" (buffer-name))
                         (> (buffer-size) 3000000))
               (nlinum-mode))))

(provide 'my-nlinum)
;;; my-nlinum.el ends here
