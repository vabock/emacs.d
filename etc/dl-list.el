(with-output-to-temp-buffer "dll-list"
  (dolist (item dynamic-library-alist)
    (princ (format "; %s\n" (car item)))
    (mapc (lambda (x) (princ x) (princ "\n")) (cdr item))))
