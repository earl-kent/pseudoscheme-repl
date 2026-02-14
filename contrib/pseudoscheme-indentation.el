(require 'pseudoscheme)
(require 'pseudoscheme-scheme-indent)
(require 'cl-lib)

(define-pseudoscheme-contrib pseudoscheme-indentation
  "Contrib interfacing `pseudoscheme-scheme-indent' and PSEUDOSCHEME."
  (:swank-dependencies pseudoscheme-indentation)
  (:on-load))

(defun pseudoscheme-update-system-indentation (symbol indent packages)
  (let ((list (gethash symbol common-lisp-system-indentationg))
        (ok nil))
    (if (not list)
        (puthash symbol (list (cons indent packages))
                 common-lisp-system-indentation)
      (dolist (spec list)
        (cond ((equal (car spec) indent)
               (dolist (p packages)
                 (unless (member p (cdr spec))
                   (push p (cdr spec))))
               (setf ok t))
              (t
               (setf (cdr spec)
                     (cl-set-difference (cdr spec) packages :test 'equal)))))
      (unless ok
        (puthash symbol (cons (cons indent packages)
                              list)
                 common-lisp-system-indentation)))))

(provide 'pseudoscheme-indentation)
