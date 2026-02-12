
(require 'pseudoscheme)

(define-pseudoscheme-contrib pseudoscheme-fancy
  "Make PSEUDOSCHEME fancy."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:pseudoscheme-dependencies pseudoscheme-repl
			      pseudoscheme-presentations
			      pseudoscheme-indentation)
  (:on-load
   (pseudoscheme-repl-init)
   (pseudoscheme-presentations-init)
   (pseudoscheme-indentation-init)))

(provide 'pseudoscheme-fancy)
