(eval-and-compile
  (require 'pseudoscheme))

(define-pseudoscheme-contrib pseudoscheme-presentation-streams
  "Streams that allow attaching object identities to portions of
   output."
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Helmut Eller  <heller@common-lisp.net>")
  (:license "GPL")
  (:on-load
   (add-hook 'pseudoscheme-connected-hook 'pseudoscheme-presentation-streams-on-connected))
  (:swank-dependencies swank-presentation-streams))

(defun pseudoscheme-presentation-streams-on-connected ()
  (pseudoscheme-eval `(swank:init-presentation-streams)))

(provide 'pseudoscheme-presentation-streams)
