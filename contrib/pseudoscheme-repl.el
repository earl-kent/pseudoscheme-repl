;;; pseudoscheme-repl.el ---
;;
;; Original Author: Helmut Eller
;; Contributors: too many to mention
;; License: GNU GPL (same license as Emacs)
;;
;;; Description:
;;

;;
;;; Installation:
;;
;; Call pseudoscheme-setup and include 'pseudoscheme-repl as argument:
;;
;;  (pseudoscheme-setup '(pseudoscheme-repl [others conribs ...]))
;;
(require 'slime)
(require 'slime-parse)
(require 'cl-lib)

(define-pseudoscheme-contrib pseudoscheme-repl
  "Read-Eval-Print Loop written in Emacs Lisp.

This contrib implements a Lisp Listener along with some niceties like
a persistent history and various \"shortcut\" commands.  Nothing here
depends on comint.el; I/O is multiplexed over PSEUDOSCHEME's socket.

This used to be the default REPL for PSEUDOSCHEME, but it was hard to
maintain."
  (:authors "too many to mention")
  (:license "GPL")
  (:on-load
   (pseudoscheme-repl-add-hooks))
  (:on-unload (pseudoscheme-repl-remove-hooks))
  (:swank-dependencies swank-repl))

;;;;; pseudoscheme-repl

(defgroup pseudoscheme-repl nil
  "The Read-Eval-Print Loop (*pseudoscheme-repl* buffer)."
  :prefix "pseudoscheme-repl-"
  :group 'pseudoscheme)

(defcustom pseudoscheme-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'pseudoscheme-repl)

(defcustom pseudoscheme-repl-only-save-lisp-buffers t
  "When T we only attempt to save lisp-mode file buffers. When
  NIL pseudoscheme will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'pseudoscheme-repl)

(defcustom pseudoscheme-repl-auto-right-margin nil
  "When T we bind CL:*PRINT-RIGHT-MARGIN* to the width of the
current repl's (as per pseudoscheme-output-buffer) window."
  :type '(boolean)
  :group 'pseudoscheme-repl)

(defface pseudoscheme-repl-prompt-face
    '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the PSEUDOSCHEME REPL."
  :group 'pseudoscheme-repl)

(defface pseudoscheme-repl-output-face
    '((t (:inherit font-lock-string-face)))
  "Face for Lisp output in the PSEUDOSCHEME REPL."
  :group 'pseudoscheme-repl)

(defface pseudoscheme-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the PSEUDOSCHEME REPL."
  :group 'pseudoscheme-repl)

(defface pseudoscheme-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the PSEUDOSCHEME REPL."
  :group 'pseudoscheme-repl)

(defcustom pseudoscheme-repl-history-file "~/.pseudoscheme-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'pseudoscheme-repl)

(defcustom pseudoscheme-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'pseudoscheme-repl)

(defcustom pseudoscheme-repl-history-file-coding-system
  (cond ((slime-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t pseudoscheme-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'pseudoscheme-repl)


;; dummy defvar for compiler
(defvar pseudoscheme-repl-read-mode)

(defun pseudoscheme-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (pseudoscheme-output-buffer)
    pseudoscheme-repl-read-mode))


;;;; Stream output

(pseudoscheme-def-connection-var pseudoscheme-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar pseudoscheme-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar pseudoscheme-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy definitions for the compiler
(defvar pseudoscheme-repl-package-stack)
(defvar pseudoscheme-repl-directory-stack)
(defvar pseudoscheme-repl-input-start-mark)
(defvar pseudoscheme-repl-prompt-start-mark)

(defvar pseudoscheme-repl-history-use-mark nil
  "A non-nil value means that history will be replaced from the mark.

Instead of replacing form input-start, look up history and replace input
from the mark. Calling 'pseudoscheme-repl-previous-input',
 'pseudoscheme-repl-previous-matching-input' or their -next counterparts with a prefix
 argument sets this variable for the duration of one history lookup.")

(defun pseudoscheme-repl-history-yank-start ()
  "The position which 'pseudoscheme-repl-previous-input' will replace from.

When 'pseudoscheme-repl-history-use-mark' is non-nil, and (mark) is after the current
input start, return it.  Otherwise, return 'pseudoscheme-repl-input-start-mark'."
  (if (and pseudoscheme-repl-history-use-mark (mark))
      (max (mark) pseudoscheme-repl-input-start-mark)
      pseudoscheme-repl-input-start-mark))


(defun pseudoscheme-output-buffer-alt (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (pseudoscheme-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
	(let ((connection (slime-connection)))
	  (with-current-buffer (pseudoscheme-repl-buffer t connection)
	    (pseudoscheme-repl-insert-prompt))))))


;; Need to improve buffer creation
(defun pseudoscheme-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (pseudoscheme-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (pseudoscheme-connection-output-buffer)
              (let ((connection (slime-connection)))
                (with-current-buffer (pseudoscheme-repl-buffer t connection)
                  (unless (eq major-mode 'pseudoscheme-repl-mode)
                    (pseudoscheme-repl-mode))
                  (setq pseudoscheme-buffer-connection connection)
		  ;; (setq pseudoscheme-buffer-package
		  ;; 	(pseudoscheme-lisp-package connection))
                  (pseudoscheme-reset-repl-markers)
                  (unless noprompt
                    (pseudoscheme-repl-insert-prompt))
                  (current-buffer)))))))

(defvar pseudoscheme-repl-banner-function 'pseudoscheme-repl-insert-banner)

(defun pseudoscheme-repl-update-banner ()
  (funcall pseudoscheme-repl-banner-function)
  (pseudoscheme-move-point (point-max))
  (pseudoscheme-mark-output-start)
  (pseudoscheme-mark-input-start)
  (pseudoscheme-repl-insert-prompt))

(defun pseudoscheme-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; PSEUDOSCHEME " pseudoscheme-version)))
      (insert welcome))))

(defun pseudoscheme-init-output-buffer ()
  (with-current-buffer (pseudoscheme-output-buffer t)
    (setq pseudoscheme-repl-directory-stack '()
          pseudoscheme-repl-package-stack '())
    (pseudoscheme-repl-update-banner)))

(defun pseudoscheme-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (pseudoscheme-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (pseudoscheme-repl-show-maximum-output)))

(defvar pseudoscheme-write-string-function 'pseudoscheme-repl-write-string)

(defun pseudoscheme-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the
hashtable `pseudoscheme-output-target-to-marker'; output is inserted at this marker."
  (funcall pseudoscheme-write-string-function string target))

(defun pseudoscheme-repl-write-string (string &optional target)
  (cl-case target
    ((nil) (pseudoscheme-repl-emit string))
    (:repl-result (pseudoscheme-repl-emit-result string t))
    (t (pseudoscheme-repl-emit-to-target string target))))

(defvar pseudoscheme-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro pseudoscheme-save-marker (marker &rest body)
  (declare (debug (sexp &rest form)))
  (let ((pos (cl-gensym "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'pseudoscheme-save-marker 'lisp-indent-function 1)

(defun pseudoscheme-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (pseudoscheme-output-buffer)
    (save-excursion
      (goto-char pseudoscheme-output-end)
      (pseudoscheme-save-marker pseudoscheme-output-start
        (pseudoscheme-propertize-region '(face pseudoscheme-repl-output-face
                                        pseudoscheme-repl-output t
                                        rear-nonsticky (face))
          (let ((inhibit-read-only t))
	    (insert-before-markers string)
	    (when (and (= (point) pseudoscheme-repl-prompt-start-mark)
		       (not (bolp)))
	      (insert-before-markers "\n")
	      (set-marker pseudoscheme-output-end (1- (point))))))))
    (when pseudoscheme-repl-popup-on-output
      (setq pseudoscheme-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (pseudoscheme-repl-show-maximum-output)))

(defun pseudoscheme-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (pseudoscheme-output-buffer)
    (save-excursion
      (goto-char pseudoscheme-repl-input-start-mark)
      (pseudoscheme-save-marker pseudoscheme-output-start
	(goto-char pseudoscheme-repl-input-start-mark)
	(when (and bol (not (bolp))) (insert-before-markers-and-inherit "\n"))
        (pseudoscheme-save-marker pseudoscheme-output-end
          (pseudoscheme-propertize-region `(face pseudoscheme-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))
        (set-marker pseudoscheme-output-end (point))))
    (pseudoscheme-repl-show-maximum-output)))

(defvar pseudoscheme-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defun pseudoscheme-repl-emit-to-target (string target)
  "Insert STRING at target TARGET.
See `pseudoscheme-output-target-to-marker'."
  (let* ((marker (pseudoscheme-repl-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun pseudoscheme-repl-output-target-marker (target)
  (cl-case target
    ((nil)
     (with-current-buffer (pseudoscheme-output-buffer)
       pseudoscheme-output-end))
    (:repl-result
     (with-current-buffer (pseudoscheme-output-buffer)
       pseudoscheme-repl-input-start-mark))
    (t
     (pseudoscheme-output-target-marker target))))


(defun pseudoscheme-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (pop-to-buffer (pseudoscheme-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;;
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to pseudoscheme-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Lisp, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `pseudoscheme-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt).
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

;; FIXME: pseudoscheme-lisp-package should be local in a REPL buffer
(pseudoscheme-def-connection-var pseudoscheme-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(pseudoscheme-def-connection-var pseudoscheme-lisp-package-prompt-string
    "Ps")

(slime-make-variables-buffer-local
 (defvar pseudoscheme-repl-package-stack nil
   "The stack of packages visited in this repl.")

 (defvar pseudoscheme-repl-directory-stack nil
   "The stack of default directories associated with this repl.")

 (defvar pseudoscheme-repl-prompt-start-mark)
 (defvar pseudoscheme-repl-input-start-mark)
 (defvar pseudoscheme-repl-old-input-counter 0
   "Counter used to generate unique `pseudoscheme-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun pseudoscheme-reset-repl-markers ()
  (dolist (markname '(pseudoscheme-output-start
                      pseudoscheme-output-end
                      pseudoscheme-repl-prompt-start-mark
                      pseudoscheme-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar pseudoscheme-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (copy-keymap lisp-mode-map))
    map))

(pseudoscheme-define-keys pseudoscheme-prefix-map
  ("\C-z" 'pseudoscheme-switch-to-output-buffer)
  ("\M-p" 'pseudoscheme-repl-set-package))

(pseudoscheme-define-keys pseudoscheme-mode-map
  ("\C-c~" 'pseudoscheme-sync-package-and-default-directory)
  ("\C-c\C-y" 'pseudoscheme-call-defun)
  ("\C-c\C-j" 'pseudoscheme-eval-last-expression-in-repl))

(pseudoscheme-define-keys pseudoscheme-connection-list-mode-map
  ((kbd "RET") 'pseudoscheme-goto-connection)
  ([return] 'pseudoscheme-goto-connection))

(pseudoscheme-define-keys pseudoscheme-repl-mode-map
  ("\C-m" 'pseudoscheme-repl-return)
  ([return] 'pseudoscheme-repl-return)
  ("\C-j" 'pseudoscheme-repl-newline-and-indent)
  ("\C-\M-m" 'pseudoscheme-repl-closing-return)
  ([(control return)] 'pseudoscheme-repl-closing-return)
  ("\M-p" 'pseudoscheme-repl-previous-input)
  ((kbd "C-<up>") 'pseudoscheme-repl-backward-input)
  ("\M-n" 'pseudoscheme-repl-next-input)
  ((kbd "C-<down>") 'pseudoscheme-repl-forward-input)
  ;; ("\M-r" 'pseudoscheme-repl-previous-matching-input)
  ("\M-s" 'pseudoscheme-repl-next-matching-input)
  ("\C-c\C-c" 'pseudoscheme-interrupt)
  (" " 'pseudoscheme-space)
  ((string pseudoscheme-repl-shortcut-dispatch-char) 'pseudoscheme-handle-repl-shortcut)
  ("\C-c\C-o" 'pseudoscheme-repl-clear-output)
  ("\C-c\M-o" 'pseudoscheme-repl-clear-buffer)
  ("\C-c\C-u" 'pseudoscheme-repl-kill-input)
  ("\C-c\C-n" 'pseudoscheme-repl-next-prompt)
  ("\C-c\C-p" 'pseudoscheme-repl-previous-prompt)
  ("\C-c\C-z" 'pseudoscheme-nop)
  ("\C-cI" 'pseudoscheme-repl-inspect)
  ("\C-x\C-e" 'pseudoscheme-eval-last-expression))

;; (slime-define-keys pseudoscheme-inspector-mode-map
;;   ((kbd "M-RET") 'pseudoscheme-inspector-copy-down-to-repl))

(pseudoscheme-define-keys sldb-mode-map
  ("\C-y" 'sldb-insert-frame-call-to-repl)
  ((kbd "M-RET") 'sldb-copy-down-to-repl))

(def-pseudoscheme-selector-method ?r
  "PSEUDOSCHEME Read-Eval-Print-Loop."
  (pseudoscheme-output-buffer))

(define-minor-mode pseudoscheme-repl-map-mode
  "Minor mode which makes pseudoscheme-repl-mode-map available.
\\{pseudoscheme-repl-mode-map}"
  :init-value nil
  :lighter nil
  :keymap pseudoscheme-repl-mode-map)


(defun my-pseudoscheme-repl-mode ()
  "Major mode for interacting with a superior Lisp.
\\{pseudoscheme-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pseudoscheme-repl-mode)
  (pseudoscheme-editing-mode 1)
  (pseudoscheme-repl-map-mode 1)
  (lisp-mode-variables t)
  ;; (set (make-local-variable 'lisp-indent-function)
  ;;      'common-lisp-indent-function)
  ;; (pseudoscheme-setup-completion)
  ;; (set (make-local-variable 'tab-always-indent) 'complete)
  ;; (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq pseudoscheme-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  ;; (when pseudoscheme-repl-history-file
  ;;   (pseudoscheme-repl-safe-load-history)
  ;;   (add-hook 'kill-buffer-hook
  ;;             'pseudoscheme-repl-safe-save-merged-history
  ;;             'append t))
  ;; (add-hook 'kill-emacs-hook 'pseudoscheme-repl-save-all-histories)
  ;; ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; ;; the start of the previous prompt or next prompt respectively.
  ;; ;; Notice the interplay with PSEUDOSCHEME-REPL-BEGINNING-OF-DEFUN.
  ;; (set (make-local-variable 'beginning-of-defun-function)
  ;;      'pseudoscheme-repl-mode-beginning-of-defun)
  ;; (set (make-local-variable 'end-of-defun-function)
  ;;      'pseudoscheme-repl-mode-end-of-defun)
  ;; (run-mode-hooks 'pseudoscheme-repl-mode-hook)
  )

(defun pseudoscheme-repl-mode ()
  "Major mode for interacting with a superior Lisp.
\\{pseudoscheme-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pseudoscheme-repl-mode)
  (pseudoscheme-editing-mode 1)
  (pseudoscheme-repl-map-mode 1)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (pseudoscheme-setup-completion)
  (set (make-local-variable 'tab-always-indent) 'complete)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq pseudoscheme-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when pseudoscheme-repl-history-file
    (pseudoscheme-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'pseudoscheme-repl-safe-save-merged-history
              'append t))
  (add-hook 'kill-emacs-hook 'pseudoscheme-repl-save-all-histories)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with PSEUDOSCHEME-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function)
       'pseudoscheme-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'pseudoscheme-repl-mode-end-of-defun)
  (run-mode-hooks 'pseudoscheme-repl-mode-hook))

(defun pseudoscheme-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           "*pseudoscheme-repl*"))

(defun pseudoscheme-repl ()
  (interactive)
  (pseudoscheme-switch-to-output-buffer)
  (current-buffer))

(defun pseudoscheme-repl-mode-beginning-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (pseudoscheme-repl-mode-end-of-defun (- arg))
    (dotimes (i (or arg 1))
      (pseudoscheme-repl-previous-prompt))))

(defun pseudoscheme-repl-mode-end-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (pseudoscheme-repl-mode-beginning-of-defun (- arg))
    (dotimes (i (or arg 1))
      (pseudoscheme-repl-next-prompt))))

(defun pseudoscheme-repl-send-string (string &optional command-string)
  (cond (pseudoscheme-repl-read-mode
         (pseudoscheme-repl-return-string string))
        (t (pseudoscheme-repl-eval-string string))))


;; swank should look something like this:
;; (let nil
;;   (slime-dispatch-event
;;    (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)")
;; 	 (slime-lisp-package) slime-current-thread
;; 	 (lambda (G283)
;; 	   (slime-dcase G283
;; 	     ((:ok result) (slime-repl-insert-result result))
;; 	     ((:abort condition) (slime-repl-show-abort condition)))))))





(defun pseudoscheme-lisp-package ()
  "REVISED^4-SCHEME"
  ;; But also could be SCHEME-TRANSLATION, PS-LISP. At least those
  ;; need to be investigated.
  )


;; (defun my-test-pseudoscheme-repl-eval-string ()
;;   (pseudoscheme-dispatch-event
;;    (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)") "PS"
;; 	 (lambda (G387)
;; 	   (slime-dcase G387
;; 	     ((:ok result)
;; 	      (pseudoscheme-repl-insert-result result))
;; 	     ((:abort condition)
;; 	      (pseudoscheme-repl-show-abort condition)))))))


(defun pseudoscheme-repl-eval-string (string)
  ;; (error "pseudoscheme-repl-eval-string: swank connection not working yet")
  (pseudoscheme-rex ()
      ((if slime-repl-auto-right-margin
           `(swank-repl:listener-eval
	     ,string
	     :window-width
	     ,(with-current-buffer (pseudoscheme-output-buffer)
		(window-width)))
         `(swank-repl:listener-eval ,string))
       (pseudoscheme-lisp-package))
      ((:ok result)
       ;; Obviously needs to be changed once things are working.
       (pseudoscheme-repl-insert-result result))
      ((:abort condition)
       (pseudoscheme-repl-show-abort condition))))

(defun pseudoscheme-repl-insert-result (result)
  (with-current-buffer (pseudoscheme-output-buffer)
    (save-excursion
      (when result
        (pseudoscheme-dcase result
          ((:values &rest strings)
           (cond ((null strings)
                  (pseudoscheme-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (pseudoscheme-repl-emit-result s t)))))))
      (pseudoscheme-repl-insert-prompt))
    (pseudoscheme-repl-show-maximum-output)))

(defun pseudoscheme-repl-show-abort (condition)
  (with-current-buffer (pseudoscheme-output-buffer)
    (save-excursion
      (pseudoscheme-save-marker pseudoscheme-output-start
        (pseudoscheme-save-marker pseudoscheme-output-end
          (goto-char pseudoscheme-output-end)
          (insert-before-markers
           ;; Comment-out multi-line error messages.
           (format "; Evaluation aborted on %s.\n"
                   (replace-regexp-in-string "\n" "\n; " condition)))
          (pseudoscheme-repl-insert-prompt))))
    (pseudoscheme-repl-show-maximum-output)))

(defvar pseudoscheme-repl-suppress-prompt nil
  "Supresses Pseudoscheme REPL prompt when bound to T.")

(defun pseudoscheme-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.
Return the position of the prompt beginning.

If `pseudoscheme-repl-suppress-prompt' is true, does nothing and returns nil."
  (goto-char pseudoscheme-repl-input-start-mark)
  (unless pseudoscheme-repl-suppress-prompt
    (pseudoscheme-save-marker pseudoscheme-output-start
      (pseudoscheme-save-marker pseudoscheme-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (format "%s> " (pseudoscheme-lisp-package-prompt-string))))
          (pseudoscheme-propertize-region
              '(face pseudoscheme-repl-prompt-face
                     read-only t pseudoscheme-repl-prompt t
                     rear-nonsticky t front-sticky (read-only)
                     inhibit-line-move-field-capture t
                     field output)
            (insert-before-markers prompt))
          (set-marker pseudoscheme-repl-prompt-start-mark prompt-start)
          (setq buffer-undo-list nil)
          prompt-start)))))

(defun pseudoscheme-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                 (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defvar pseudoscheme-repl-current-input-hooks)

(defun pseudoscheme-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'pseudoscheme-repl-current-input-hooks
                                        until-point-p)
      (buffer-substring-no-properties (pseudoscheme-repl-history-yank-start)
                                      (if until-point-p
                                          (point)
                                        (point-max)))))

(defun pseudoscheme-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))

(defun pseudoscheme-mark-input-start ()
  (set-marker pseudoscheme-repl-input-start-mark (point) (current-buffer)))

(defun pseudoscheme-mark-output-start ()
  (set-marker pseudoscheme-output-start (point))
  (set-marker pseudoscheme-output-end (point)))

(defun pseudoscheme-mark-output-end ()
  ;; Don't put pseudoscheme-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties pseudoscheme-output-start pseudoscheme-output-end
                       '(;;face pseudoscheme-repl-output-face
                         rear-nonsticky (face))))

(defun pseudoscheme-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun pseudoscheme-repl-in-input-area-p ()
  (<= pseudoscheme-repl-input-start-mark (point)))

(defun pseudoscheme-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) pseudoscheme-repl-input-start-mark))

(defun pseudoscheme-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger PSEUDOSCHEME-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (pseudoscheme-repl-at-prompt-start-p))
           (pseudoscheme-repl-in-input-area-p))
      (goto-char pseudoscheme-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun pseudoscheme-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. PSEUDOSCHEME-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max)))
           (pseudoscheme-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun pseudoscheme-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (pseudoscheme-repl-find-prompt t))

(defun pseudoscheme-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (pseudoscheme-repl-find-prompt))

(defun pseudoscheme-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'pseudoscheme-repl-prompt))
    (while (progn
             (pseudoscheme-search-property-change prop backward)
             (not (or (pseudoscheme-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (pseudoscheme-end-of-proprange-p prop)
      (goto-char origin))))

(defun pseudoscheme-search-property-change (prop &optional backward)
  (cond (backward
         (goto-char (or (previous-single-char-property-change (point) prop)
			(point-min))))
        (t
         (goto-char (or (next-single-char-property-change (point) prop)
			(point-max))))))

(defun pseudoscheme-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar pseudoscheme-repl-return-hooks)

(defun pseudoscheme-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input only if a whole expression has been entered,
i.e. the parenthesis are matched.

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (pseudoscheme-check-connected)
  (cond (end-of-input
         (pseudoscheme-repl-send-input))
        (pseudoscheme-repl-read-mode ; bad style?
         (pseudoscheme-repl-send-input t))
        ((and (get-text-property (point) 'pseudoscheme-repl-old-input)
              (< (point) pseudoscheme-repl-input-start-mark))
         (pseudoscheme-repl-grab-old-input end-of-input)
         (pseudoscheme-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'pseudoscheme-repl-return-hooks end-of-input))
        ((pseudoscheme-input-complete-p pseudoscheme-repl-input-start-mark (point-max))
         (pseudoscheme-repl-send-input t))
        (t
         (pseudoscheme-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun pseudoscheme-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun pseudoscheme-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (pseudoscheme-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (pseudoscheme-repl-add-to-input-history
     (buffer-substring pseudoscheme-repl-input-start-mark end))
    (when newline
      ;; Reset the output columns independently in case they are out of sync.
      (insert "\n")
      (pseudoscheme-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties pseudoscheme-repl-input-start-mark
                           (point)
                           `(pseudoscheme-repl-old-input
                             ,(cl-incf pseudoscheme-repl-old-input-counter))))
    (let ((overlay (make-overlay pseudoscheme-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'face 'pseudoscheme-repl-input-face)))
  (let ((input (pseudoscheme-repl-current-input)))
    (goto-char (point-max))
    (pseudoscheme-mark-input-start)
    (pseudoscheme-mark-output-start)
    (pseudoscheme-repl-send-string input)))

(defun pseudoscheme-repl-grab-old-input (replace)
  "Resend the old REPL input at point.
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `pseudoscheme-repl-old-input'."
  (cl-multiple-value-bind (beg end) (pseudoscheme-property-bounds 'pseudoscheme-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char pseudoscheme-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun pseudoscheme-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region pseudoscheme-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (pseudoscheme-repl-return))

(defun pseudoscheme-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region pseudoscheme-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun pseudoscheme-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region (pseudoscheme-repl-history-yank-start) (point-max)))

(defun pseudoscheme-eval-last-expression-in-repl (prefix)
  "Evaluates last expression in the Pseudoscheme REPL.

Switches REPL to current package of the source buffer for the duration. If
used with a prefix argument (C-u), doesn't switch back afterwards."
  (interactive "P")
  (let ((expr (pseudoscheme-last-expression))
        (buffer-name (buffer-name (current-buffer)))
        (new-package (pseudoscheme-current-package))
        (old-package (pseudoscheme-lisp-package))
        (pseudoscheme-repl-suppress-prompt t)
        (yank-back nil))
    (with-current-buffer (pseudoscheme-output-buffer)
      (unless (eq (current-buffer) (window-buffer))
        (pop-to-buffer (current-buffer) t))
      (goto-char (point-max))
      ;; Kill pending input in the REPL
      (when (< (marker-position pseudoscheme-repl-input-start-mark) (point))
        (kill-region pseudoscheme-repl-input-start-mark (point))
        (setq yank-back t))
      (unwind-protect
          (progn
            (insert-before-markers (format "\n;;; from %s\n" buffer-name))
            (when new-package
              (pseudoscheme-repl-set-package new-package))
            (let ((pseudoscheme-repl-suppress-prompt nil))
              (pseudoscheme-repl-insert-prompt))
            (insert expr)
            (pseudoscheme-repl-return))
        (unless (or prefix (equal (pseudoscheme-lisp-package) old-package))
          ;; Switch back.
          (pseudoscheme-repl-set-package old-package)
          (let ((pseudoscheme-repl-suppress-prompt nil))
            (pseudoscheme-repl-insert-prompt))))
      ;; Put pending input back.
      (when yank-back
        (yank)))))

(defun pseudoscheme-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position pseudoscheme-repl-input-start-mark) (point))
         (kill-region pseudoscheme-repl-input-start-mark (point)))
        ((= (point) (marker-position pseudoscheme-repl-input-start-mark))
         (pseudoscheme-repl-delete-current-input))))

(defun pseudoscheme-repl-replace-input (string)
  (pseudoscheme-repl-delete-current-input)
  (insert-and-inherit string))

(defun pseudoscheme-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char pseudoscheme-repl-input-start-mark)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun pseudoscheme-clear-repl-variables ()
  (interactive)
  (pseudoscheme-eval-async `(swank-repl:clear-repl-variables)))

(defvar pseudoscheme-repl-clear-buffer-hook)

(add-hook 'pseudoscheme-repl-clear-buffer-hook 'pseudoscheme-clear-repl-variables)
(setq pseudoscheme-terminal-output-function 'pseudoscheme-write-string)

(defun pseudoscheme-repl-clear-buffer ()
  "Delete the output generated by the Lisp process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) pseudoscheme-repl-prompt-start-mark)
    (delete-region pseudoscheme-output-start pseudoscheme-output-end)
    (when (< (point) pseudoscheme-repl-input-start-mark)
      (goto-char pseudoscheme-repl-input-start-mark))
    (recenter t))
  (run-hooks 'pseudoscheme-repl-clear-buffer-hook))

(defun pseudoscheme-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion
                 (when (>= (point) pseudoscheme-repl-input-start-mark)
                   (goto-char pseudoscheme-repl-input-start-mark))
                 (pseudoscheme-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (pseudoscheme-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun pseudoscheme-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (pseudoscheme-current-package))
                            (p (and p (pseudoscheme-pretty-package-name p)))
                            (p (and (not (equal p (pseudoscheme-lisp-package))) p)))
                       (pseudoscheme-read-package-name "Package: " p))))
  (with-current-buffer (pseudoscheme-output-buffer)
    (let ((previouse-point (- (point) pseudoscheme-repl-input-start-mark))
          (previous-prompt (pseudoscheme-lisp-package-prompt-string)))
      (cl-destructuring-bind (name prompt-string)
          (pseudoscheme-repl-shortcut-eval `(swank:set-package ,package))
        (setf (pseudoscheme-lisp-package) name)
        ;; (setf pseudoscheme-buffer-package name)
        (unless (equal previous-prompt prompt-string)
          (setf (pseudoscheme-lisp-package-prompt-string) prompt-string)
          (pseudoscheme-repl-insert-prompt))
        (when (cl-plusp previouse-point)
          (goto-char (+ previouse-point pseudoscheme-repl-input-start-mark)))))))


;;;;; History

(defcustom pseudoscheme-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'pseudoscheme-repl)

(make-variable-buffer-local
 (defvar pseudoscheme-repl-input-history '()
   "History list of strings read from the REPL buffer."))

(defun pseudoscheme-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (setq string (pseudoscheme-trim-whitespace string))
  (unless (equal string "")
    (setq pseudoscheme-repl-input-history
          (remove string pseudoscheme-repl-input-history))
    (unless (equal string (car pseudoscheme-repl-input-history))
      (push string pseudoscheme-repl-input-history))))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'pseudoscheme-repl-history-replace,
;; otherwise we reinitialize them.

(defvar pseudoscheme-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar pseudoscheme-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun pseudoscheme-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq pseudoscheme-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length pseudoscheme-repl-input-history))
         (pos0 (cond ((pseudoscheme-repl-history-search-in-progress-p)
                      pseudoscheme-repl-input-history-position)
                     (t min-pos)))
         (pos (pseudoscheme-repl-position-in-history pos0 direction (or regexp "")
                                              (pseudoscheme-repl-current-input)))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (pseudoscheme-repl-replace-input (nth pos pseudoscheme-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not pseudoscheme-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (pseudoscheme-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq pseudoscheme-repl-input-history-position pos)
    (setq this-command 'pseudoscheme-repl-history-replace)))

(defun pseudoscheme-repl-history-search-in-progress-p ()
  (eq last-command 'pseudoscheme-repl-history-replace))

(defun pseudoscheme-repl-terminate-history-search ()
  (setq last-command this-command))

(defun pseudoscheme-repl-position-in-history (start-pos direction regexp
                                                 &optional exclude-string)
  "Return the position of the history item matching REGEXP.
Return -1 resp. the length of the history if no item matches.
If EXCLUDE-STRING is specified then it's excluded from the search."
  ;; Loop through the history list looking for a matching line
  (let* ((step (cl-ecase direction
                 (forward -1)
                 (backward 1)))
         (history pseudoscheme-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             for history-item = (nth pos history)
             if (and (string-match regexp history-item)
                     (not (equal history-item exclude-string)))
             return pos)))

(defun pseudoscheme-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern.

With a prefix-arg, do replacement from the mark."
  (interactive)
  (let ((pseudoscheme-repl-history-use-mark (or pseudoscheme-repl-history-use-mark
                                         current-prefix-arg)))
    (pseudoscheme-repl-history-replace 'backward (pseudoscheme-repl-history-pattern t))))

(defun pseudoscheme-repl-next-input ()
  "Cycle forwards through input history.
See `pseudoscheme-repl-previous-input'.

With a prefix-arg, do replacement from the mark."
  (interactive)
  (let ((pseudoscheme-repl-history-use-mark (or pseudoscheme-repl-history-use-mark
                                         current-prefix-arg)))
    (pseudoscheme-repl-history-replace 'forward (pseudoscheme-repl-history-pattern t))))

(defun pseudoscheme-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (pseudoscheme-repl-history-replace 'forward (pseudoscheme-repl-history-pattern)))

(defun pseudoscheme-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (pseudoscheme-repl-history-replace 'backward (pseudoscheme-repl-history-pattern)))

(defun pseudoscheme-repl-previous-matching-input (regexp)
  "Insert the previous matching input.

With a prefix-arg, do the insertion at the mark."
  (interactive (list (pseudoscheme-read-from-minibuffer
		      "Previous element matching (regexp): ")))
  (pseudoscheme-repl-terminate-history-search)
  (let ((pseudoscheme-repl-history-use-mark (or pseudoscheme-repl-history-use-mark
                                         current-prefix-arg)))
    (pseudoscheme-repl-history-replace 'backward regexp)))

(defun pseudoscheme-repl-next-matching-input (regexp)
  "Insert the next matching input.

With a prefix-arg, do the insertion at the mark."
  (interactive (list (pseudoscheme-read-from-minibuffer
		      "Next element matching (regexp): ")))
  (pseudoscheme-repl-terminate-history-search)
  (let ((pseudoscheme-repl-history-use-mark (or pseudoscheme-repl-history-use-mark
                                         current-prefix-arg)))
   (pseudoscheme-repl-history-replace 'forward regexp)))

(defun pseudoscheme-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((pseudoscheme-repl-history-search-in-progress-p)
         pseudoscheme-repl-history-pattern)
        (use-current-input
         (goto-char (max (pseudoscheme-repl-history-yank-start) (point)))
         (let ((str (pseudoscheme-repl-current-input t)))
           (cond ((string-match "^[ \t\n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun pseudoscheme-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history.

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (pseudoscheme-repl-current-input)))
  (let ((merged-history
         (pseudoscheme-repl-merge-histories (pseudoscheme-repl-read-history nil t)
                                     pseudoscheme-repl-input-history)))
    (setq pseudoscheme-repl-input-history
          (cl-delete string merged-history :test #'string=))
    (pseudoscheme-repl-save-history))
  (pseudoscheme-repl-delete-current-input))

;;;;; Persistent History

(defun pseudoscheme-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (cl-remove-if test new-hist)
            (cl-remove-if test old-hist))))

(defun pseudoscheme-repl-load-history (&optional filename)
  "Set the current PSEUDOSCHEME REPL history.
It can be read either from FILENAME or `pseudoscheme-repl-history-file' or
from a user defined filename."
  (interactive (list (pseudoscheme-repl-read-history-filename)))
  (let ((file (or filename pseudoscheme-repl-history-file)))
    (setq pseudoscheme-repl-input-history (pseudoscheme-repl-read-history file t))))

(defun pseudoscheme-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.
The default value for FILENAME is `pseudoscheme-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename pseudoscheme-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun pseudoscheme-repl-read-history-filename ()
  (read-file-name "Use PSEUDOSCHEME REPL history from file: "
                  pseudoscheme-repl-history-file))

(defun pseudoscheme-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`pseudoscheme-repl-merge-histories'."
  (interactive (list (pseudoscheme-repl-read-history-filename)))
  (let ((file (or filename pseudoscheme-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (pseudoscheme-repl-merge-histories (pseudoscheme-repl-read-history file t)
                                              pseudoscheme-repl-input-history)))
        (pseudoscheme-repl-save-history file hist)))))

(defun pseudoscheme-repl-save-history (&optional filename history)
  "Simply save the current PSEUDOSCHEME REPL history to a file.
When PSEUDOSCHEME is setup to always load the old history and one uses only
one instance of pseudoscheme all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `pseudoscheme-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (pseudoscheme-repl-read-history-filename)))
  (let ((file (or filename pseudoscheme-repl-history-file))
        (hist (or history pseudoscheme-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (cl-subseq hist 0 (min (length hist) pseudoscheme-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs pseudoscheme-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for PSEUDOSCHEME REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun pseudoscheme-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'pseudoscheme-repl-mode)
        (pseudoscheme-repl-safe-save-merged-history)))))

(defun pseudoscheme-repl-safe-save-merged-history ()
  (pseudoscheme-repl-call-with-handler
   #'pseudoscheme-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun pseudoscheme-repl-safe-load-history ()
  (pseudoscheme-repl-call-with-handler
   #'pseudoscheme-repl-load-history
   "%S while loading the history. Continue? "))

(defun pseudoscheme-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(defvar pseudoscheme-repl-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'pseudoscheme-repl-return)
    (define-key map [return] 'pseudoscheme-repl-return)
    (define-key map (kbd "TAB") 'self-insert-command)
    (define-key map "\C-c\C-b" 'pseudoscheme-repl-read-break)
    (define-key map "\C-c\C-c" 'pseudoscheme-repl-read-break)
    (define-key map [remap pseudoscheme-indent-and-complete-symbol] 'ignore)
    (define-key map [remap pseudoscheme-handle-repl-shortcut] 'self-insert-command)
    map))

(define-minor-mode pseudoscheme-repl-read-mode
  "Mode to read input from Emacs
\\{pseudoscheme-repl-read-mode-map}"
  :init-value nil
  :lighter "[read]")

(make-variable-buffer-local
 (defvar pseudoscheme-read-string-threads nil))

(make-variable-buffer-local
 (defvar pseudoscheme-read-string-tags nil))

(defun pseudoscheme-repl-read-string (thread tag)
  (pseudoscheme-switch-to-output-buffer)
  (push thread pseudoscheme-read-string-threads)
  (push tag pseudoscheme-read-string-tags)
  (goto-char (point-max))
  (pseudoscheme-mark-output-end)
  (pseudoscheme-mark-input-start)
  (pseudoscheme-repl-read-mode 1))

(defun pseudoscheme-repl-return-string (string)
  (pseudoscheme-dispatch-event `(:emacs-return-string
                          ,(pop pseudoscheme-read-string-threads)
                          ,(pop pseudoscheme-read-string-tags)
                          ,string))
  (pseudoscheme-repl-read-mode -1))

(defun pseudoscheme-repl-read-break ()
  (interactive)
  (pseudoscheme-dispatch-event `(:emacs-interrupt ,(car pseudoscheme-read-string-threads))))

(defun pseudoscheme-repl-abort-read (thread tag)
  (with-current-buffer (pseudoscheme-output-buffer)
    (pop pseudoscheme-read-string-threads)
    (pop pseudoscheme-read-string-tags)
    (pseudoscheme-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(cl-defstruct (pseudoscheme-repl-shortcut (:conc-name pseudoscheme-repl-shortcut.))
  symbol names handler one-liner)

(defvar pseudoscheme-repl-shortcut-table nil
  "A list of pseudoscheme-repl-shortcuts")

(defvar pseudoscheme-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar pseudoscheme-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun pseudoscheme-handle-repl-shortcut ()
  (interactive)
  (if (> (point) pseudoscheme-repl-input-start-mark)
      (insert (string pseudoscheme-repl-shortcut-dispatch-char))
    (let ((shortcut (pseudoscheme-lookup-shortcut
                     (completing-read "Command: "
                                      (pseudoscheme-bogus-completion-alist
                                       (pseudoscheme-list-all-repl-shortcuts))
                                      nil t nil
                                      'pseudoscheme-repl-shortcut-history))))
      (with-struct (pseudoscheme-repl-shortcut. handler) shortcut
        (let ((pseudoscheme-within-repl-shortcut-handler-p t))
          (call-interactively handler))))))

(defun pseudoscheme-list-all-repl-shortcuts ()
  (cl-loop for shortcut in pseudoscheme-repl-shortcut-table
           append (pseudoscheme-repl-shortcut.names shortcut)))

(defun pseudoscheme-lookup-shortcut (name)
  (cl-find-if (lambda (s) (member name (pseudoscheme-repl-shortcut.names s)))
              pseudoscheme-repl-shortcut-table))

(defmacro defpseudoscheme-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created.

NAMES is a list of \(full-name . aliases\).

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(cl-second (assoc :handler options)))))
     (let ((new-shortcut (make-pseudoscheme-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq pseudoscheme-repl-shortcut-table
             (cl-remove-if (lambda (s)
                             (member ',(car names) (pseudoscheme-repl-shortcut.names s)))
                           pseudoscheme-repl-shortcut-table))
       (push new-shortcut pseudoscheme-repl-shortcut-table)
       ',elisp-name)))

(defun pseudoscheme-repl-shortcut-eval (sexp &optional package)
  "This function should be used by REPL shortcut handlers instead
of `pseudoscheme-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when pseudoscheme-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (pseudoscheme-repl-add-to-input-history (prin1-to-string sexp)))
  (pseudoscheme-eval sexp package))

(defun pseudoscheme-repl-shortcut-eval-async (sexp &optional cont package)
  "This function should be used by REPL shortcut handlers instead
of `pseudoscheme-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when pseudoscheme-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (pseudoscheme-repl-add-to-input-history (prin1-to-string sexp)))
  (pseudoscheme-eval-async sexp cont package))

(defun pseudoscheme-list-repl-short-cuts ()
  (interactive)
  (pseudoscheme-with-popup-buffer ((pseudoscheme-buffer-name :repl-help))
    (let ((table (cl-sort (cl-copy-list pseudoscheme-repl-shortcut-table) #'string<
                          :key (lambda (x)
                                 (car (pseudoscheme-repl-shortcut.names x))))))
      (save-excursion
        (dolist (shortcut table)
          (let ((names (pseudoscheme-repl-shortcut.names shortcut)))
            (insert (pop names)) ;; first print the "full" name
            (when names
              ;; we also have aliases
              (insert " (aka ")
              (while (cdr names)
                (insert (pop names) ", "))
              (insert (car names) ")"))
            (when (pseudoscheme-repl-shortcut.one-liner shortcut)
              (insert "\n     " (pseudoscheme-repl-shortcut.one-liner shortcut)))
            (insert "\n")))))))

(defun pseudoscheme-save-some-lisp-buffers ()
  (if pseudoscheme-repl-only-save-lisp-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode pseudoscheme-lisp-modes)
                                    (not (null buffer-file-name)))))
    (save-some-buffers)))

(defun pseudoscheme-kill-all-buffers ()
  "Kill all the PSEUDOSCHEME-related buffers."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) pseudoscheme-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match "^\\*pseudoscheme-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf))
              (string-match "^\\*PSEUDOSCHEME.*\\*$" (buffer-name buf)))
      (kill-buffer buf))))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-shortcut-help ("help")
  (:handler 'pseudoscheme-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defpseudoscheme-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'pseudoscheme-set-default-directory)
  (:one-liner "Change the current directory."))

(defpseudoscheme-repl-shortcut nil ("pwd")
  (:handler (lambda ()
              (interactive)
              (let ((dir (pseudoscheme-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-push-directory
  ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (pseudoscheme-eval '(swank:default-directory))
                      nil nil "")))
              (push (pseudoscheme-eval '(swank:default-directory))
                    pseudoscheme-repl-directory-stack)
              (pseudoscheme-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-pop-directory
  ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null pseudoscheme-repl-directory-stack)
                  (message "Directory stack is empty.")
                (pseudoscheme-set-default-directory
                 (pop pseudoscheme-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defpseudoscheme-repl-shortcut nil ("change-package" "!p" "in-package" "in")
  (:handler 'pseudoscheme-repl-set-package)
  (:one-liner "Change the current package."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (pseudoscheme-read-package-name "Package: ")))
              (push (pseudoscheme-lisp-package) pseudoscheme-repl-package-stack)
              (pseudoscheme-repl-set-package package)))
  (:one-liner "Save the current package and set it to a new one."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null pseudoscheme-repl-package-stack)
                  (message "Package stack is empty.")
                (pseudoscheme-repl-set-package
                 (pop pseudoscheme-repl-package-stack)))))
  (:one-liner "Restore the last saved package."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car pseudoscheme-repl-input-history))
              (insert "\n")
              (pseudoscheme-repl-send-input)))
  (:one-liner "Resend the last form."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-disconnect ("disconnect")
  (:handler 'pseudoscheme-disconnect)
  (:one-liner "Disconnect the current connection."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-disconnect-all ("disconnect-all")
  (:handler 'pseudoscheme-disconnect-all)
  (:one-liner "Disconnect all connections."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (pseudoscheme-connected-p)
                (pseudoscheme-quit-lisp))
              (pseudoscheme-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all PSEUDOSCHEME buffers."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-quit ("quit")
  (:handler (lambda ()
	      (interactive)
              ;; `pseudoscheme-quit-lisp' determines the connection to quit
              ;; on behalf of the REPL's `pseudoscheme-buffer-connection'.
              (let ((repl-buffer (pseudoscheme-output-buffer)))
                (pseudoscheme-quit-lisp)
                (kill-buffer repl-buffer))))
  (:one-liner "Quit the current Lisp."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (pseudoscheme-read-symbol-name "Name (symbol): " t)
                                 (pseudoscheme-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value
                      " \"REPL generated global variable.\")")
              (pseudoscheme-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defpseudoscheme-repl-shortcut pseudoscheme-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (pseudoscheme-save-some-lisp-buffers)
              (pseudoscheme-repl-shortcut-eval-async
               `(swank:compile-file-if-needed
                 ,(pseudoscheme-to-lisp-filename filename) t)
               #'pseudoscheme-compilation-finished)))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defpseudoscheme-repl-shortcut nil  ("restart-inferior-lisp")
  (:handler 'pseudoscheme-restart-inferior-lisp)
  (:one-liner "Restart *inferior-lisp* and reconnect PSEUDOSCHEME."))

(defun pseudoscheme-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create (pseudoscheme-buffer-name :trace))))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (cl-incf pseudoscheme-last-output-target-id)))
        (puthash target marker pseudoscheme-output-target-to-marker)
        (pseudoscheme-eval `(swank-repl:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; pseudoscheme-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun pseudoscheme-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (cl-labels ((insert-call
               (name &key (function t)
                     defclass)
               (let* ((setf (and function
                                 (consp name)
                                 (= (length name) 2)
                                 (eql (car name) 'setf)))
                      (symbol (if setf
                                  (cadr name)
                                name))
                      (qualified-symbol-name
                       (pseudoscheme-qualify-cl-symbol-name symbol))
                      (symbol-name (pseudoscheme-cl-symbol-name qualified-symbol-name))
                      (symbol-package (pseudoscheme-cl-symbol-package
                                       qualified-symbol-name))
                      (call (if (cl-equalp (pseudoscheme-lisp-package) symbol-package)
                                symbol-name
                              qualified-symbol-name)))
                 (pseudoscheme-switch-to-output-buffer)
                 (goto-char pseudoscheme-repl-input-start-mark)
                 (insert (if function
                             "("
                           " "))
                 (when setf
                   (insert "setf ("))
                 (if defclass
                     (insert "make-instance '"))
                 (insert call)
                 (cond (setf
                        (insert " ")
                        (save-excursion (insert ") )")))
                       (function
                        (insert " ")
                        (save-excursion (insert ")"))))
                 (unless function
                   (goto-char pseudoscheme-repl-input-start-mark)))))
    (let ((toplevel (pseudoscheme-parse-toplevel-form '(:defun :defgeneric :defmacro :define-compiler-macro
                                                 :defmethod :defparameter :defvar :defconstant :defclass))))
      (if (symbolp toplevel)
          (error "Not in a function definition")
        (pseudoscheme-dcase toplevel
          (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
           (insert-call symbol))
          ((:defmethod symbol &rest args)
           (declare (ignore args))
           (insert-call symbol))
          (((:defparameter :defvar :defconstant) symbol)
           (insert-call symbol :function nil))
          (((:defclass) symbol)
           (insert-call symbol :defclass t))
          (t
           (error "Not in a function definition")))))))

(defun pseudoscheme-repl-copy-down-to-repl (pseudoschemefun &rest args)
  (pseudoscheme-eval-async `(swank-repl:listener-save-value ',pseudoschemefun ,@args)
    #'(lambda (_ignored)
        (with-current-buffer (pseudoscheme-repl)
          (pseudoscheme-eval-async '(swank-repl:listener-get-value)
            #'(lambda (_ignored)
                (pseudoscheme-repl-insert-prompt)))))))

(defun pseudoscheme-inspector-copy-down-to-repl (number)
  "Evaluate the inspector slot at point via the REPL (to set `*')."
  (interactive (list (or (get-text-property (point) 'pseudoscheme-part-number)
                         (error "No part at point"))))
  (pseudoscheme-repl-copy-down-to-repl 'swank:inspector-nth-part number))

(defun sldb-copy-down-to-repl (frame-id var-id)
  "Evaluate the frame var at point via the REPL (to set `*')."
  (interactive (list (sldb-frame-number-at-point) (sldb-var-number-at-point)))
  (pseudoscheme-repl-copy-down-to-repl 'swank/backend:frame-var-value frame-id var-id))

(defun sldb-insert-frame-call-to-repl ()
  "Insert a call to a frame at point."
  (interactive)
  (let ((call (pseudoscheme-eval `(swank/backend::frame-call
                            ,(sldb-frame-number-at-point)))))
    (pseudoscheme-switch-to-output-buffer)
    (if (>= (point) pseudoscheme-repl-prompt-start-mark)
        (insert call)
      (save-excursion
        (goto-char (point-max))
        (insert call))))
  (pseudoscheme-repl))

(defun pseudoscheme-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (pseudoscheme-from-lisp-filename
              (pseudoscheme-repl-shortcut-eval `(swank:set-default-directory
                                          ,(pseudoscheme-to-lisp-filename dir)))))
    (with-current-buffer (pseudoscheme-output-buffer)
      (setq default-directory dir))))

(defun pseudoscheme-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let* ((package (pseudoscheme-current-package))
         (exists-p (or (null package)
                       (pseudoscheme-eval `(cl:packagep
                                     (swank::guess-package ,package)))))
         (directory default-directory))
    (when (and package exists-p)
      (pseudoscheme-repl-set-package package))
    (pseudoscheme-set-default-directory directory)
    ;; Sync *inferior-lisp* dir
    (let* ((proc (pseudoscheme-process))
           (buffer (and proc (process-buffer proc))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "package: %s%s  directory: %s"
             (with-current-buffer (pseudoscheme-output-buffer)
               (pseudoscheme-lisp-package))
             (if exists-p "" (format " (package %s doesn't exist)" package))
             directory)))

(defun pseudoscheme-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((pseudoscheme-dispatching-connection (pseudoscheme-connection-at-point)))
    (switch-to-buffer (pseudoscheme-output-buffer))))

(defun pseudoscheme-repl-inside-string-or-comment-p ()
  (save-restriction
    (when (and (boundp 'pseudoscheme-repl-input-start-mark)
               pseudoscheme-repl-input-start-mark
               (>= (point) pseudoscheme-repl-input-start-mark))
      (narrow-to-region pseudoscheme-repl-input-start-mark (point)))
    (pseudoscheme-inside-string-or-comment-p)))

(defvar pseudoscheme-repl-easy-menu
  (let ((C '(pseudoscheme-connected-p)))
    `("REPL"
      [ "Send Input"             pseudoscheme-repl-return ,C ]
      [ "Close and Send Input "  pseudoscheme-repl-closing-return ,C ]
      [ "Interrupt Lisp process" pseudoscheme-interrupt ,C ]
      "--"
      [ "Previous Input"         pseudoscheme-repl-previous-input t ]
      [ "Next Input"             pseudoscheme-repl-next-input t ]
      [ "Goto Previous Prompt "  pseudoscheme-repl-previous-prompt t ]
      [ "Goto Next Prompt "      pseudoscheme-repl-next-prompt t ]
      [ "Clear Last Output"      pseudoscheme-repl-clear-output t ]
      [ "Clear Buffer "          pseudoscheme-repl-clear-buffer t ]
      [ "Kill Current Input"     pseudoscheme-repl-kill-input t ])))

(defun pseudoscheme-repl-add-easy-menu ()
  (easy-menu-define menubar-pseudoscheme-repl pseudoscheme-repl-mode-map
    "REPL" pseudoscheme-repl-easy-menu)
  (easy-menu-define menubar-pseudoscheme pseudoscheme-repl-mode-map
    "PSEUDOSCHEME" pseudoscheme-easy-menu)
  (easy-menu-add pseudoscheme-repl-easy-menu 'pseudoscheme-repl-mode-map))

(add-hook 'pseudoscheme-repl-mode-hook 'pseudoscheme-repl-add-easy-menu)

(defun pseudoscheme-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (pseudoscheme-process)
                     (process-buffer (pseudoscheme-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (pseudoscheme-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

(defun pseudoscheme-repl-choose-coding-system ()
  (let ((candidates (pseudoscheme-connection-coding-systems)))
    (or (cl-find (symbol-name (car default-process-coding-system))
                 candidates
                 :test (lambda (s1 s2)
                         (if (fboundp 'coding-system-equal)
                             (coding-system-equal (intern s1) (intern s2)))))
	(car candidates)
	(error "Can't find suitable coding-system"))))

;; Unlike slime we call this function directly from the emacs user
;; function: pseudoscheme.
(defun pseudoscheme-repl-connected-hook-function ()
  ;; (cl-destructuring-bind (package prompt)
  ;;     (let ((pseudoscheme-current-thread t))
  ;; 	(pseudoscheme-eval `(swank-repl:create-repl nil)))
  ;;   (setf (pseudoscheme-lisp-package) package)
  ;;   (setf (pseudoscheme-lisp-package-prompt-string) prompt))
  ;; (pseudoscheme-hide-inferior-lisp-buffer)
  (pseudoscheme-init-output-buffer))

(defun pseudoscheme-repl-event-hook-function (event)
  (pseudoscheme-dcase event
    ((:write-string output &optional target thread)
     (pseudoscheme-write-string output target)
     (when thread
       (pseudoscheme-send `(:write-done ,thread)))
     t)
    ((:read-string thread tag)
     (cl-assert thread)
     (pseudoscheme-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (pseudoscheme-repl-abort-read thread tag)
     t)
    ((:new-package package prompt-string)
     (setf (pseudoscheme-lisp-package) package)
     (setf (pseudoscheme-lisp-package-prompt-string) prompt-string)
     ;; (let ((buffer (pseudoscheme-connection-output-buffer)))
     ;;   (when (buffer-live-p buffer)
     ;; 	 (with-current-buffer buffer
     ;; 	   (setq pseudoscheme-buffer-package package))))
     t)
    (t nil)))

(defun pseudoscheme-change-repl-to-default-connection ()
  "Change current REPL to the REPL of the default connection.
If the current buffer is not a REPL, don't do anything."
  (when (equal major-mode 'pseudoscheme-repl-mode)
    (let ((pseudoscheme-buffer-connection pseudoscheme-default-connection))
      (pop-to-buffer-same-window (pseudoscheme-connection-output-buffer)))))

;; - pseudoscheme-repl-find-buffer-package

(defun pseudoscheme-repl-add-hooks ()
  (add-hook 'pseudoscheme-event-hooks 'pseudoscheme-repl-event-hook-function)
  ;; This kicks off the repl, since we piggyback on slime, we call it
  ;; directly.
  ;; (add-hook 'pseudoscheme-connected-hook
  ;; 	    'pseudoscheme-repl-connected-hook-function)
  (add-hook 'pseudoscheme-cycle-connections-hook
            'pseudoscheme-change-repl-to-default-connection))

(defun pseudoscheme-repl-remove-hooks ()
  (remove-hook 'pseudoscheme-event-hooks 'pseudoscheme-repl-event-hook-function)
  (remove-hook 'pseudoscheme-connected-hook 'pseudoscheme-repl-connected-hook-function)
  (remove-hook 'pseudoscheme-cycle-connections-hook
               'pseudoscheme-change-repl-to-default-connection))

(defun pseudoscheme-repl-sexp-at-point ()
  "Returns the current sexp at point (or NIL if none is found)
while ignoring the repl prompt text."
  (if (<= pseudoscheme-repl-input-start-mark (point))
      (save-restriction
        (narrow-to-region pseudoscheme-repl-input-start-mark (point-max))
        (pseudoscheme-sexp-at-point))
    (pseudoscheme-sexp-at-point)))

(defun pseudoscheme-repl-inspect (string)
  (interactive
   (list (pseudoscheme-read-from-minibuffer "Inspect value (evaluated): "
                                     (pseudoscheme-repl-sexp-at-point))))
  (pseudoscheme-inspect string))

(require 'bytecomp)

;; (mapc (lambda (sym)
;;         (cond ((fboundp sym)
;;                (unless (byte-code-function-p (symbol-function sym))
;;                  (byte-compile sym)))
;;               (t (error "%S is not fbound" sym))))
;;       '(pseudoscheme-repl-event-hook-function
;;         pseudoscheme-write-string
;;         pseudoscheme-repl-write-string
;;         pseudoscheme-repl-emit
;;         pseudoscheme-repl-show-maximum-output))

(provide 'pseudoscheme-repl)
