;;; Code:


(require 'cl-lib)

(eval-and-compile
  (if (< emacs-major-version 23)
      (error "Pseudoscheme requires an Emacs version of 23, or above")))

(require 'hyperspec "lib/hyperspec")
(require 'thingatpt)
(require 'comint)
(require 'pp)
(require 'easymenu)
(require 'outline)
(require 'arc-mode)
(require 'etags)
(require 'xref nil t)
(require 'compile)
(require 'gv)

(eval-and-compile
 (require 'apropos))

(eval-when-compile
  (require 'gud)
  (require 'lisp-mnt))

(declare-function lm-version "lisp-mnt")

(defvar pseudoscheme-path nil
  "Directory containing the Pseudoscheme package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of
the Emacs Lisp package.")
(setq pseudoscheme-path (file-name-directory load-file-name))

(defvar pseudoscheme-version nil
  "The version of PSEUDOSCHEME that you're using.")
(setq pseudoscheme-version
      (eval-when-compile
       (lm-version
        (cl-find "pseudoscheme.el"
                 (remove nil
                         (list load-file-name
                               (when (boundp 'byte-compile-current-file)
                                 byte-compile-current-file)))
                 :key #'file-name-nondirectory
                 :test #'string-equal))))

(defvar pseudoscheme-scheme-modes '(scheme-mode))
(defvar pseudoscheme-lisp-modes '(scheme-mode))
(defvar pseudoscheme-contribs '(pseudoscheme-fancy)
  "A list of contrib packages to load with PSEUDOSCHEME.")
(define-obsolete-variable-alias 'pseudoscheme-setup-contribs
'pseudoscheme-contribs "0.0.1")


;;;###autoload
(cl-defun pseudoscheme-setup (&optional (contribs nil contribs-p))
  "Setup Emacs so that scheme-mode buffers always use PSEUDOSCHEME.
CONTRIBS is a list of contrib packages to load. If `nil', use
`pseudoscheme-contribs'. "
  (interactive)
  (when (member 'scheme-mode pseudoscheme-scheme-modes)
    (add-hook 'scheme-mode-hook 'pseudoscheme-scheme-mode-hook))
  (when contribs-p
    (setq pseudoscheme-contribs contribs))
  (pseudoscheme--setup-contribs))

(defvar pseudoscheme-required-modules '())

(defun pseudoscheme--setup-contribs ()
  "Load and initialize contribs."
  (dolist (c pseudoscheme-contribs)
    (unless (featurep c)
      (require c)
      (let ((init (intern (format "%s-init" c))))
        (when (fboundp init)
          (funcall init))))))

(defun pseudoscheme-lisp-mode-hook ()
  (pseudoscheme-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))

(defvar pseudoscheme-protocol-version nil)
(setq pseudoscheme-protocol-version pseudoscheme-version)



;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
(defgroup pseudoscheme-mode nil
  "Settings for pseudoscheme-mode Lisp source buffers."
  :prefix "pseudoscheme-"
  :group 'pseudoscheme)

;; -----------------------------------------------------------------------------





;; -----------------------------------------------------------------------------

(defun pseudoscheme-scheme-mode-hook ()
  (pseudoscheme-mode 1)
  (set (make-local-variable 'scheme-indent-function)
       ;; see ./contrib/slime-cl-indent.el for how to do this.
       'master-scheme-indent-function))

;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------

;; Interface
(defmacro pseudoscheme-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))


;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------

;;;; Starting SLIME
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defvar inferior-scheme-program "lisp"
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

;; ----------------------------------------------------------------------------




;; ----------------------------------------------------------------------------

;;;###autoload
(defun pseudoscheme (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (pseudoscheme-setup)
  (pseudoscheme-start*))

;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------


;; The slime start up sequence somewhat complex -- it overlays the
;; standard emacs process for initiating a mode with the specific
;; needs of establinging network communication with the remote cl
;; session you're connecting. Below is the call sequence.
;;
;; Note the reason for slime --> slime-start* --> slime-start are
;; inscrutable.
;;
;; - slime (autoload)
;; - slime-start*
;; - slime-start
;; - slime-maybe-start-lisp
;; - slime-inferior-connect
;; - slime-read-port-and-connect
;; - slime-setup-connection
;; - slime-init-connection-state (send swank:connection-info)
;;
;; then we retrieve the message
;;
;; - event: slime-net-filter
;; - slime-process-available-input
;; - slime-dispatch-event
;; - slime-set-connection-info (async callback)
;; - run-hooks
;; - slime-repl-connected-hook-function
;;   (synchronous: (swank-repl:create-repl nil))
;; - slime-hide-inferior-lisp-buffer
;; - slime-output-buffer
;;
;; Most of this machinery is not needed for the Pseudoscheme REPL
;; since piggy-backs on top of slime.

;; This should probably be called directly from slime (autoload).
(cl-defun pseudoscheme-start ()
  "We assume that slime has been started and the pseudoscheme
environment alreadyd loaded."
    (when (pseudoscheme-bytecode-stale-p)
      (pseudoscheme-urge-bytecode-recompile))
    (slime-output-buffer))

(defun pseudoscheme-start* ()
  (apply #'pseudoscheme-start))


;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------

;;; Starting the inferior Lisp and loading Swank:


;; We rely on slime for the following:
;; - slime-maybe-start-lisp
;; - slime-reinitialize-inferior-lisp-p
;; - slime-inferior-process-start-hook
;; - slime-inferior-lisp-connected
;; - slime-terminal-output-function
;; - slime-start-lisp
;; - pseudoscheme-inferior-connect

;; We rely on slime for for the following
;; - slime-inferior-lisp-args
;; - slime-start-swank-server
;; - slime-inferior-lisp-args
;; - slime-init-command
;; - slime-swank-port-file
;; - slime-temp-directory
;; - slime-delete-swank-port-file
;; - slime-read-port-and-connect


(defun pseudoscheme-attempt-connection ()
  (let ((current-connection (slime-current-connection)))
    (unless current-connection
      (error "No current connection."))
    (slime-inferior-process (slime-current-connection))))


;; We rely on slame for practically all connection stuff.
;; - slime-timer-call
;; - slime-cancel-connect-retry-timer
;; - slime-read-swank-port
;; - slime-toggle-debug-on-swank-error
;; - slime-user-first-name
;; - slime-words-of-encouragement
;; - slime-random-words-of-encouragement

;;;; Networking
;; - slime-net-processes
;; - slime-net-process-close-hooks
;; - slime-secret
;; - slime-send-secret
;; - slime-net-connect
;; - slime-make-net-buffer
;; - slime-set-query-on-exit-flag

;;;;; Coding system madness
;; - slime-check-coding-system
;; - slime-coding-system-mulibyte-p
;; - slime-coding-system-cl-name
;; - slime-net-send
;; - slime-safe-encoding-p
;; - slime-net-sentinel
;; - slime-net-filter
;; - slime-process-available-input
;; - slime-net-have-input-p
;; - slime-run-when-idle
;; - slime-handle-net-read-error
;; - slime-net-read-or-lose
;; - slime-net-read
;; - slime-net-decode-length
;; - slime-net-encode-length

;;;; Connections
;; - slime-dispatching-connection
;; - slime-default-connection
;; - slime-current-connection
;; - slime-connection
;; - slime-auto-start
;; - slime-auto-start
;; - slime-auto-select-connection
;; - slime-auto-select-connection
;; - slime-select-connection
;; - slime-cycle-connections-hook
;; - slime-cycle-connections-within
;; - slime-next-connection
;; - slime-prev-connection
;; - slime-with-connection-buffer

;;; Connection-local variables:
;; - slime-def-connection-var
;; - slime-connection-number
;; - slime-lisp-features
;; - slime-lisp-modules
;; - slime-pid
;; - slime-lisp-implementation-type
;; - slime-lisp-implementation-version
;; - slime-lisp-implementation-name
;; - slime-lisp-implementation-program
;; - slime-connection-name
;; - slime-inferior-process
;; - slime-communication-style
;; - slime-machine-instance
;; - slime-connection-coding-systems

;;;;; Connection setup
;; - slime-connection-counter
;; - slime-setup-connection
;; - slime-init-connection-state
;; - slime-insert-inferior-lisp-output
;; - slime-set-connection-info
;; - slime-net-close
;; - slime-check-version
;; - slime-generate-connection-name
;; - slime-connection-close-hook

;;;;; Commands on connections
;; - slime-disconnect
;; - slime-disconnect-all
;; - slime-connection-port
;; - slime-process
;; - slime-set-inferior-process
;; - slime-use-sigint-for-interrupt
;; - slime-inhibit-pipelining
;; - slime-background-activities-enabled-p


;; -----------------------------------------------------------------------------





;; -----------------------------------------------------------------------------


;;;; Minor modes

;;;;; pseudoscheme-mode

(defvar pseudoscheme-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `pseudoscheme-mode-map' as it's parent.
This is a hack so that we can reinitilize the real pseudoscheme-mode-map
more easily. See `pseudoscheme-init-keymaps'.")

(defvar pseudoscheme-buffer-connection)
(defvar pseudoscheme-current-thread)

(defun pseudoscheme--on ()
  (pseudoscheme-setup-completion))

(defun pseudoscheme--off ()
  (remove-hook 'completion-at-point-functions #'pseudoscheme--completion-at-point t))

;;;###autoload
(define-minor-mode pseudoscheme-mode
  "\\<pseudoscheme-mode-map>\
PSEUDOSCHEME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[pseudoscheme-compile-and-load-file]	- Compile and load the current buffer's file.
\\[pseudoscheme-compile-file]	- Compile (but not load) the current buffer's file.
\\[pseudoscheme-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[pseudoscheme-next-note]	- Goto the next form with a compiler note.
\\[pseudoscheme-previous-note]	- Goto the previous form with a compiler note.
\\[pseudoscheme-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[pseudoscheme-edit-definition]
- Edit the definition of the function called at point.
\\[pseudoscheme-pop-find-definition-stack]
- Pop the definition stack to go back from a definition.

Documentation commands:
\\[pseudoscheme-describe-symbol]	- Describe symbol.
\\[pseudoscheme-apropos]	- Apropos search.
\\[pseudoscheme-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[pseudoscheme-eval-defun]	- Evaluate top-level from containing point.
\\[pseudoscheme-eval-last-expression]	- Evaluate sexp before point.
\\[pseudoscheme-pprint-eval-last-expression]	\
- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{pseudoscheme-mode-map}"
  :keymap pseudoscheme-mode-indirect-map
  :lighter (:eval (pseudoscheme-modeline-string))
  (cond (pseudoscheme-mode (pseudoscheme--on))
        (t (pseudoscheme--off))))

;; -----------------------------------------------------------------------------


;; -----------------------------------------------------------------------------





;;;; Minor modes

;;;;; pseudoscheme-mode

(defvar pseudoscheme-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `pseudoscheme-mode-map' as it's parent.
This is a hack so that we can reinitilize the real pseudoscheme-mode-map
more easily. See `pseudoscheme-init-keymaps'.")

(defvar pseudoscheme-buffer-connection)
(defvar pseudoscheme-current-thread)

(defun pseudoscheme--on ()
  (pseudoscheme-setup-completion))

(defun pseudoscheme--off ()
  (remove-hook 'completion-at-point-functions #'pseudoscheme--completion-at-point t))

;;;###autoload
(define-minor-mode pseudoscheme-mode
  "\\<pseudoscheme-mode-map>\
PSEUDOSCHEME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[pseudoscheme-compile-and-load-file]	- Compile and load the current buffer's file.
\\[pseudoscheme-compile-file]	- Compile (but not load) the current buffer's file.
\\[pseudoscheme-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[pseudoscheme-next-note]	- Goto the next form with a compiler note.
\\[pseudoscheme-previous-note]	- Goto the previous form with a compiler note.
\\[pseudoscheme-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[pseudoscheme-edit-definition]
- Edit the definition of the function called at point.
\\[pseudoscheme-pop-find-definition-stack]
- Pop the definition stack to go back from a definition.

Documentation commands:
\\[pseudoscheme-describe-symbol]	- Describe symbol.
\\[pseudoscheme-apropos]	- Apropos search.
\\[pseudoscheme-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[pseudoscheme-eval-defun]	- Evaluate top-level from containing point.
\\[pseudoscheme-eval-last-expression]	- Evaluate sexp before point.
\\[pseudoscheme-pprint-eval-last-expression]	\
- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{pseudoscheme-mode-map}"
  :keymap pseudoscheme-mode-indirect-map
  :lighter (:eval (pseudoscheme-modeline-string))
  (cond (pseudoscheme-mode (pseudoscheme--on))
        (t (pseudoscheme--off))))


;;;;;; Modeline

(defun pseudoscheme-modeline-string ()
  "Return the string to display in the modeline.
\"Pseudoscheme\" only appears if we aren't connected.  If connected,
include package-name, connection-name, and possibly some state
information."
  (let ((conn (pseudoscheme-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `pseudoscheme-connection' which may query the user.
    (if (not conn)
        (and pseudoscheme-mode " Pseudoscheme")
      (let ((local (eq conn pseudoscheme-buffer-connection))
            (pkg   (pseudoscheme-current-package)))
        (concat " "
                (if local "{" "[")
                (if pkg (replace-regexp-in-string "%" "%%" (pseudoscheme-pretty-package-name pkg)) "?")
                " "
                ;; ignore errors for closed connections
                (ignore-errors (pseudoscheme-connection-name conn))
                (pseudoscheme-modeline-state-string conn)
                (if local "}" "]"))))))

(defun pseudoscheme-pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (cond ((string-match "^#?:\\(.*\\)$" name)
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name)
         (match-string 1 name))
        (t name)))

(defun pseudoscheme-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
         (format " %s" (process-status conn)))
        ((let ((pending (length (pseudoscheme-rex-continuations conn)))
               (sldbs (length (sldb-buffers conn))))
           (cond ((and (zerop sldbs) (zerop pending)) nil)
                 ((zerop sldbs) (format " %s" pending))
                 (t (format " %s/%s" pending sldbs)))))))

(defun pseudoscheme--recompute-modelines ()
  (force-mode-line-update t))


;;;;; Key bindings

(defvar pseudoscheme-parent-map nil
  "Parent keymap for shared between all Pseudoscheme related modes.")

(defvar pseudoscheme-parent-bindings
  '(("\M-."      pseudoscheme-edit-definition)
    ("\M-,"      pseudoscheme-pop-find-definition-stack)
    ("\M-_"      pseudoscheme-edit-uses)    ; for German layout
    ("\M-?"      pseudoscheme-edit-uses)    ; for USian layout
    ("\C-x4."    pseudoscheme-edit-definition-other-window)
    ("\C-x5."    pseudoscheme-edit-definition-other-frame)
    ("\C-x\C-e"  pseudoscheme-eval-last-expression)
    ("\C-\M-x"   pseudoscheme-eval-defun)
    ;; Include PREFIX keys...
    ("\C-c"	 pseudoscheme-prefix-map)))

(defvar pseudoscheme-prefix-map nil
  "Keymap for commands prefixed with `pseudoscheme-prefix-key'.")

(defvar pseudoscheme-prefix-bindings
  '(("\C-r"  pseudoscheme-eval-region)
    (":"     pseudoscheme-interactive-eval)
    ("\C-e"  pseudoscheme-interactive-eval)
    ("E"     pseudoscheme-edit-value)
    ("\C-l"  pseudoscheme-load-file)
    ("\C-b"  pseudoscheme-interrupt)
    ("\M-d"  pseudoscheme-disassemble-symbol)
    ("\C-t"  pseudoscheme-toggle-trace-fdefinition)
    ("I"     pseudoscheme-inspect)
    ("\C-xt" pseudoscheme-list-threads)
    ("\C-xn" pseudoscheme-next-connection)
    ("\C-xp" pseudoscheme-prev-connection)
    ("\C-xc" pseudoscheme-list-connections)
    ("<"     pseudoscheme-list-callers)
    (">"     pseudoscheme-list-callees)
    ;; Include DOC keys...
    ("\C-d"  pseudoscheme-doc-map)
    ;; Include XREF WHO-FOO keys...
    ("\C-w"  pseudoscheme-who-map)
    ))

(defvar pseudoscheme-editing-map nil
  "These keys are useful for buffers where the user can insert and
edit s-exprs, e.g. for source buffers and the REPL.")

(defvar pseudoscheme-editing-keys
  `(;; Arglist display & completion
    (" "          pseudoscheme-space)
    ;; Evaluating
    ;;("\C-x\M-e" pseudoscheme-eval-last-expression-display-output :inferior t)
    ("\C-c\C-p"   pseudoscheme-pprint-eval-last-expression)
    ;; Macroexpand
    ("\C-c\C-m"   pseudoscheme-expand-1)
    ("\C-c\M-m"   pseudoscheme-macroexpand-all)
    ;; Misc
    ("\C-c\C-u"   pseudoscheme-undefine-function)
    (,(kbd "C-M-.")   pseudoscheme-next-location)
    (,(kbd "C-M-,")   pseudoscheme-previous-location)
    ;; Obsolete, redundant bindings
    ("\C-c\C-i" completion-at-point)
    ;;("\M-*" pop-tag-mark) ; almost to clever
    ))

(defvar pseudoscheme-mode-map nil
  "Keymap for pseudoscheme-mode.")

(defvar pseudoscheme-keys
  '( ;; Compiler notes
    ("\M-p"       pseudoscheme-previous-note)
    ("\M-n"       pseudoscheme-next-note)
    ("\C-c\M-c"   pseudoscheme-remove-notes)
    ("\C-c\C-k"   pseudoscheme-compile-and-load-file)
    ("\C-c\M-k"   pseudoscheme-compile-file)
    ("\C-c\C-c"   pseudoscheme-compile-defun)))

(defun pseudoscheme-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar pseudoscheme-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar pseudoscheme-doc-bindings
  '((?a pseudoscheme-apropos)
    (?z pseudoscheme-apropos-all)
    (?p pseudoscheme-apropos-package)
    (?d pseudoscheme-describe-symbol)
    (?f pseudoscheme-describe-function)
    (?h pseudoscheme-documentation-lookup)
    (?~ common-lisp-hyperspec-format)
    (?g common-lisp-hyperspec-glossary-term)
    (?# common-lisp-hyperspec-lookup-reader-macro)))

(defvar pseudoscheme-who-map nil
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar pseudoscheme-who-bindings
  '((?c pseudoscheme-who-calls)
    (?w pseudoscheme-calls-who)
    (?r pseudoscheme-who-references)
    (?b pseudoscheme-who-binds)
    (?s pseudoscheme-who-sets)
    (?m pseudoscheme-who-macroexpands)
    (?a pseudoscheme-who-specializes)))

(defun pseudoscheme-init-keymaps ()
  "(Re)initialize the keymaps for `pseudoscheme-mode'."
  (interactive)
  (pseudoscheme-init-keymap 'pseudoscheme-doc-map t t pseudoscheme-doc-bindings)
  (pseudoscheme-init-keymap 'pseudoscheme-who-map t t pseudoscheme-who-bindings)
  (pseudoscheme-init-keymap 'pseudoscheme-prefix-map t nil pseudoscheme-prefix-bindings)
  (pseudoscheme-init-keymap 'pseudoscheme-parent-map nil nil pseudoscheme-parent-bindings)
  (pseudoscheme-init-keymap 'pseudoscheme-editing-map nil nil pseudoscheme-editing-keys)
  (set-keymap-parent pseudoscheme-editing-map pseudoscheme-parent-map)
  (pseudoscheme-init-keymap 'pseudoscheme-mode-map nil nil pseudoscheme-keys)
  (set-keymap-parent pseudoscheme-mode-map pseudoscheme-editing-map)
  (set-keymap-parent pseudoscheme-mode-indirect-map pseudoscheme-mode-map))

(defun pseudoscheme-init-keymap (keymap-name prefixp bothp bindings)
  (set keymap-name (make-sparse-keymap))
  (when prefixp (define-prefix-command keymap-name))
  (pseudoscheme-bind-keys (eval keymap-name) bothp bindings))

(defun pseudoscheme-bind-keys (keymap bothp bindings)
  "Add BINDINGS to KEYMAP.
If BOTHP is true also add bindings with control modifier."
  (cl-loop for (key command) in bindings do
           (cond (bothp
                  (define-key keymap `[,key] command)
                  (unless (equal key ?h)     ; But don't bind C-h
                    (define-key keymap `[(control ,key)] command)))
                 (t (define-key keymap key command)))))

(pseudoscheme-init-keymaps)

(define-minor-mode pseudoscheme-editing-mode
  "Minor mode which makes pseudoscheme-editing-map available.
\\{pseudoscheme-editing-map}"
  :init-value nil
  :lighter nil
  :keymap pseudoscheme-editing-map)



;;;; Framework'ey bits
;;;
;;; This section contains some standard SLIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro pseudoscheme-dcase (value &rest patterns)
  (declare (indent 1))
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (cl-gensym "op-"))
	(operands (cl-gensym "rand-"))
	(tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (cl-case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (cl-destructuring-bind ((op &rest rands) &rest body)
                           clause
                         `(,op (cl-destructuring-bind ,rands ,operands
                                 . ,(or body
                                        '((ignore)) ; suppress some warnings
                                        ))))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "slime-dcase failed: %S" ,tmp))))))))

(defmacro pseudoscheme-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  (declare (indent 1))
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

;; -  with-struct

;;;;; Very-commonly-used functions

(defvar pseudoscheme-message-function 'message)

;; Interface
(defun pseudoscheme-buffer-name (type &optional hidden)
  (cl-assert (keywordp type))
  (concat (if hidden " " "")
          (format "*pseudoscheme-%s*" (substring (symbol-name type) 1))))

;; Interface
(defun pseudoscheme-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply pseudoscheme-message-function format args))

(defun pseudoscheme-display-warning (message &rest args)
  (display-warning '(pseudoscheme warning) (apply #'format message args)))

(defvar pseudoscheme-background-message-function 'pseudoscheme-display-oneliner)

;; Interface
(defun pseudoscheme-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `pseudoscheme-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply pseudoscheme-background-message-function format-string format-args))

(defun pseudoscheme-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (pseudoscheme-oneliner msg)))))

(defun pseudoscheme-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (window-width (minibuffer-window))))))

;; Interface
(defun pseudoscheme-set-truncate-lines ()
  "Apply `pseudoscheme-truncate-lines' to the current buffer."
  (when pseudoscheme-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun pseudoscheme-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (pseudoscheme-bogus-completion-alist
                             (pseudoscheme-eval
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

;; Interface
(defun pseudoscheme-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (pseudoscheme-symbol-at-point)))
         (pseudoscheme-read-from-minibuffer prompt (pseudoscheme-symbol-at-point)))
        (t (pseudoscheme-symbol-at-point))))

;; Interface
(defmacro pseudoscheme-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun pseudoscheme-add-face (face string)
  (declare (indent 1))
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

;; Interface
(defsubst pseudoscheme-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (pseudoscheme-propertize-region props (apply #'insert args)))

(defmacro pseudoscheme-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (declare (indent 1))
  (let ((start (cl-gensym)) (l (cl-gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (pseudoscheme-indent-rigidly ,start (point) ,l)))))

(defun pseudoscheme-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun pseudoscheme-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (pseudoscheme-with-rigid-indentation nil
    (apply #'insert strings)))

(defun pseudoscheme-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (cl-assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun pseudoscheme-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun pseudoscheme-rcurry (fun &rest args)
  "Like `pseudoscheme-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))

;; ---------------------------------------------------------------------


(cl-defmacro pseudoscheme-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  (declare (indent 1))
  `(with-current-buffer
       (process-buffer (or ,process (slime-connection)
                           (error "No connection")))
     ,@body))

;;; Connection-local variables:

(defmacro pseudoscheme-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `pseudoscheme-connection'."
  (declare (indent 2))
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (pseudoscheme-with-connection-buffer (process) ,real-var))
       ;; Setf
       (gv-define-setter ,varname (store &optional process)
         `(pseudoscheme-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))))
       '(\, varname))))

;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------





;;;; Pseudoscheme Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; Pseudoscheme's interface is considerably different, but for the
;;; moment it's worth preserving this text from slime.
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`slime-eval-async') for
;;; most things. Reserve synchronous evaluations (`slime-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `slime-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (cl-first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `slime-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar pseudoscheme-current-thread t
   "The id of the current thread on the Lisp side.
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

;; - pseudoscheme-buffer-package

;;; `slime-rex' is the RPC primitive which is used to implement both
;;; `slime-eval' and `slime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(cl-defmacro pseudoscheme-rex ((&rest saved-vars)
                        (sexp &optional
                              (thread 'slime-current-thread))
                        &rest continuations)
  "(slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (slime-current-package).

CLAUSES is a list of patterns with same syntax as
`slime-dcase'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 2))
  (let ((result (cl-gensym)))
    `(let ,(cl-loop for var in saved-vars
                    collect (cl-etypecase var
                              (symbol (list var var))
                              (cons var)))
       (slime-dispatch-event
        (list :emacs-rex ,sexp "PS" ,thread
              (lambda (,result)
                (slime-dcase ,result
                  ,@continuations)))))))

;;; Interface
(defun pseudoscheme-current-package ()
  "The CL package that contains the pseudoscheme reader"
  "PS")

;; -pseudoscheme-find-buffer-package-function
;; -pseudoscheme-find-buffer-package
;; - pseudoscheme-package-cache
;; - pseudoscheme-search-buffer-package

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar pseudoscheme-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun pseudoscheme-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (pseudoscheme-current-package)))
  (let* ((tag (cl-gensym (format "pseudoscheme-result-%d-"
                                 (1+ (pseudoscheme-continuation-counter)))))
	 (pseudoscheme-stack-eval-tags (cons tag pseudoscheme-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (pseudoscheme-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag pseudoscheme-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort _condition)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (pseudoscheme-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (accept-process-output nil 0.01)))))))

(defun pseudoscheme-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (declare (indent 1))
  (pseudoscheme-rex (cont (buffer (current-buffer)))
      (sexp (or package (pseudoscheme-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; pseudoscheme-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :pseudoscheme-eval-async)

;;; These functions can be handy too:

(defun pseudoscheme-connected-p ()
  "Return true if the Swank connection is open."
  ;; What we really want to do here is check whether lucid-script.lisp
  ;; has been properly loaded in *slime-repl sbcl*
  t
  ;; (not (null pseudoscheme-net-processes))
  )

(defun pseudoscheme-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (pseudoscheme-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[pseudoscheme]"))))


;; UNUSED
(defun pseudoscheme-debugged-connection-p (conn)
  ;; This previously was (AND (SLDB-DEBUGGED-CONTINUATIONS CONN) T),
  ;; but an SLDB buffer may exist without having continuations
  ;; attached to it, e.g. the one resulting from `pseudoscheme-interrupt'.
  (cl-loop for b in (sldb-buffers)
           thereis (with-current-buffer b
                     (eq pseudoscheme-buffer-connection conn))))

(defun pseudoscheme-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (pseudoscheme-connection)))))
    (cl-remove-if (lambda (id)
                    (memq id debugged))
                  (pseudoscheme-rex-continuations)
                  :key #'car)))

(defun pseudoscheme-sync ()
  "Block until the most recent request has finished."
  (when (pseudoscheme-rex-continuations)
    (let ((tag (caar (pseudoscheme-rex-continuations))))
      (while (cl-find tag (pseudoscheme-rex-continuations) :key #'car)
        (accept-process-output nil 0.1)))))

(defun pseudoscheme-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (pseudoscheme-eval "PONG")))

;;;;; Protocol event handler (cl-the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(pseudoscheme-def-connection-var pseudoscheme-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(pseudoscheme-def-connection-var pseudoscheme-continuation-counter 0
  "Continuation serial number counter.")

(defvar pseudoscheme-event-hooks)

(defun pseudoscheme-dispatch-event (event &optional process)
  ;; (let ((pseudoscheme-dispatching-connection (or process (pseudoscheme-connection))))
    (or (run-hook-with-args-until-success 'pseudoscheme-event-hooks event)
        (pseudoscheme-dcase event
          ((:emacs-rex form package thread continuation)
           ;; try do use the same discipline as slime
           (when (and (slime-use-sigint-for-interrupt) (slime-busy-p))
             (pseudoscheme-display-oneliner "; pipelined request... %S" form))
           (let ((id (cl-incf (pseudoscheme-continuation-counter))))
             (pseudoscheme-send `(:emacs-rex ,form ,package ,thread ,id))
             (push (cons id continuation) (pseudoscheme-rex-continuations))
             (pseudoscheme--recompute-modelines)))
          ((:return value id)
           (let ((rec (assq id (pseudoscheme-rex-continuations))))
             (cond (rec (setf (pseudoscheme-rex-continuations)
                              (remove rec (pseudoscheme-rex-continuations)))
                        (pseudoscheme--recompute-modelines)
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (cl-assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (cl-assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (cl-assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (pseudoscheme-send `(:emacs-interrupt ,thread)))
          ((:channel-send id msg)
           (pseudoscheme-channel-send (or (pseudoscheme-find-channel id)
                                   (error "Invalid channel id: %S %S" id msg))
                               msg))
          ((:emacs-channel-send id msg)
           (pseudoscheme-send `(:emacs-channel-send ,id ,msg)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (pseudoscheme-read-from-minibuffer-for-swank thread tag prompt
                                                 initial-value))
          ((:y-or-n-p thread tag question)
           (pseudoscheme-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (pseudoscheme-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (pseudoscheme-lisp-features) features))
          ((:indentation-update info)
           (pseudoscheme-handle-indentation-update info))
          ((:eval-no-wait form)
           (pseudoscheme-check-eval-in-emacs-enabled)
           (eval (read form)))
          ((:eval thread tag form-string)
           (pseudoscheme-check-eval-in-emacs-enabled)
           (pseudoscheme-eval-for-lisp thread tag form-string))
          ((:ed-rpc-no-wait fn-name &rest args)
           (let ((fn (intern-soft fn-name)))
             (pseudoscheme-check-rpc-allowed fn)
             (apply fn args)))
          ((:ed-rpc thread tag fn-name &rest args)
           (pseudoscheme-rpc-from-lisp thread tag (intern-soft fn-name) args))
          ((:emacs-return thread tag value)
           (pseudoscheme-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (pseudoscheme-ed what))
          ((:inspect what thread tag)
           (let ((hook (when (and thread tag)
                         (pseudoscheme-curry #'pseudoscheme-send
                                      `(:emacs-return ,thread ,tag nil)))))
             (pseudoscheme-open-inspector what nil hook)))
          ((:background-message message)
           (pseudoscheme-background-message "%s" message))
          ((:debug-condition thread message)
           (cl-assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (pseudoscheme-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (pseudoscheme-with-popup-buffer ((pseudoscheme-buffer-name :error))
             (princ (format "Invalid protocol message:\n%s\n\n%s"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (pseudoscheme-rex-continuations)
                 (cl-remove id (pseudoscheme-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))
          ((:emacs-skipped-packet _pkg))
          ((:test-delay seconds) ; for testing only
           (sit-for seconds)))))

(defun pseudoscheme-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (slime-net-send sexp (slime-connection)))

(defun pseudoscheme-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (slime-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers))
  (slime-with-connection-buffer ()
    (erase-buffer)))

;; - slime-send-sigint





;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------



;;;; Arglist Display

(defun pseudoscheme-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (slime-echo-arglist))

(put 'pseudoscheme-space 'delete-selection t) ; for delete-section-mode & CUA

(defun pseudoscheme-echo-arglist ()
  ;; we rely on slime for the underlying connection. This detects
  ;; whether there's a background activity happening.
  (when (slime-background-activities-enabled-p)
    (let ((op (slime-operator-before-point)))
      (when op
        (slime-eval-async `(swank:operator-arglist ,op
                                                   ,(slime-current-package))
          (lambda (arglist)
            (when arglist
              (slime-message "%s" arglist))))))))

(defvar pseudoscheme-operator-before-point-function 'pseudoscheme-lisp-operator-before-point)

(defun slime-operator-before-point ()
  (funcall pseudoscheme-operator-before-point-function))

(defun pseudoscheme-lisp-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slime-symbol-at-point))))

;;;; Completion

;; FIXME: use this in Emacs 24
;;(define-obsolete-function-alias slime-complete-symbol completion-at-point)

(defalias 'pseudoscheme-complete-symbol #'completion-at-point)

;; This is the function that we add to
;; `completion-at-point-functions'.  For backward-compatibilty we look
;; at `slime-complete-symbol-function' first.  The indirection through
;; `slime-completion-at-point-functions' is used so that users don't
;; have to set `completion-at-point-functions' in every slime-like
;; buffer.
(defun pseudoscheme--completion-at-point ()
  (cond (pseudoscheme-complete-symbol-function
         pseudoscheme-complete-symbol-function)
        (t
         (run-hook-with-args-until-success
          'pseudoscheme-completion-at-point-functions))))

(defun pseudoscheme-setup-completion ()
  (add-hook 'completion-at-point-functions #'pseudoscheme--completion-at-point nil t))

(defun pseudoscheme-simple-completion-at-point ()
  "Complete the symbol at point.
Perform completion similar to `elisp-completion-at-point'."
  (let* ((end (point))
         (beg (pseudoscheme-symbol-start-pos)))
    (list beg end (completion-table-dynamic #'pseudoscheme-simple-completions))))

;; Rely on slime rather than a pseudoscheme implementation
;; slime-filename-completion

;; FIXME: for backward compatibility.  Remove it one day
;; together with slime-complete-symbol-function.
(defun pseudoscheme-simple-complete-symbol ()
  (let ((completion-at-point-functions '(pseudoscheme-maybe-complete-as-filename
                                         pseudoscheme-simple-completion-at-point)))
    (completion-at-point)))

;; NOTE: the original idea was to bind this to TAB but that no longer
;; works as `completion-at-point' sets a transient keymap that
;; overrides TAB.  So this is rather useless now.
(defun pseudoscheme-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'pseudoscheme-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (completion-at-point))
            ((memq (char-before) '(?\t ?\ ))
             (pseudoscheme-echo-arglist))))))


;; Usep slime version of function
;; - slime-minibuffer-map

(defvar pseudoscheme-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defun pseudoscheme-minibuffer-setup-hook ()
  (cons (let ((package (slime-current-package))
              (connection (slime-connection)))
          (lambda ()
            (setq slime-buffer-package package)
            (setq slime-buffer-connection connection)
            (set-syntax-table lisp-mode-syntax-table)
            (slime-setup-completion)))
        minibuffer-setup-hook))

(defun pseudoscheme-read-from-minibuffer (prompt &optional initial-value history)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook (slime-minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-minibuffer-map ;; use slime version
			  nil (or history 'pseudoscheme-minibuffer-history))))
;; Use slime's version
;; - slime-bogus-completion-alist

(defun pseudoscheme-simple-completions (prefix)
  (cl-destructuring-bind (completions _partial)
      (let ((pseudoscheme-current-thread t))
        (slime-eval
	 (error "slime-simple-completions not implemented yet.")
	 ;; this will need additional work
         ;; `(swank:simple-completions ,(substring-no-properties prefix)
         ;;                            ',(slime-current-package))
	 ))
    completions))

;; -----------------------------------------------------------------------------



















;; -----------------------------------------------------------------------------





;;;;; Connection listing

(defun pseudoscheme-move-point (position)
  "Move point in the current buffer and in the window the buffer is displayed."
  (let ((window (get-buffer-window (current-buffer) t)))
    (goto-char position)
    (when window
      (set-window-point window position))))

(defun pseudoscheme-display-threads (threads)
  (with-current-buffer pseudoscheme-threads-buffer-name
    (let* ((inhibit-read-only t)
           (old-thread-id (get-text-property (point) 'thread-id))
           (old-line (line-number-at-pos))
           (old-column (current-column)))
      (erase-buffer)
      (pseudoscheme-insert-threads threads)
      (let ((new-position (cl-position old-thread-id (cdr threads)
                                       :key #'car :test #'equal)))
        (goto-char (point-min))
        (forward-line (or new-position (1- old-line)))
        (move-to-column old-column)
        (pseudoscheme-move-point (point))))))

(defun pseudoscheme-transpose-lists (list-of-lists)
  (let ((ncols (length (car list-of-lists))))
    (cl-loop for col-index below ncols
             collect (cl-loop for row in list-of-lists
                              collect (elt row col-index)))))

(defun pseudoscheme-insert-table-row (line line-props col-props col-widths)
  (pseudoscheme-propertize-region line-props
    (cl-loop for string in line
             for col-prop in col-props
             for width in col-widths do
             (pseudoscheme-insert-propertized col-prop string)
             (insert-char ?\ (- width (length string))))))

(defun pseudoscheme-insert-table (rows header row-properties column-properties)
  "Insert a \"table\" so that the columns are nicely aligned."
  (let* ((ncols (length header))
         (lines (cons header rows))
         (widths (cl-loop for columns in (pseudoscheme-transpose-lists lines)
                          collect (1+ (cl-loop for cell in columns
                                               maximize (length cell)))))
         (header-line (with-temp-buffer
                        (pseudoscheme-insert-table-row
                         header nil (make-list ncols nil) widths)
                        (buffer-string))))
    (cond ((boundp 'header-line-format)
           (setq header-line-format header-line))
          (t (insert header-line "\n")))
    (cl-loop for line in rows  for line-props in row-properties do
             (pseudoscheme-insert-table-row line line-props column-properties widths)
             (insert "\n"))))

(defvar pseudoscheme-threads-table-properties
  '(nil (face bold)))

(defun pseudoscheme-insert-threads (threads)
  (let* ((labels (car threads))
         (threads (cdr threads))
         (header (cl-loop for label in labels collect
                          (capitalize (substring (symbol-name label) 1))))
         (rows (cl-loop for thread in threads collect
                        (cl-loop for prop in thread collect
                                 (format "%s" prop))))
         (line-props (cl-loop for (id) in threads for i from 0
                              collect `(thread-index ,i thread-id ,id)))
         (col-props (cl-loop for nil in labels for i from 0 collect
                             (nth i pseudoscheme-threads-table-properties))))
    (pseudoscheme-insert-table rows header line-props col-props)))


;;;;; Major mode

(define-derived-mode pseudoscheme-thread-control-mode fundamental-mode
  "Threads"
  "PSEUDOSCHEME Thread Control Panel Mode.

\\{pseudoscheme-thread-control-mode-map}
\\{pseudoscheme-popup-buffer-mode-map}"
  (when pseudoscheme-truncate-lines
    (set (make-local-variable 'truncate-lines) t))
  (setq buffer-undo-list t))

(pseudoscheme-define-keys pseudoscheme-thread-control-mode-map
  ("a" 'pseudoscheme-thread-attach)
  ("d" 'pseudoscheme-thread-debug)
  ("g" 'pseudoscheme-update-threads-buffer)
  ("k" 'pseudoscheme-thread-kill)
  ("q" 'pseudoscheme-quit-threads-buffer))

(defun pseudoscheme-thread-kill ()
  (interactive)
  (pseudoscheme-eval `(cl:mapc 'swank:kill-nth-thread
                        ',(pseudoscheme-get-properties 'thread-index)))
  (call-interactively 'pseudoscheme-update-threads-buffer))

(defun pseudoscheme-get-region-properties (prop start end)
  (cl-loop for position = (if (get-text-property start prop)
                              start
                            (next-single-property-change start prop))
           then (next-single-property-change position prop)
           while (<= position end)
           collect (get-text-property position prop)))

(defun pseudoscheme-get-properties (prop)
  (if (use-region-p)
      (pseudoscheme-get-region-properties prop
                                   (region-beginning)
                                   (region-end))
    (let ((value (get-text-property (point) prop)))
      (when value
        (list value)))))

(defun pseudoscheme-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index))
        (file (pseudoscheme-swank-port-file)))
    (pseudoscheme-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (pseudoscheme-read-port-and-connect nil))

(defun pseudoscheme-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index)))
    (pseudoscheme-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode pseudoscheme-connection-list-mode fundamental-mode
  "Pseudoscheme-Connections"
  "PSEUDOSCHEME Connection List Mode.

\\{pseudoscheme-connection-list-mode-map}
\\{pseudoscheme-popup-buffer-mode-map}"
  (when pseudoscheme-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(pseudoscheme-define-keys pseudoscheme-connection-list-mode-map
  ("d"         'pseudoscheme-connection-list-make-default)
  ("g"         'pseudoscheme-update-connection-list)
  ((kbd "C-k") 'pseudoscheme-quit-connection-at-point)
  ("R"         'pseudoscheme-restart-connection-at-point))

;; -----------------------------------------------------------------------------





(defvar pseudoscheme-selector-methods nil
  "List of buffer-selection methods for the `pseudoscheme-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defmacro def-pseudoscheme-selector-method (key description &rest body)
  "Define a new `pseudoscheme-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda ()
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (pseudoscheme-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq pseudoscheme-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key pseudoscheme-selector-methods :key #'car))
                    #'< :key #'car))))


;;;; Contrib modules

(defun pseudoscheme-require (module)
  (cl-pushnew module pseudoscheme-required-modules)
  (when (pseudoscheme-connected-p)
    (pseudoscheme-load-contribs)))

(defun pseudoscheme-load-contribs ()
  (let ((needed (cl-remove-if (lambda (s)
                                (member (cl-subseq (symbol-name s) 1)
                                        (mapcar #'downcase
                                                (pseudoscheme-lisp-modules))))
                              pseudoscheme-required-modules)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Lisp.
      (setf (pseudoscheme-lisp-modules)
            (pseudoscheme-eval `(swank:swank-require ',needed))))))

(cl-defstruct pseudoscheme-contrib
  name
  pseudoscheme-dependencies
  swank-dependencies
  enable
  disable
  authors
  license)

(defun pseudoscheme-contrib--enable-fun (name)
  (intern (concat (symbol-name name) "-init")))

(defun pseudoscheme-contrib--disable-fun (name)
  (intern (concat (symbol-name name) "-unload")))

(defmacro define-pseudoscheme-contrib (name _docstring &rest clauses)
  (declare (indent 1))
  (cl-destructuring-bind (&key pseudoscheme-dependencies
                               swank-dependencies
                               on-load
                               on-unload
                               authors
                               license)
      (cl-loop for (key . value) in clauses append `(,key ,value))
    `(progn
       ,@(mapcar (lambda (d) `(require ',d)) pseudoscheme-dependencies)
       (defun ,(pseudoscheme-contrib--enable-fun name) ()
         (mapc #'funcall ',(mapcar
                            #'pseudoscheme-contrib--enable-fun
                            pseudoscheme-dependencies))
         (mapc #'pseudoscheme-require ',swank-dependencies)
         ,@on-load)
       (defun ,(pseudoscheme-contrib--disable-fun name) ()
         ,@on-unload
         (mapc #'funcall ',(mapcar
                            #'pseudoscheme-contrib--disable-fun
                            pseudoscheme-dependencies)))
       (put 'pseudoscheme-contribs ',name
            (make-pseudoscheme-contrib
             :name ',name :authors ',authors :license ',license
             :pseudoscheme-dependencies ',pseudoscheme-dependencies
             :swank-dependencies ',swank-dependencies
             :enable ',(pseudoscheme-contrib--enable-fun name)
             :disable ',(pseudoscheme-contrib--disable-fun name))))))

(defun pseudoscheme-all-contribs ()
  (cl-loop for (nil val) on (symbol-plist 'pseudoscheme-contribs) by #'cddr
           when (pseudoscheme-contrib-p val)
           collect val))

(defun pseudoscheme-contrib-all-dependencies (contrib)
  "List all contribs recursively needed by CONTRIB, including self."
  (cons contrib
        (cl-mapcan #'pseudoscheme-contrib-all-dependencies
                   (pseudoscheme-contrib-pseudoscheme-dependencies
                    (pseudoscheme-find-contrib contrib)))))

(defun pseudoscheme-find-contrib (name)
  (get 'pseudoscheme-contribs name))

(defun pseudoscheme-read-contrib-name ()
  (let ((names (cl-loop for c in (pseudoscheme-all-contribs) collect
                        (symbol-name (pseudoscheme-contrib-name c)))))
    (intern (completing-read "Contrib: " names nil t))))

(defun pseudoscheme-enable-contrib (name)
  (interactive (list (pseudoscheme-read-contrib-name)))
  (let ((c (or (pseudoscheme-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (pseudoscheme-contrib-enable c))))

(defun pseudoscheme-disable-contrib (name)
  (interactive (list (pseudoscheme-read-contrib-name)))
  (let ((c (or (pseudoscheme-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (pseudoscheme-contrib-disable c))))



;;;;; Pull-down menu

(defvar pseudoscheme-easy-menu
  (let ((C '(pseudoscheme-connected-p)))
    `("PSEUDOSCHEME"
      [ "Edit Definition..."       pseudoscheme-edit-definition ,C ]
      [ "Return From Definition"   pseudoscheme-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          completion-at-point ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              pseudoscheme-eval-defun ,C ]
       [ "Eval Last Expression"    pseudoscheme-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   pseudoscheme-pprint-eval-last-expression ,C ]
       [ "Eval Region"             pseudoscheme-eval-region ,C ]
       [ "Eval Region And Pretty-Print" pseudoscheme-pprint-eval-region ,C ]
       [ "Interactive Eval..."     pseudoscheme-interactive-eval ,C ]
       [ "Edit Lisp Value..."      pseudoscheme-edit-value ,C ]
       [ "Call Defun"              pseudoscheme-call-defun ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     pseudoscheme-macroexpand-1 ,C ]
       [ "Macroexpand All..."      pseudoscheme-macroexpand-all ,C ]
       [ "Create Trace Buffer"     pseudoscheme-redirect-trace-output ,C ]
       [ "Toggle Trace..."         pseudoscheme-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             pseudoscheme-untrace-all ,C]
       [ "Disassemble..."          pseudoscheme-disassemble-symbol ,C ]
       [ "Inspect..."              pseudoscheme-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           pseudoscheme-compile-defun ,C ]
       [ "Compile/Load File"       pseudoscheme-compile-and-load-file ,C ]
       [ "Compile File"            pseudoscheme-compile-file ,C ]
       [ "Compile Region"          pseudoscheme-compile-region ,C ]
       "--"
       [ "Next Note"               pseudoscheme-next-note t ]
       [ "Previous Note"           pseudoscheme-previous-note t ]
       [ "Remove Notes"            pseudoscheme-remove-notes t ]
       [ "List Notes"              pseudoscheme-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            pseudoscheme-who-calls ,C ]
       [ "Who References... "      pseudoscheme-who-references ,C ]
       [ "Who Sets..."             pseudoscheme-who-sets ,C ]
       [ "Who Binds..."            pseudoscheme-who-binds ,C ]
       [ "Who Macroexpands..."     pseudoscheme-who-macroexpands ,C ]
       [ "Who Specializes..."      pseudoscheme-who-specializes ,C ]
       [ "List Callers..."         pseudoscheme-list-callers ,C ]
       [ "List Callees..."         pseudoscheme-list-callees ,C ]
       [ "Next Location"           pseudoscheme-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      pseudoscheme-update-indentation ,C]
       [ "Select Buffer"           pseudoscheme-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     pseudoscheme-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         pseudoscheme-profile-package ,C]
       [ "Profile by Substring"    pseudoscheme-profile-by-substring ,C ]
       [ "Unprofile All"           pseudoscheme-unprofile-all ,C ]
       [ "Show Profiled"           pseudoscheme-profiled-functions ,C ]
       "--"
       [ "Report"                  pseudoscheme-profile-report ,C ]
       [ "Reset Counters"          pseudoscheme-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      pseudoscheme-describe-symbol ,C ]
       [ "Lookup Documentation..." pseudoscheme-documentation-lookup t ]
       [ "Apropos..."              pseudoscheme-apropos ,C ]
       [ "Apropos all..."          pseudoscheme-apropos-all ,C ]
       [ "Apropos Package..."      pseudoscheme-apropos-package ,C ]
       [ "Hyperspec..."            pseudoscheme-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        pseudoscheme-interrupt ,C ]
      [ "Abort Async. Command"     pseudoscheme-quit ,C ]
      [ "Sync Package & Directory" pseudoscheme-sync-package-and-default-directory ,C]
      )))

(defvar pseudoscheme-sldb-easy-menu
  (let ((C '(pseudoscheme-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." pseudoscheme-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-pseudoscheme pseudoscheme-mode-map "PSEUDOSCHEME" pseudoscheme-easy-menu)

(defun pseudoscheme-add-easy-menu ()
  (easy-menu-add pseudoscheme-easy-menu 'pseudoscheme-mode-map))

(add-hook 'pseudoscheme-mode-hook 'pseudoscheme-add-easy-menu)

(defun pseudoscheme-sldb-add-easy-menu ()
  (easy-menu-define menubar-pseudoscheme-sldb
    sldb-mode-map "SLDB" pseudoscheme-sldb-easy-menu)
  (easy-menu-add pseudoscheme-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'pseudoscheme-sldb-add-easy-menu)



;; -----------------------------------------------------------------------------



(defun pseudoscheme-trim-whitespace (str)
  (let ((start (cl-position-if-not (lambda (x)
                                     (memq x '(?\t ?\n ?\s ?\r)))
                                   str))

        (end (cl-position-if-not (lambda (x)
                                   (memq x '(?\t ?\n ?\s ?\r)))
                                 str
                                 :from-end t)))
    (if start
        (substring str start (1+ end))
        "")))

;;;;; Buffer related

(defun pseudoscheme-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun pseudoscheme-column-max ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop for column = (prog2 (end-of-line) (current-column) (forward-line))
             until (= (point) (point-max))
             maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun pseudoscheme-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
            symbol-part))
      n)))

(defun pseudoscheme-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun pseudoscheme-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a package prefix the
current package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (pseudoscheme-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (pseudoscheme-current-package)))
                ;; package is a string like ":cl-user"
                ;; or "CL-USER", or "\"CL-USER\"".
                (if package
                    (pseudoscheme-pretty-package-name package)
                  "CL-USER"))
              (pseudoscheme-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro pseudoscheme-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (declare (indent 0))
  (let ((pointvar (cl-gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(defun pseudoscheme-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (_i (or count 1))
    (pseudoscheme-forward-cruft)
    (forward-sexp)))

(defconst pseudoscheme-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))

(defun pseudoscheme-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at pseudoscheme-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (pseudoscheme-eval-feature-expression
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax
                       (signal 'pseudoscheme-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (pseudoscheme-forward-sexp)))))

(defun pseudoscheme-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (pseudoscheme-point-moves-p (skip-chars-forward " \t\n")
                              (forward-comment (buffer-size))
                              (inline (pseudoscheme-forward-reader-conditional)))))

(defun pseudoscheme-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(put 'pseudoscheme-incorrect-feature-expression
     'error-conditions '(pseudoscheme-incorrect-feature-expression error))

(put 'pseudoscheme-unknown-feature-expression
     'error-conditions '(pseudoscheme-unknown-feature-expression
                         pseudoscheme-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the length=1 constraint is bogus
(defun pseudoscheme-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (pseudoscheme-keywordify e) (pseudoscheme-lisp-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (pseudoscheme-keywordify (car e))))
                    (cl-case head
                      (:and #'cl-every)
                      (:or #'cl-some)
                      (:not
                       (let ((feature-expression e))
                         (lambda (f l)
                           (cond
                            ((pseudoscheme-length= l 0) t)
                            ((pseudoscheme-length= l 1) (not (apply f l)))
                            (t (signal 'pseudoscheme-incorrect-feature-expression
                                       feature-expression))))))
                      (t (signal 'pseudoscheme-unknown-feature-expression head))))
                  #'pseudoscheme-eval-feature-expression
                  (cdr e)))
        (t (signal 'pseudoscheme-incorrect-feature-expression e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun pseudoscheme-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (pseudoscheme-region-for-defun-at-point)))

(defun pseudoscheme-region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun pseudoscheme-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun pseudoscheme-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(put 'pseudoscheme-symbol 'end-op 'pseudoscheme-end-of-symbol)
(put 'pseudoscheme-symbol 'beginning-op 'pseudoscheme-beginning-of-symbol)

(defun pseudoscheme-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (pseudoscheme-beginning-of-symbol) (point)))

(defun pseudoscheme-symbol-end-pos ()
  (save-excursion (pseudoscheme-end-of-symbol) (point)))



(defun pseudoscheme-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  ;; (thing-at-point 'symbol) returns "" in empty buffers
  (let ((bounds (slime-bounds-of-symbol-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun pseudoscheme-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (or (pseudoscheme-bounds-of-symbol-at-point)
      (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)))

(defun pseudoscheme-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (pseudoscheme-bounds-of-sexp-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun pseudoscheme-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (pseudoscheme-sexp-at-point) (user-error "No expression at point")))

(defun pseudoscheme-string-at-point ()
  "Returns the string at point as a string, otherwise nil."
  (let ((sexp (pseudoscheme-sexp-at-point)))
    (if (and sexp
             (eql (char-syntax (aref sexp 0)) ?\"))
        sexp
        nil)))

(defun pseudoscheme-string-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (pseudoscheme-string-at-point) (error "No string at point.")))

(defun pseudoscheme-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))


;; - slime-output-target-to-marker
;; - slime-output-target-marker (target)
;; - slime-emit-to-target (string target)


;;;; Finishing up

(eval-when-compile
  (require 'bytecomp))

;; Not yet analyzed:
;; -slime--byte-compile
;; -slime--compile-hotspots

;; (slime--compile-hotspots)

(add-to-list 'load-path (expand-file-name "contrib" pseudoscheme-path))

(run-hooks 'pseudoscheme-load-hook)
(provide 'pseudoscheme)

;; Do we need to add this for scheme mode?
;; (when (member 'lisp-mode slime-lisp-modes)
;;   (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))

;; Local Variables:
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: latin-1-unix
;; End:
;;; slime.el ends here
