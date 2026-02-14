(require 'ert)






;; The first goal is to create a buffer similar to what is seen in
;; slime.
;; - a prompt that works under beginning and end line.
;  -


;; Steps in the creating the buffer. Note that the actually tasks
;; happen in reverse sequence of calls.

;; 1) slime-repl-buffer -- create the buffer if it doesn't exist,
;;    giving it the name *slime-repl sbcl*
;;
;;    To test (get-buffer "*pseudoscheme-repl*")
;;
;; 2) slime-output-buffer -- after the raw buffer is created, set up variables:
;;    - slime-connection-output-buffer
;;    - slime-repl-mode
;;    - slime-buffer-connection
;;    - slime-buffer-package
;;    - slime-reset-repl-markers
;;
;;    To test:
;;      (pseudoscheme-connection-output-buffer) => #<buffer *slime-repl sbcl*>

;;
;; 3) - slime-repl-insert-prompt -- After everything else is set up in
;;      the buffer, including markers, inserte the repl prompt.
;;


;; The initialization calls
;; - slime-connect
;; - slime-setup-connection
;; - slime-init-connection-state
;; - slime-set-connection-info
;; - (run-hooks 'slime-connected-hook)
;; - slime-repl-connected-hook-function
;; - slime-init-output-buffer
;; - slime-repl-update-banner
;; - slime-repl-set-package  (or slime-eval-last-expression-in-repl)
;; - slime-repl-insert-prompt
;; - slime-output-buffer
;; - slime-repl-buffer

slime-set-connection-info



(let nil
  (slime-dispatch-event
   (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)")
	 (slime-lisp-package) slime-current-thread
	 (lambda (G283)
	   (slime-dcase G283
	     ((:ok result) (slime-repl-insert-result result))
	     ((:abort condition) (slime-repl-show-abort condition)))))))



(slime-repl-insert-result '(:values "4"))



(defun slime-repl-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-repl-event-hook-function)
  (add-hook 'slime-connected-hook 'slime-repl-connected-hook-function)
  (add-hook 'slime-cycle-connections-hook
            'slime-change-repl-to-default-connection))

(defun slime-repl-remove-hooks ()
  (remove-hook 'slime-event-hooks 'slime-repl-event-hook-function)
  (remove-hook 'slime-connected-hook 'slime-repl-connected-hook-function)
  (remove-hook 'slime-cycle-connections-hook
               'slime-change-repl-to-default-connection))


(defun slime-repl-connected-hook-function ()
  (cl-destructuring-bind (package prompt)
      (let ((slime-current-thread t))
	(slime-eval `(swank-repl:create-repl nil)))
    (setf (slime-lisp-package) package)
    (setf (slime-lisp-package-prompt-string) prompt))
  (slime-hide-inferior-lisp-buffer)
  (slime-init-output-buffer (slime-connection)))


(defun slime-init-output-buffer (connection)
  (with-current-buffer (slime-output-buffer t)
    (setq slime-buffer-connection connection
          slime-repl-directory-stack '()
          slime-repl-package-stack '())
    (slime-repl-update-banner)))


(defun slime-repl-update-banner ()
  (funcall slime-repl-banner-function)
  (slime-move-point (point-max))
  (slime-mark-output-start)
  (slime-mark-input-start)
  (slime-repl-insert-prompt))


(defun slime-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (slime-current-package))
                            (p (and p (slime-pretty-package-name p)))
                            (p (and (not (equal p (slime-lisp-package))) p)))
                       (slime-read-package-name "Package: " p))))
  (with-current-buffer (slime-output-buffer)
    (let ((previouse-point (- (point) slime-repl-input-start-mark))
          (previous-prompt (slime-lisp-package-prompt-string)))
      (cl-destructuring-bind (name prompt-string)
          (slime-repl-shortcut-eval `(swank:set-package ,package))
        (setf (slime-lisp-package) name)
        (setf slime-buffer-package name)
        (unless (equal previous-prompt prompt-string)
          (setf (slime-lisp-package-prompt-string) prompt-string)
          (slime-repl-insert-prompt))
        (when (cl-plusp previouse-point)
          (goto-char (+ previouse-point slime-repl-input-start-mark)))))))



(defun slime-eval-last-expression-in-repl (prefix)
  "Evaluates last expression in the Slime REPL.

Switches REPL to current package of the source buffer for the duration. If
used with a prefix argument (C-u), doesn't switch back afterwards."
  (interactive "P")
  (let ((expr (slime-last-expression))
        (buffer-name (buffer-name (current-buffer)))
        (new-package (slime-current-package))
        (old-package (slime-lisp-package))
        (slime-repl-suppress-prompt t)
        (yank-back nil))
    (with-current-buffer (slime-output-buffer)
      (unless (eq (current-buffer) (window-buffer))
        (pop-to-buffer (current-buffer) t))
      (goto-char (point-max))
      ;; Kill pending input in the REPL
      (when (< (marker-position slime-repl-input-start-mark) (point))
        (kill-region slime-repl-input-start-mark (point))
        (setq yank-back t))
      (unwind-protect
          (progn
            (insert-before-markers (format "\n;;; from %s\n" buffer-name))
            (when new-package
              (slime-repl-set-package new-package))
            (let ((slime-repl-suppress-prompt nil))
              (slime-repl-insert-prompt))
            (insert expr)
            (slime-repl-return))
        (unless (or prefix (equal (slime-lisp-package) old-package))
          ;; Switch back.
          (slime-repl-set-package old-package)
          (let ((slime-repl-suppress-prompt nil))
            (slime-repl-insert-prompt))))
      ;; Put pending input back.
      (when yank-back
        (yank)))))

(defun slime-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.
Return the position of the prompt beginning.

If `slime-repl-suppress-prompt' is true, does nothing and returns nil."
  (goto-char slime-repl-input-start-mark)
  (unless slime-repl-suppress-prompt
    (slime-save-marker slime-output-start
      (slime-save-marker slime-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (format "%s> " (slime-lisp-package-prompt-string))))
          (slime-propertize-region
              '(face slime-repl-prompt-face
                     read-only t slime-repl-prompt t
                     rear-nonsticky t front-sticky (read-only)
                     inhibit-line-move-field-capture t
                     field output)
            (insert-before-markers prompt))
          (set-marker slime-repl-prompt-start-mark prompt-start)
          (setq buffer-undo-list nil)
          prompt-start)))))




(get-buffer-process "*inferior-lisp*")

(get-buffer "*inferior-lisp*")


(slime-connection)

(slime-connection-name connection)


(slime-repl-buffer (slime-connection))





;; Note, this curiously seems to be a dead end.
(defun slime-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let* ((package (slime-current-package))
         (exists-p (or (null package)
                       (slime-eval `(cl:packagep
                                     (swank::guess-package ,package)))))
         (directory default-directory))
    (when (and package exists-p)
      (slime-repl-set-package package))
    (slime-set-default-directory directory)
    ;; Sync *inferior-lisp* dir
    (let* ((proc (slime-process))
           (buffer (and proc (process-buffer proc))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "package: %s%s  directory: %s"
             (with-current-buffer (slime-output-buffer)
               (slime-lisp-package))
             (if exists-p "" (format " (package %s doesn't exist)" package))
             directory)))




;;; Some tests

;; The slime connoction we communicate with
(setq my-pseudoscheme-connection (slime-connection))

;; Create buffer
;; (following updated in pseudoscheme.el)
;; (defun pseudoscheme-repl-buffer (&optional create)
;;   "Get the REPL buffer for the current connection; optionally create."
;;   (funcall  #'get-buffer-create
;;            (format "*pseudoscheme-repl*")))

;; Set buffer
(setq (pseudoscheme-connection-output-buffer)
      (get-buffer "*pseudoscheme-repl*"))

;; some setup testing
(defun pseudoscheme-test-output-buffer ()
  (with-current-buffer my-pseudoscheme-connection-output-buffer
    (my-pseudoscheme-repl-mode)
    (pseudoscheme-reset-repl-markers)
    (pseudoscheme-repl-insert-prompt)

    (print (format "pseudoscheme-output-start: %s" pseudoscheme-output-start))
    (print (format "pseudoscheme-output-end: %s" pseudoscheme-output-end))
    (print (format "pseudoscheme-repl-prompt-start-mark: %s"
		   pseudoscheme-repl-prompt-start-mark))
    (print (format "pseudoscheme-repl-input-start-mark: %s"
		   pseudoscheme-repl-input-start-mark))
    (print (format "(eq major-mode 'pseudoscheme-repl-mode) %s"
		   (eq major-mode 'pseudoscheme-repl-mode)))))


;; Place return value into the *pseudoscheme-repl*




(defvar my-slime-repl-insert-result nil)
(let nil
  (slime-dispatch-event
   (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)")
	 (slime-lisp-package) slime-current-thread
	 (lambda (G283)
	   (slime-dcase G283
	     ((:ok result) (setq my-slime-repl-insert-result result))
	     ((:abort condition) (slime-repl-show-abort condition)))))))




(slime-send `(:emacs-rex ,form ,package ,thread ,id))


(defvar my-id)

(setq my-id (cl-incf (slime-continuation-counter)))






(cl-defmacro with-buffer-test-environment (buf-expr &rest body)
  (declare (indent 1))
  (let ((buf-sym (make-symbol "buf"))
        (win-sym (make-symbol "orig-window"))
        (cur-sym (make-symbol "orig-buffer"))
        (pt-sym  (make-symbol "orig-point"))
        (txt-sym (make-symbol "orig-text")))
    `(let* ((,buf-sym ,buf-expr)
            (,win-sym (selected-window))
            (,cur-sym (current-buffer))
            (,pt-sym (point))
            (,txt-sym (with-current-buffer ,buf-sym
                        (buffer-string)))
            (test-window (display-buffer ,buf-sym)))
       (unwind-protect
           (progn
             (select-window test-window)
             ,@body)
         ;; restore state
         (select-window ,win-sym)
         (switch-to-buffer ,cur-sym)
         (with-current-buffer ,buf-sym
           (erase-buffer)
           (insert ,txt-sym)
           (goto-char ,pt-sym))))))


(cl-defmacro with-buffer-test-environment (buffer &rest body)
  "Run BODY with BUFFER selected, then restore window/buffer state."
  (declare (indent 1))
  `(let* ((orig-window (selected-window))
          (orig-buffer (current-buffer))
          (orig-point (point))
          (orig-contents (with-current-buffer ,buffer
                           (buffer-string)))
          (test-window (display-buffer ,buffer)))
     (unwind-protect
         (progn
           (select-window test-window)
           ,@body)
       ;; restore window/buffer state
       (select-window orig-window)
       (switch-to-buffer orig-buffer)
       (with-current-buffer ,buffer
         (erase-buffer)
         (insert orig-contents)
         (goto-char orig-point)))))

(with-buffer-test-environment "*pseudoscheme-repl*"
  (execute-kbd-macro (kbd "RET")))


(cl-defun pseudoscheme-test-buffer ()
  (with-current-buffer "*pseudoscheme-repl*"
    (execute-kbd-macro (kbd "RET"))
    t))


(cl-defun my-slime-test-buffer ()
  (with-current-buffer (slime-output-buffer)
    (execute-kbd-macro (kbd "RET"))))



  ;;(execute-kbd-macro (kbd "H e l l o SPC w o r l d RET"))


(cl-defmacro eek-run-in-slime (name &rest rest)
  (with-current-buffer (slime-output-buffer)
    (apply name rest)))

(cl-defmacro eek-run-in-ps (name &rest rest)
  (with-current-buffer (pseudoscheme-output-buffer)
    (apply name rest)))

(lambda ()
(pseudoscheme-dispatch-event
 (list ':emacs-return-string
       (car-safe
	(prog1
	    pseudoscheme-read-string-threads
	  (setq pseudoscheme-read-string-threads
		(cdr pseudoscheme-read-string-threads))))
       (car-safe
	(prog1
	    pseudoscheme-read-string-tags
	  (setq pseudoscheme-read-string-tags
		(cdr pseudoscheme-read-string-tags))))
       string)))



(defun example-update ()
  ;; initial part is slime-repl-return
  (pseudoscheme-dispatch-presentation-event '(:presentation-start 3 :repl-result))
  (pseudoscheme-repl-event-hook-function '(:write-string "3" :repl-result))
  (pseudoscheme-dispatch-presentation-event '(:presentation-end 3 :repl-result))
  (pseudoscheme-repl-event-hook-function '(:write-string "\n" :repl-result)))



(defun slime-dispatch-event (event &optional process)
  (slime-dcase event
    ((:emacs-rex form package thread continuation)
     (let ((id (cl-incf (slime-continuation-counter))))
       (slime-send `(:emacs-rex ,form ,package ,thread ,id))
       (push (cons id continuation) (slime-rex-continuations))
       (slime--recompute-modelines)))


;; insert a value into the pseudoscheme buffer
(pseudoscheme-repl-insert-result '(:values "4"))


(defun my-pseudoscheme-output-buffer (&optional noprompt)
  (let ((connection (slime-connection)))
    (with-current-buffer (pseudoscheme-repl-buffer t connection)
      (pseudoscheme-repl-insert-prompt))))


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





(with-current-buffer (slime-output-buffer)
  (slime-dispatch-event
   (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)")
	 (slime-lisp-package) slime-current-thread
	 (lambda (G283)
	   (slime-dcase G283
	     ((:ok result) (slime-repl-insert-result result))
	     ((:abort condition) (slime-repl-show-abort condition)))))))



(slime-dispatch-event
   (list :emacs-rex '(swank-repl:listener-eval "(+ 1 2)")
	 (slime-lisp-package) slime-current-thread
	 (lambda (G283)
	   (slime-dcase G283
	     ((:ok result) (pseudoscheme-repl-insert-result result))
	     ((:abort condition) (slime-repl-show-abort condition))))))







(defun slime-init-output-buffer (connection)
  (with-current-buffer (slime-output-buffer t)
    (setq slime-buffer-connection connection
          slime-repl-directory-stack '()
          slime-repl-package-stack '())
    (slime-repl-update-banner)))

(defun slime-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (slime-repl-show-maximum-output)))



(defun slime-repl-connected-hook-function ()
  (cl-destructuring-bind (package prompt)
      (let ((slime-current-thread t))
	(slime-eval `(swank-repl:create-repl nil)))
    (setf (slime-lisp-package) package)
    (setf (slime-lisp-package-prompt-string) prompt))
  (slime-hide-inferior-lisp-buffer)
  (slime-init-output-buffer (slime-connection)))


(ert-deftest my-typing-test ()
  ;; make it the selected window
  (with-temp-buffer (switch-to-buffer (current-buffer))
		    (execute-kbd-macro (kbd "H e l l o"))
		    (should (equal (buffer-string) "Hello"))))



(defun my-run-slime-tests ()
  (let ((start-buffer (current-buffer))
	(test-buffer (get-buffer "*slime-repl sbcl*")))
    (save-window-excursion
      (pop-to-buffer test-buffer)
      (insert "\n")
      ;;(execute-kbd-macro (kbd "RET"))
      (slime-repl-send-input)
      )))



(defmacro with-typing-test (&rest body)
  "Create a temporary buffer in a real window and run BODY inside it."
  `(let ((buf (generate-new-buffer "*typing-test*")))
    (save-window-excursion
      (pop-to-buffer (get-buffer "*slime-repl sbcl*"))
      (insert "\n")
      ;;(execute-kbd-macro (kbd "RET"))
      (slime-repl-send-input)
      )))



(defun my-run-pseudoscheme-tests ()
  (let ((start-buffer (current-buffer))
	(test-buffer (get-buffer "*pseudoscheme-repl*")))
    (save-window-excursion
      (pop-to-buffer test-buffer)
      (insert "\n")
      ;;(execute-kbd-macro (kbd "RET"))
      (unless (pseudoscheme-repl-in-input-area-p)
	(error "No input at point."))
      ;; (pseudoscheme-repl-send-input)
      )))



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










(defmacro with-typing-test (&rest body)
  "Create a temporary buffer in a real window and run BODY inside it."
  `(let ((buf (generate-new-buffer "*typing-test*")))
     ;; select window + show buffer
     (unwind-protect (progn (pop-to-buffer buf)
			    (erase-buffer)
			    (typing--reset) ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))


(slime-output-buffer)


;; A "\n" typed into *slime-repl sbcl* produces the following events


;; (slime-repl-eval-string "\n") -->

(slime-dispatch-event
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (slime-dcase ,result
                  ,@continuations))))



;;   (:emacs-rex (swank-repl:listener-eval "\n") "COMMON-LISP-USER"
	    ;; :repl-thread 13)

(:write-string "; No value" :repl-result)

(:return (:ok nil) 13)






get-buffer-create


slime-repl-buffer






(defun global-f1 ()
  (funcall (dispatch 'f1)))

(defun global-f2 ()
  (funcall (dispatch 'f2)))

(defun dispatch (fun)
  (cl-flet* ((my-function1 ()
               1)

             (my-function2 ()
	       2))

    (case fun
      ('f1 #'my-function1)
      ('f2 #'my-function2))))






;; examples
(defun my-example-with-current-buffer ()
  (with-current-buffer "*slime-repl sbcl*"
    (cl-destructuring-bind (package prompt)
	(let ((slime-current-thread t))
	  (slime-eval `(swank-repl:create-repl nil)))
      (list package prompt))))


;; Create a pedegogical example.


(defvar my-slime-response-handlers '())
(defvar my-slime-stack-eval-tags '())
(defvar my-slime-continuation-counter 0)





(defun my-slime-asynchronous-call ()

 (defun  my-slime-synchronous-call ()
  (let* ((tag (cl-gensym (format "my-slime-result-%d-"
                                 (1+ (slime-continuation-counter))))))
    (catch tag
      (push
       (lambda (result)
	 (throw tag (list #'identity value))


(defun my-slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (slime-current-package)))
  (let* ((tag (cl-gensym (format "slime-result-%d-"
                                 (1+ (slime-continuation-counter)))))
	 (slime-stack-eval-tags (cons tag slime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (slime-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag slime-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort _condition)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (slime-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
	   (message "I doubt this shows up only once")
           (accept-process-output nil 0.01)
	   ))))))
