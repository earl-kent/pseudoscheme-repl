(require 'pseudoscheme)
(require 'cl-lib)

(define-pseudoscheme-contrib pseudoscheme-presentations
  "Imitate LispM presentations."
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>")
  (:license "GPL")
  (:pseudoscheme-dependencies pseudoscheme-repl)
  (:swank-dependencies swank-presentations)
  (:on-load
   (add-hook 'pseudoscheme-repl-mode-hook
             (lambda ()
               ;; Respect the syntax text properties of presentation.
               (set (make-local-variable 'parse-sexp-lookup-properties) t)
               (add-hook 'after-change-functions
                         'pseudoscheme-after-change-function 'append t)))
   (add-hook 'pseudoscheme-event-hooks 'pseudoscheme-dispatch-presentation-event)
   (setq pseudoscheme-write-string-function 'pseudoscheme-presentation-write)
   (add-hook 'pseudoscheme-connected-hook 'pseudoscheme-presentations-on-connected)
   (add-hook 'pseudoscheme-repl-return-hooks 'pseudoscheme-presentation-on-return-pressed)
   (add-hook 'pseudoscheme-repl-current-input-hooks 'pseudoscheme-presentation-current-input)
   (add-hook 'pseudoscheme-repl-clear-buffer-hook 'pseudoscheme-clear-presentations)
   (add-hook 'pseudoscheme-edit-definition-hooks 'pseudoscheme-edit-presentation)
   (setq sldb-insert-frame-variable-value-function
         'pseudoscheme-presentation-sldb-insert-frame-variable-value)
   (pseudoscheme-presentation-init-keymaps)
   (pseudoscheme-presentation-add-easy-menu)))

;; To get presentations in the inspector as well, add this to your
;; init file.
;;
;; (eval-after-load 'pseudoscheme-presentations
;;    '(setq pseudoscheme-inspector-insert-ispec-function
;;           'pseudoscheme-presentation-inspector-insert-ispec))
;;
(defface pseudoscheme-repl-output-mouseover-face
    '((t (:box (:line-width 1 :color "black" :style released-button)
          :inherit pseudoscheme-repl-inputed-output-face)))
  "Face for Lisp output in the PSEUDOSCHEME REPL, when the mouse hovers over it"
  :group 'pseudoscheme-repl)

(defface pseudoscheme-repl-inputed-output-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "light salmon"))
    (t (:slant italic)))
  "Face for the result of an evaluation in the PSEUDOSCHEME REPL."
  :group 'pseudoscheme-repl)

;; FIXME: This conditional is not right - just used because the code
;; here does not work in XEmacs.
(when (boundp 'text-property-default-nonsticky)
  (cl-pushnew '(pseudoscheme-repl-presentation . t) text-property-default-nonsticky
              :test 'equal)
  (cl-pushnew '(pseudoscheme-repl-result-face . t) text-property-default-nonsticky
              :test 'equal))

(make-variable-buffer-local
 (defvar pseudoscheme-presentation-start-to-point (make-hash-table)))

(defun pseudoscheme-mark-presentation-start (id &optional target)
  "Mark the beginning of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (setf (gethash id pseudoscheme-presentation-start-to-point)
        ;; We use markers because text can also be inserted before this presentation.
        ;; (Output arrives while we are writing presentations within REPL results.)
        (copy-marker (pseudoscheme-repl-output-target-marker target) nil)))

(defun pseudoscheme-mark-presentation-start-handler (process string)
  (if (and string (string-match "<\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (pseudoscheme-mark-presentation-start id))))

(defun pseudoscheme-mark-presentation-end (id &optional target)
  "Mark the end of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (let ((start (gethash id pseudoscheme-presentation-start-to-point)))
    (remhash id pseudoscheme-presentation-start-to-point)
    (when start
      (let* ((marker (pseudoscheme-repl-output-target-marker target))
             (buffer (and marker (marker-buffer marker))))
        (with-current-buffer buffer
          (let ((end (marker-position marker)))
            (pseudoscheme-add-presentation-properties start end
                                               id nil)))))))

(defun pseudoscheme-mark-presentation-end-handler (process string)
  (if (and string (string-match ">\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (pseudoscheme-mark-presentation-end id))))

(cl-defstruct pseudoscheme-presentation text id)

(defvar pseudoscheme-presentation-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This allows to use C-M-k, C-M-SPC,
    ;; etc. to deal with a whole presentation.  (For Lisp mode, this
    ;; is not desirable, since we do not wish to get a mismatched
    ;; paren highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Syntax table for presentations.")

(defun pseudoscheme-add-presentation-properties (start end id result-p)
  "Make the text between START and END a presentation with ID.
RESULT-P decides whether a face for a return value or output text is used."
  (let* ((text (buffer-substring-no-properties start end))
         (presentation (make-pseudoscheme-presentation :text text :id id)))
    (let ((inhibit-modification-hooks t))
      (add-text-properties start end
                           `(modification-hooks (pseudoscheme-after-change-function)
                             insert-in-front-hooks (pseudoscheme-after-change-function)
                             insert-behind-hooks (pseudoscheme-after-change-function)
                             syntax-table ,pseudoscheme-presentation-syntax-table
                             rear-nonsticky t))
      ;; Use the presentation as the key of a text property
      (cl-case (- end start)
        (0)
        (1
         (add-text-properties start end
                              `(pseudoscheme-repl-presentation ,presentation
                                ,presentation :start-and-end)))
        (t
         (add-text-properties start (1+ start)
                              `(pseudoscheme-repl-presentation ,presentation
                                ,presentation :start))
         (when (> (- end start) 2)
           (add-text-properties (1+ start) (1- end)
                                `(,presentation :interior)))
         (add-text-properties (1- end) end
                              `(pseudoscheme-repl-presentation ,presentation
                                ,presentation :end))))
      ;; Also put an overlay for the face and the mouse-face.  This enables
      ;; highlighting of nested presentations.  However, overlays get lost
      ;; when we copy a presentation; their removal is also not undoable.
      ;; In these cases the mouse-face text properties need to take over ---
      ;; but they do not give nested highlighting.
      (pseudoscheme-ensure-presentation-overlay start end presentation))))

(defvar pseudoscheme-presentation-map (make-sparse-keymap))

(defun pseudoscheme-ensure-presentation-overlay (start end presentation)
  (unless (cl-find presentation (overlays-at start)
                   :key (lambda (overlay)
                          (overlay-get overlay 'pseudoscheme-repl-presentation)))
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'pseudoscheme-repl-presentation presentation)
      (overlay-put overlay 'mouse-face 'pseudoscheme-repl-output-mouseover-face)
      (overlay-put overlay 'help-echo
                   (if (eq major-mode 'pseudoscheme-repl-mode)
                       "mouse-2: copy to input; mouse-3: menu"
                     "mouse-2: inspect; mouse-3: menu"))
      (overlay-put overlay 'face 'pseudoscheme-repl-inputed-output-face)
      (overlay-put overlay 'keymap pseudoscheme-presentation-map))))

(defun pseudoscheme-remove-presentation-properties (from to presentation)
  (let ((inhibit-read-only t))
    (remove-text-properties from to
                            `(,presentation t syntax-table t rear-nonsticky t))
    (when (eq (get-text-property from 'pseudoscheme-repl-presentation) presentation)
      (remove-text-properties from (1+ from) `(pseudoscheme-repl-presentation t)))
    (when (eq (get-text-property (1- to) 'pseudoscheme-repl-presentation) presentation)
      (remove-text-properties (1- to) to `(pseudoscheme-repl-presentation t)))
    (dolist (overlay (overlays-at from))
      (when (eq (overlay-get overlay 'pseudoscheme-repl-presentation) presentation)
        (delete-overlay overlay)))))

(defun pseudoscheme-insert-presentation (string output-id &optional rectangle)
  "Insert STRING in current buffer and mark it as a presentation
corresponding to OUTPUT-ID.  If RECTANGLE is true, indent multi-line
strings to line up below the current point."
  (cl-labels ((insert-it ()
                       (if rectangle
                           (pseudoscheme-insert-indented string)
                         (insert string))))
    (let ((start (point)))
      (insert-it)
      (pseudoscheme-add-presentation-properties start (point) output-id t))))

(defun pseudoscheme-presentation-whole-p (presentation start end &optional object)
  (let ((object (or object (current-buffer))))
    (string= (cl-etypecase object
               (buffer (with-current-buffer object
                         (buffer-substring-no-properties start end)))
               (string (substring-no-properties object start end)))
             (pseudoscheme-presentation-text presentation))))

(defun pseudoscheme-presentations-around-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (cl-loop for (key value . rest) on (text-properties-at point object) by 'cddr
             when (pseudoscheme-presentation-p key)
             collect key)))

(defun pseudoscheme-presentation-start-p (tag)
  (memq tag '(:start :start-and-end)))

(defun pseudoscheme-presentation-stop-p (tag)
  (memq tag '(:end :start-and-end)))

(cl-defun pseudoscheme-presentation-start (point presentation
                                          &optional (object (current-buffer)))
  "Find start of `presentation' at `point' in `object'.
Return buffer index and whether a start-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (pseudoscheme-presentation-start-p this-presentation))
      (let ((change-point (previous-single-property-change
                           point presentation object (point-min))))
        (unless change-point
          (cl-return-from pseudoscheme-presentation-start
            (cl-values (cl-etypecase object
                         (buffer (with-current-buffer object 1))
                         (string 0))
                       nil)))
        (setq this-presentation (get-text-property change-point
                                                   presentation object))
        (unless this-presentation
          (cl-return-from pseudoscheme-presentation-start
            (cl-values point nil)))
        (setq point change-point)))
    (cl-values point t)))

(cl-defun pseudoscheme-presentation-end (point presentation
                                        &optional (object (current-buffer)))
  "Find end of presentation at `point' in `object'.  Return buffer
index (after last character of the presentation) and whether an
end-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (pseudoscheme-presentation-stop-p this-presentation))
      (let ((change-point (next-single-property-change
                           point presentation object)))
        (unless change-point
          (cl-return-from pseudoscheme-presentation-end
            (cl-values (cl-etypecase object
                         (buffer (with-current-buffer object (point-max)))
                         (string (length object)))
                       nil)))
        (setq point change-point)
        (setq this-presentation (get-text-property point
                                                   presentation object))))
    (if this-presentation
        (let ((after-end (next-single-property-change point
                                                      presentation object)))
          (if (not after-end)
              (cl-values (cl-etypecase object
                           (buffer (with-current-buffer object (point-max)))
                           (string (length object)))
                         t)
            (cl-values after-end t)))
      (cl-values point nil))))

(cl-defun pseudoscheme-presentation-bounds (point presentation
                                           &optional (object (current-buffer)))
  "Return start index and end index of `presentation' around `point'
in `object', and whether the presentation is complete."
  (cl-multiple-value-bind (start good-start)
      (pseudoscheme-presentation-start point presentation object)
    (cl-multiple-value-bind (end good-end)
        (pseudoscheme-presentation-end point presentation object)
      (cl-values start end
                 (and good-start good-end
                      (pseudoscheme-presentation-whole-p presentation
                                                  start end object))))))

(defun pseudoscheme-presentation-around-point (point &optional object)
  "Return presentation, start index, end index, and whether the
presentation is complete."
  (let ((object (or object (current-buffer)))
        (innermost-presentation nil)
        (innermost-start 0)
        (innermost-end most-positive-fixnum))
    (dolist (presentation (pseudoscheme-presentations-around-point point object))
      (cl-multiple-value-bind (start end whole-p)
          (pseudoscheme-presentation-bounds point presentation object)
        (when whole-p
          (when (< (- end start) (- innermost-end innermost-start))
            (setq innermost-start start
                  innermost-end end
                  innermost-presentation presentation)))))
    (cl-values innermost-presentation
               innermost-start innermost-end)))

(defun pseudoscheme-presentation-around-or-before-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (cl-multiple-value-bind (presentation start end whole-p)
        (pseudoscheme-presentation-around-point point object)
      (if (or presentation (= point (point-min)))
          (cl-values presentation start end whole-p)
        (pseudoscheme-presentation-around-point (1- point) object)))))

(defun pseudoscheme-presentation-around-or-before-point-or-error (point)
  (cl-multiple-value-bind (presentation start end whole-p)
      (pseudoscheme-presentation-around-or-before-point point)
    (unless presentation
      (error "No presentation at point"))
    (cl-values presentation start end whole-p)))

(cl-defun pseudoscheme-for-each-presentation-in-region (from to function
                                                      &optional (object (current-buffer)))
  "Call `function' with arguments `presentation', `start', `end',
`whole-p' for every presentation in the region `from'--`to' in the
string or buffer `object'."
  (cl-labels ((handle-presentation (presentation point)
                                   (cl-multiple-value-bind (start end whole-p)
                                       (pseudoscheme-presentation-bounds point presentation object)
                                     (funcall function presentation start end whole-p))))
    ;; Handle presentations active at `from'.
    (dolist (presentation (pseudoscheme-presentations-around-point from object))
      (handle-presentation presentation from))
    ;; Use the `pseudoscheme-repl-presentation' property to search for new presentations.
    (let ((point from))
      (while (< point to)
        (setq point (next-single-property-change point 'pseudoscheme-repl-presentation
                                                 object to))
        (let* ((presentation (get-text-property point 'pseudoscheme-repl-presentation object))
               (status (get-text-property point presentation object)))
          (when (pseudoscheme-presentation-start-p status)
            (handle-presentation presentation point)))))))

(defun pseudoscheme-after-change-function (start end &rest ignore)
  "Check all presentations within and adjacent to the change.
When a presentation has been altered, change it to plain text."
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end   (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      ;; positions around the change
      (pseudoscheme-for-each-presentation-in-region
       real-start real-end
       (lambda (presentation from to whole-p)
         (cond
          (whole-p
           (pseudoscheme-ensure-presentation-overlay from to presentation))
          ((not undo-in-progress)
           (pseudoscheme-remove-presentation-properties from to
                                                 presentation)
           (setq any-change t)))))
      (when any-change
        (undo-boundary)))))

(defun pseudoscheme-presentation-around-click (event)
  "Return the presentation around the position of the mouse-click EVENT.
If there is no presentation, signal an error.
Also return the start position, end position, and buffer of the presentation."
  (when (and (featurep 'xemacs) (not (button-press-event-p event)))
    (error "Command must be bound to a button-press-event"))
  (let ((point (if (featurep 'xemacs) (event-point event) (posn-point (event-end event))))
        (window (if (featurep 'xemacs) (event-window event) (caadr event))))
    (with-current-buffer (window-buffer window)
      (cl-multiple-value-bind (presentation start end)
          (pseudoscheme-presentation-around-point point)
        (unless presentation
          (error "No presentation at click"))
        (cl-values presentation start end (current-buffer))))))

(defun pseudoscheme-check-presentation (from to buffer presentation)
  (unless (pseudoscheme-eval `(cl:nth-value 1 (swank:lookup-presented-object
                                        ',(pseudoscheme-presentation-id presentation))))
    (with-current-buffer buffer
      (pseudoscheme-remove-presentation-properties from to presentation))))

(defun pseudoscheme-copy-or-inspect-presentation-at-mouse (event)
  (interactive "e") ; no "@" -- we don't want to select the clicked-at window
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (pseudoscheme-check-presentation start end buffer presentation)
    (if (with-current-buffer buffer
          (eq major-mode 'pseudoscheme-repl-mode))
        (pseudoscheme-copy-presentation-at-mouse-to-repl event)
      (pseudoscheme-inspect-presentation-at-mouse event))))

(defun pseudoscheme-inspect-presentation (presentation start end buffer)
  (let ((reset-p
	 (with-current-buffer buffer
	   (not (eq major-mode 'pseudoscheme-inspector-mode)))))
    (pseudoscheme-eval-async `(swank:inspect-presentation ',(pseudoscheme-presentation-id presentation) ,reset-p)
      'pseudoscheme-open-inspector)))

(defun pseudoscheme-inspect-presentation-at-mouse (event)
  (interactive "e")
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (pseudoscheme-inspect-presentation presentation start end buffer)))

(defun pseudoscheme-inspect-presentation-at-point (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation start end)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-inspect-presentation presentation start end (current-buffer))))


(defun pseudoscheme-M-.-presentation (presentation start end buffer &optional where)
  (let* ((id (pseudoscheme-presentation-id presentation))
	 (presentation-string (format "Presentation %s" id))
	 (location (pseudoscheme-eval `(swank:find-definition-for-thing
				 (swank:lookup-presented-object
				  ',(pseudoscheme-presentation-id presentation))))))
    (unless (eq (car location) :error)
      (pseudoscheme-edit-definition-cont
       (and location (list (make-pseudoscheme-xref :dspec `(,presentation-string)
                                            :location location)))
       presentation-string
       where))))

(defun pseudoscheme-M-.-presentation-at-mouse (event)
  (interactive "e")
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (pseudoscheme-M-.-presentation presentation start end buffer)))

(defun pseudoscheme-M-.-presentation-at-point (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation start end)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-M-.-presentation presentation start end (current-buffer))))

(defun pseudoscheme-edit-presentation (name &optional where)
  (if (or current-prefix-arg (not (equal (pseudoscheme-symbol-at-point) name)))
      nil ; NAME came from user explicitly, so decline.
    (cl-multiple-value-bind (presentation start end whole-p)
        (pseudoscheme-presentation-around-or-before-point (point))
      (when presentation
        (pseudoscheme-M-.-presentation presentation start end (current-buffer) where)))))

(defun pseudoscheme-copy-presentation-to-repl (presentation start end buffer)
  (let ((text (with-current-buffer buffer
                ;; we use the buffer-substring rather than the
                ;; presentation text to capture any overlays
                (buffer-substring start end)))
        (id (pseudoscheme-presentation-id presentation)))
    (unless (integerp id)
      (setq id (pseudoscheme-eval `(swank:lookup-and-save-presented-object-or-lose ',id))))
    (unless (eql major-mode 'pseudoscheme-repl-mode)
      (pseudoscheme-switch-to-output-buffer))
    (cl-flet ((do-insertion ()
                (unless (looking-back "\\s-" (- (point) 1))
                  (insert " "))
                (pseudoscheme-insert-presentation text id)
                (unless (or (eolp) (looking-at "\\s-"))
                  (insert " "))))
      (if (>= (point) pseudoscheme-repl-prompt-start-mark)
          (do-insertion)
        (save-excursion
          (goto-char (point-max))
          (do-insertion))))))

(defun pseudoscheme-copy-presentation-at-mouse-to-repl (event)
  (interactive "e")
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (pseudoscheme-copy-presentation-to-repl presentation start end buffer)))

(defun pseudoscheme-copy-presentation-at-point-to-repl (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation start end)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-copy-presentation-to-repl presentation start end (current-buffer))))

(defun pseudoscheme-copy-presentation-at-mouse-to-point (event)
  (interactive "e")
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (let ((presentation-text
           (with-current-buffer buffer
             (buffer-substring start end))))
      (when (not (string-match "\\s-"
                               (buffer-substring (1- (point)) (point))))
        (insert " "))
      (insert presentation-text)
      (pseudoscheme-after-change-function (point) (point))
      (when (and (not (eolp)) (not (looking-at "\\s-")))
        (insert " ")))))

(defun pseudoscheme-copy-presentation-to-kill-ring (presentation start end buffer)
  (let ((presentation-text
         (with-current-buffer buffer
           (buffer-substring start end))))
    (kill-new presentation-text)
    (message "Saved presentation \"%s\" to kill ring" presentation-text)))

(defun pseudoscheme-copy-presentation-at-mouse-to-kill-ring (event)
  (interactive "e")
  (cl-multiple-value-bind (presentation start end buffer)
      (pseudoscheme-presentation-around-click event)
    (pseudoscheme-copy-presentation-to-kill-ring presentation start end buffer)))

(defun pseudoscheme-copy-presentation-at-point-to-kill-ring (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation start end)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-copy-presentation-to-kill-ring presentation start end (current-buffer))))

(defun pseudoscheme-describe-presentation (presentation)
  (pseudoscheme-eval-describe
   `(swank::describe-to-string
     (swank:lookup-presented-object ',(pseudoscheme-presentation-id presentation)))))

(defun pseudoscheme-describe-presentation-at-mouse (event)
  (interactive "@e")
  (cl-multiple-value-bind (presentation) (pseudoscheme-presentation-around-click event)
    (pseudoscheme-describe-presentation presentation)))

(defun pseudoscheme-describe-presentation-at-point (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-describe-presentation presentation)))

(defun pseudoscheme-pretty-print-presentation (presentation)
  (pseudoscheme-eval-describe
   `(swank::swank-pprint
     (cl:list
      (swank:lookup-presented-object ',(pseudoscheme-presentation-id presentation))))))

(defun pseudoscheme-pretty-print-presentation-at-mouse (event)
  (interactive "@e")
  (cl-multiple-value-bind (presentation) (pseudoscheme-presentation-around-click event)
    (pseudoscheme-pretty-print-presentation presentation)))

(defun pseudoscheme-pretty-print-presentation-at-point (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (pseudoscheme-pretty-print-presentation presentation)))

(defun pseudoscheme-mark-presentation (point)
  (interactive "d")
  (cl-multiple-value-bind (presentation start end)
      (pseudoscheme-presentation-around-or-before-point-or-error point)
    (goto-char start)
    (push-mark end nil t)))

(defun pseudoscheme-previous-presentation (&optional arg)
  "Move point to the beginning of the first presentation before point.
With ARG, do this that many times.
A negative argument means move forward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (pseudoscheme-next-presentation (- arg)))

(defun pseudoscheme-next-presentation (&optional arg)
  "Move point to the beginning of the next presentation after point.
With ARG, do this that many times.
A negative argument means move backward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((cl-plusp arg)
    (dotimes (i arg)
      ;; First skip outside the current surrounding presentation (if any)
      (cl-multiple-value-bind (presentation start end)
	  (pseudoscheme-presentation-around-point (point))
	(when presentation
	  (goto-char end)))
      (let ((p (next-single-property-change (point) 'pseudoscheme-repl-presentation)))
	(unless p
	  (error "No next presentation"))
	(cl-multiple-value-bind (presentation start end)
	    (pseudoscheme-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))
   ((cl-minusp arg)
    (dotimes (i (- arg))
      ;; First skip outside the current surrounding presentation (if any)
      (cl-multiple-value-bind (presentation start end)
	  (pseudoscheme-presentation-around-point (point))
	(when presentation
	  (goto-char start)))
      (let ((p (previous-single-property-change (point) 'pseudoscheme-repl-presentation)))
	(unless p
	  (error "No previous presentation"))
	(cl-multiple-value-bind (presentation start end)
	    (pseudoscheme-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))))

(define-key  pseudoscheme-presentation-map [mouse-2] 'pseudoscheme-copy-or-inspect-presentation-at-mouse)
(define-key  pseudoscheme-presentation-map [mouse-3] 'pseudoscheme-presentation-menu)

(when (featurep 'xemacs)
  (define-key  pseudoscheme-presentation-map [button2] 'pseudoscheme-copy-or-inspect-presentation-at-mouse)
  (define-key  pseudoscheme-presentation-map [button3] 'pseudoscheme-presentation-menu))

;; protocol for handling up a menu.
;; 1. Send lisp message asking for menu choices for this object.
;;    Get back list of strings.
;; 2. Let used choose
;; 3. Call back to execute menu choice, passing nth and string of choice

(defun pseudoscheme-menu-choices-for-presentation (presentation buffer from to choice-to-lambda)
  "Return a menu for `presentation' at `from'--`to' in `buffer', suitable for `x-popup-menu'."
  (let* ((what (pseudoscheme-presentation-id presentation))
         (choices (with-current-buffer buffer
                    (pseudoscheme-eval
                     `(swank::menu-choices-for-presentation-id ',what)))))
    (cl-labels ((savel (f) ;; IMPORTANT - xemacs can't handle lambdas in x-popup-menu. So give them a name
                     (let ((sym (cl-gensym)))
                       (setf (gethash sym choice-to-lambda) f)
                       sym)))
      (cl-etypecase choices
        (list
         `(,(format "Presentation %s" (truncate-string-to-width
                                       (pseudoscheme-presentation-text presentation)
                                       30 nil nil t))
           (""
            ("Find Definition" . ,(savel 'pseudoscheme-M-.-presentation-at-mouse))
            ("Inspect" . ,(savel 'pseudoscheme-inspect-presentation-at-mouse))
            ("Describe" . ,(savel 'pseudoscheme-describe-presentation-at-mouse))
            ("Pretty-print" . ,(savel 'pseudoscheme-pretty-print-presentation-at-mouse))
            ("Copy to REPL" . ,(savel 'pseudoscheme-copy-presentation-at-mouse-to-repl))
            ("Copy to kill ring" . ,(savel 'pseudoscheme-copy-presentation-at-mouse-to-kill-ring))
            ,@(unless buffer-read-only
                `(("Copy to point" . ,(savel 'pseudoscheme-copy-presentation-at-mouse-to-point))))
            ,@(let ((nchoice 0))
                (mapcar
                 (lambda (choice)
                   (cl-incf nchoice)
                   (cons choice
                         (savel `(lambda ()
                                   (interactive)
                                   (pseudoscheme-eval
                                    '(swank::execute-menu-choice-for-presentation-id
                                      ',what ,nchoice ,(nth (1- nchoice) choices)))))))
                 choices)))))
        (symbol                           ; not-present
         (with-current-buffer buffer
           (pseudoscheme-remove-presentation-properties from to presentation))
         (sit-for 0)                      ; allow redisplay
         `("Object no longer recorded"
           ("sorry" . ,(if (featurep 'xemacs) nil '(nil)))))))))

(defun pseudoscheme-presentation-menu (event)
  (interactive "e")
  (let* ((point (if (featurep 'xemacs) (event-point event)
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (choice-to-lambda (make-hash-table)))
    (cl-multiple-value-bind (presentation from to)
        (with-current-buffer buffer
          (pseudoscheme-presentation-around-point point))
      (unless presentation
        (error "No presentation at event position"))
      (let ((menu (pseudoscheme-menu-choices-for-presentation
                   presentation buffer from to choice-to-lambda)))
        (let ((choice (x-popup-menu event menu)))
          (when choice
            (call-interactively (gethash choice choice-to-lambda))))))))

(defun pseudoscheme-presentation-expression (presentation)
  "Return a string that contains a CL s-expression accessing
the presented object."
  (let ((id (pseudoscheme-presentation-id presentation)))
    (cl-etypecase id
      (number
       ;; Make sure it works even if *read-base* is not 10.
       (format "(swank:lookup-presented-object-or-lose %d.)" id))
      (list
       ;; for frame variables and inspector parts
       (format "(swank:lookup-presented-object-or-lose '%s)" id)))))

(defun pseudoscheme-buffer-substring-with-reified-output (start end)
  (let ((str-props (buffer-substring start end))
        (str-no-props (buffer-substring-no-properties start end)))
    (pseudoscheme-reify-old-output str-props str-no-props)))

(defun pseudoscheme-reify-old-output (str-props str-no-props)
  (let ((pos (pseudoscheme-property-position 'pseudoscheme-repl-presentation str-props)))
    (if (null pos)
        str-no-props
      (cl-multiple-value-bind (presentation start-pos end-pos whole-p)
          (pseudoscheme-presentation-around-point pos str-props)
        (if (not presentation)
            str-no-props
          (concat (substring str-no-props 0 pos)
                  ;; Eval in the reader so that we play nice with quote.
                  ;; -luke (19/May/2005)
                  "#." (pseudoscheme-presentation-expression presentation)
                  (pseudoscheme-reify-old-output (substring str-props end-pos)
                                          (substring str-no-props end-pos))))))))



(defun pseudoscheme-repl-grab-old-output (replace)
  "Resend the old REPL output at point.
If replace it non-nil the current input is replaced with the old
output; otherwise the new input is appended."
  (cl-multiple-value-bind (presentation beg end)
      (pseudoscheme-presentation-around-or-before-point (point))
    (pseudoscheme-check-presentation beg end (current-buffer) presentation)
    (let ((old-output (buffer-substring beg end))) ;;keep properties
      ;; Append the old input or replace the current input
      (cond (replace (goto-char pseudoscheme-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (let ((inhibit-read-only t))
        (insert old-output)))))

;;; Presentation-related key bindings, non-context menu

(defvar pseudoscheme-presentation-command-map nil
  "Keymap for presentation-related commands. Bound to a prefix key.")

(defvar pseudoscheme-presentation-bindings
  '((?i pseudoscheme-inspect-presentation-at-point)
    (?d pseudoscheme-describe-presentation-at-point)
    (?w pseudoscheme-copy-presentation-at-point-to-kill-ring)
    (?r pseudoscheme-copy-presentation-at-point-to-repl)
    (?p pseudoscheme-previous-presentation)
    (?n pseudoscheme-next-presentation)
    (?\  pseudoscheme-mark-presentation)))

(defun pseudoscheme-presentation-init-keymaps ()
  (pseudoscheme-init-keymap 'pseudoscheme-presentation-command-map nil t
		     pseudoscheme-presentation-bindings)
  (define-key pseudoscheme-presentation-command-map "\M-o" 'pseudoscheme-clear-presentations)
  ;; C-c C-v is the prefix for the presentation-command map.
  (define-key pseudoscheme-prefix-map "\C-v" pseudoscheme-presentation-command-map))

(defun pseudoscheme-presentation-around-or-before-point-p ()
  (cl-multiple-value-bind (presentation beg end)
      (pseudoscheme-presentation-around-or-before-point (point))
    presentation))

(defvar pseudoscheme-presentation-easy-menu
  (let ((P '(pseudoscheme-presentation-around-or-before-point-p)))
    `("Presentations"
      [ "Find Definition" pseudoscheme-M-.-presentation-at-point ,P ]
      [ "Inspect" pseudoscheme-inspect-presentation-at-point ,P ]
      [ "Describe" pseudoscheme-describe-presentation-at-point ,P ]
      [ "Pretty-print" pseudoscheme-pretty-print-presentation-at-point ,P ]
      [ "Copy to REPL" pseudoscheme-copy-presentation-at-point-to-repl ,P ]
      [ "Copy to kill ring" pseudoscheme-copy-presentation-at-point-to-kill-ring ,P ]
      [ "Mark" pseudoscheme-mark-presentation ,P ]
      "--"
      [ "Previous presentation" pseudoscheme-previous-presentation ]
      [ "Next presentation" pseudoscheme-next-presentation ]
      "--"
      [ "Clear all presentations" pseudoscheme-clear-presentations ])))

(defun pseudoscheme-presentation-add-easy-menu ()
  (easy-menu-define menubar-pseudoscheme-presentation pseudoscheme-mode-map "Presentations" pseudoscheme-presentation-easy-menu)
  (easy-menu-define menubar-pseudoscheme-presentation pseudoscheme-repl-mode-map "Presentations" pseudoscheme-presentation-easy-menu)
  (easy-menu-define menubar-pseudoscheme-presentation sldb-mode-map "Presentations" pseudoscheme-presentation-easy-menu)
  (easy-menu-define menubar-pseudoscheme-presentation pseudoscheme-inspector-mode-map "Presentations" pseudoscheme-presentation-easy-menu)
  (easy-menu-add pseudoscheme-presentation-easy-menu 'pseudoscheme-mode-map)
  (easy-menu-add pseudoscheme-presentation-easy-menu 'pseudoscheme-repl-mode-map)
  (easy-menu-add pseudoscheme-presentation-easy-menu 'sldb-mode-map)
  (easy-menu-add pseudoscheme-presentation-easy-menu 'pseudoscheme-inspector-mode-map))

;;; hook functions (hard to isolate stuff)

(defun pseudoscheme-dispatch-presentation-event (event)
  (pseudoscheme-dcase event
    ((:presentation-start id &optional target)
     (pseudoscheme-mark-presentation-start id target)
     t)
    ((:presentation-end id &optional target)
     (pseudoscheme-mark-presentation-end id target)
     t)
    (t nil)))

(defun pseudoscheme-presentation-write-result (string)
  (with-current-buffer (pseudoscheme-output-buffer)
    (let ((marker (pseudoscheme-repl-output-target-marker :repl-result))
          (saved-point (point-marker)))
      (goto-char marker)
      (pseudoscheme-propertize-region `(face pseudoscheme-repl-result-face
                                      rear-nonsticky (face))
        (insert string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point))
      (set-marker pseudoscheme-output-end (point))
      ;; Restore point before insertion but only it if was farther
      ;; than `marker'. Omitting this breaks REPL test
      ;; `repl-type-ahead'.
      (when (> saved-point (point))
        (goto-char saved-point)))
    (pseudoscheme-repl-show-maximum-output)))

(defun pseudoscheme-presentation-write (string &optional target)
  (cl-case target
    ((nil)                              ; Regular process output
     (pseudoscheme-repl-emit string))
    (:repl-result
     (pseudoscheme-presentation-write-result string))
    (t (pseudoscheme-repl-emit-to-target string target))))

(defun pseudoscheme-presentation-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer. Presentations of old results are expanded into code."
  (pseudoscheme-buffer-substring-with-reified-output (pseudoscheme-repl-history-yank-start)
                                              (if until-point-p
                                                  (point)
                                                (point-max))))

(defun pseudoscheme-presentation-on-return-pressed (end-of-input)
  (when (and (car (pseudoscheme-presentation-around-or-before-point (point)))
             (< (point) pseudoscheme-repl-input-start-mark))
    (pseudoscheme-repl-grab-old-output end-of-input)
    (pseudoscheme-repl-recenter-if-needed)
    t))

(defun pseudoscheme-clear-presentations ()
  "Forget all objects associated to PSEUDOSCHEME presentations.
This allows the garbage collector to remove these objects
even on Common Lisp implementations without weak hash tables."
  (interactive)
  (pseudoscheme-eval-async `(swank:clear-repl-results))
  (unless (eql major-mode 'pseudoscheme-repl-mode)
    (pseudoscheme-switch-to-output-buffer))
  (pseudoscheme-for-each-presentation-in-region 1 (1+ (buffer-size))
					 (lambda (presentation from to whole-p)
					   (pseudoscheme-remove-presentation-properties from to
										 presentation))))

(defun pseudoscheme-presentation-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (pseudoscheme-dcase ispec
      ((:value string id)
       (pseudoscheme-propertize-region
           (list 'pseudoscheme-part-number id
                 'mouse-face 'highlight
                 'face 'pseudoscheme-inspector-value-face)
         (pseudoscheme-insert-presentation string `(:inspected-part ,id) t)))
      ((:label string)
       (insert (pseudoscheme-inspector-fontify label string)))
      ((:action string id)
       (pseudoscheme-insert-propertized (list 'pseudoscheme-action-number id
                                       'mouse-face 'highlight
                                       'face 'pseudoscheme-inspector-action-face)
                                 string)))))

(defun pseudoscheme-presentation-sldb-insert-frame-variable-value (value frame index)
  (pseudoscheme-insert-presentation
   (sldb-in-face local-value value)
   `(:frame-var ,pseudoscheme-current-thread ,(car frame) ,index) t))

(defun pseudoscheme-presentations-on-connected ()
  (pseudoscheme-eval-async `(swank:init-presentations)))

(provide 'pseudoscheme-presentations)
