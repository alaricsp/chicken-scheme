;;; HEN.EL ---  mode for editing chicken code

;; Copyright (C) 2004 Linh Dang

;; Author: Linh Dang <linhd@>
;; Maintainer: Linh Dang <linhd@>
;; Created: 19 Apr 2004
;; Version: 1
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <linhd@>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;; LCD Archive Entry:
;; hen|Linh Dang|<linhd@>
;; | mode for editing chicken code
;; |$Date: 2004/11/22 22:36:11 $|$Revision: 1.13 $|~/packages/hen.el

;;; Commentary:
;; Hen is a mode derived from scheme-mode and is specialized for
;; editing chicken scheme.
;; This mode assumes:
;;     - the user has chicken.info install
;;     - the csi executable can be launch as "csi"

;;
;; Changes by Micky Latowicki:
;;
;; * Added implementation of with-temp-message, which is missing from xemacs 21.4.
;; * Added trivial display-mouse-p, which is similarly missing.
;; * fixed font-lock problems.
;; * removed most calls to accept-process-output, which made
;; hen unacceptably slow.
;; * removed (apparently) redundant call to hen-proc-wait-prompt in
;; hen-proc-send
;; * updated prompt regexp pattern to include the running number.
;; * start csi with -quiet
;; * fixed completions, made them more like emacs lisp behaviour.
;; Note: completions were fixed at the cost of feeding csi the commands
;; (require 'srfi-1) and (require 'regex) before matching strings are
;; searched for. This was done because the completions-searching code
;; relies on these libraries. A true fix would be to statically link these
;; libraries into csi, because the way it works now the user cannot choose
;; to keep srfi-1 and regex out of her csi environment.

;; Changes by felix:
;;
;; * removed hen-describe-symbol
;; * various cleaning up
;; * still pretty bad...

;; Changes by Adhi Hargo:
;; 
;; * automatically raise *csi* buffer on any relevant operations, and
;;   made it a read-only buffer.
;; * changes definition-at-point evaluation command.
;; * s-exp evaluation no longer shown in minibuffer.
;; * added : + Hen-mode customization group.
;;           + Buffer evaluation command.
;;           + csi process-terminating command, partly so I can erase
;;             previous definitions and start anew.
;;           + close-parens-at-point command, from SLIME.
;;           + modification-check before compilation.

;;; Code:

(defconst hen-version (substring "$Revision: 1.13 $" 11 -2)
 "$Id: hen.el,v 1.13 2004/11/22 22:36:11 flw Exp $

Report bugs to: Felix Winkelmann <bunny351@gmail.com>")

(require 'scheme)
(require 'compile)

;;; GROUP DECLARATION ================================================

(defgroup hen nil
  "Major mode for editing Scheme programs using Chicken."
  :version "21.3"
  :group 'scheme
  :prefix "hen-")
(defgroup hen-font-face nil
  "Various font face configurations."
  :group 'hen)

(defun hen-version ()
  "Outputs Hen's current version to the minibuffer."
  (interactive)
  (message "Hen %s" hen-version))

;;; USER-CONFIGURABLE COMMANDS =======================================

(defcustom hen-csc-program "csc"
  "*Chicken compiler executable's filename."
  :group 'hen
  :type 'string)
(defcustom hen-csi-program "csi"
  "*Chicken interpreter executable's filename."
  :group 'hen
  :type 'string)
(defcustom hen-build-exec-arg ""
  "*Compiler-argument when building an executable file."
  :group 'hen
  :type 'string)
(defcustom hen-build-obj-arg ""
  "*Compiler-argument when building an object file."
  :group 'hen
  :type 'string)
(defcustom hen-eval-init-arg ""
  "*Additional interpreter argument."
  :group 'hen
  :type 'string)

(defcustom hen-autosave-buffer-before-compile nil
  "*Save modified file automatically before compilation.
The default behavior is to ask the user whether to save or not."
  :group 'hen
  :type 'boolean)

(defcustom hen-load-hook nil
  "Hook run after entering Hen mode."
  :group 'hen
  :type 'hook)


;; with-temp-message pasted from a mailing list. It's not available in my xemacs 21.4
(unless (functionp 'with-temp-message)
 (defmacro with-temp-message (message &rest body)
   "Display MESSAGE temporarily while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY."
   (let ((current-message (make-symbol "current-message"))
         (temp-message (make-symbol "with-temp-message")))
     `(let ((,temp-message ,message)
            (,current-message))
        (unwind-protect
            (progn
              (when ,temp-message
                (setq ,current-message (current-message))
                (message "%s" ,temp-message))
              ,@body)
          (and ,temp-message ,current-message
               (message "%s" ,current-message)))))))

;; display-mouse-p not available in xemacs 21.4, so here's a quick fix, sort of.
(unless (functionp 'display-mouse-p)
 (defun display-mouse-p (&optional display) t))

(defconst hen-syntax-table
 (let ((tab (copy-syntax-table scheme-mode-syntax-table)))
   (modify-syntax-entry ?# "_   " tab)
   (modify-syntax-entry ?: "_   " tab)
   (modify-syntax-entry ?\[ "(]  " tab)
   (modify-syntax-entry ?\] ")[  " tab)

   tab))

(defconst hen-font-lock-keywords-1
 (eval-when-compile
   (list
    ;; Declarations
    (list (concat "\\(?:(\\|\\[\\)"
                  "\\(" (regexp-opt
                         '("define"
                           "define-class"
                           "define-external"
                           "define-constant"
                           "define-datatype"
                           "define-foreign-type"
                           "define-foreign-variable"
                           "define-foreign-record"
                           "define-generic"
                           "define-inline"
                           "define-macro"
                           "define-method"
                           "define-reader-ctor"
                           "define-record"
                           "defstruct"
                           "define-record-printer"
                           "define-record-type"
			   "define-compiler-macro"
                           "define-syntax"
                           "define-for-syntax"
                           "define-values") 1) "\\)"
                           "\\s-+(?\\(\\(\\sw\\|\\s_\\)+\\)")

          '(1 font-lock-keyword-face t t)
          '(2 font-lock-function-name-face t t))))
 "Basic font-locking for Hen mode.")

(defconst hen-font-lock-keywords-2
 (append hen-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Control structures.
     (cons
      (concat
       "\\<" (regexp-opt
            '("begin" "begin0" "else"
              "else"
              "foreign-lambda*" "foreign-safe-lambda*" "foreign-primitive"
	      "foreign-declare" "foreign-parse" "foreign-parse/declare"
              "foreign-lambda" "foreign-safe-lambda" "foreign-code"
              "match" "match-lambda" "match-lambda*" "match-define" "match-let" "match-let*"

              "case" "case-lambda" "cond" "cond-expand" "condition-case" "select"
	      "handle-exceptions"
              "cut" "cute" "time" "regex-case"

              "do" "else" "if" "lambda" "when" "while" "if*" "unless"

	      "let-location" "location" "rec"
              "let" "let*" "let-syntax" "letrec" "letrec-syntax" "set!-values"
              "and-let*" "let-optionals" "let-optionals*" "optional"
              "fluid-let" "let-values" "let*-values" "letrec-values"
              "parameterize"
              "module" "import-only" "import" "import*"

              "and" "or" "delay" "receive"

              "assert" "ignore-errors" "ensure" "eval-when"

	      "loop" "sc-macro-transformer"

              "declare" "include" "require-extension" "require" "require-for-syntax" "use" "quasiquote"

              "syntax" "with-syntax" "syntax-case" "identifier-syntax" "syntax-rules") t)
       "\\>") 'font-lock-keyword-face)
     '("\\<set!" . font-lock-keyword-face)
     ;;
     ;;  `:' keywords as builtins.
     '("#?\\<:\\sw+\\>" . font-lock-builtin-face)
     '("\\<\\sw+:\\>" . font-lock-builtin-face)
     '(",@?\\|`" . font-lock-builtin-face)
     '("\\(##\\sw+#\\)" (1 font-lock-builtin-face t nil))
     '("#\\\\?\\sw+"  (0 font-lock-constant-face nil t))
;?      '("(\\(declare\\|require\\(-extension\\)?\\)" . font-lock-keyword-face)
     )))
 "Gaudy expressions to highlight in Hen mode.")

(defconst hen-font-lock-keywords hen-font-lock-keywords-2)

(mapc (lambda (cell)
       (put (car cell) 'scheme-indent-function (cdr cell)))
     '((begin0 . 0)

       (when . 1) (while . 1) (unless . 1)
       (and-let* . 1) (fluid-let . 1)

       (call-with-input-pipe . 1)
       (call-with-ouput-pipe . 1)
       (call-with-input-string . 1)
       (call-with-input-string . 1)

       (call-with-values . 1)

       (with-input-from-pipe . 1)
       (with-ouput-to-pipe . 0)
       (with-input-from-string . 1)
       (with-output-to-string . 0)

       (if* . 2)))

(defun hen-identifier-at-point ()
 "Return the identifier close to the cursor."
 (save-excursion
   (save-match-data
     (let ((beg (line-beginning-position))
           (end (line-end-position))
           (pos (point)))
     (cond ((progn (goto-char pos)
                   (skip-chars-forward " \t" end)
                   (skip-syntax-backward "w_" beg)
                   (memq (char-syntax (following-char)) '(?w ?_)))
            (buffer-substring-no-properties (point) (progn (forward-sexp 1) (point))))
           ((progn (goto-char pos)
                   (skip-chars-backward " \t" beg)
                   (skip-syntax-forward "w_" end)
                   (memq (char-syntax (preceding-char)) '(?w ?_)))
            (buffer-substring-no-properties (point) (progn (forward-sexp -1) (point))))
           (t nil))))))

(defun hen-build (cmd args)
  (when (and (buffer-modified-p)
	     (or hen-autosave-buffer-before-compile
		 (progn (beep)
			(y-or-n-p "File modified. Save it? "))))
    (save-buffer))
 (compile-internal (mapconcat 'identity (cons cmd args) " ")
                   "No more errors" "csc" nil
                   `(("Error:.+in line \\([0-9]+\\):" 0 1 nil ,(buffer-file-name)))
                   (lambda (ignored) "*csc*")))

(defun hen-build-extension ()
  (interactive)
  (let* ((file-name (file-name-nondirectory
		     (buffer-file-name))))
    (hen-build hen-csc-program (list "-s" file-name hen-build-obj-arg))))

(defun hen-build-program ()
 (interactive)
 (let* ((file-name (file-name-nondirectory
                     (buffer-file-name))))
    (hen-build hen-csc-program (list file-name hen-build-exec-arg))))

(define-derived-mode hen-mode scheme-mode "Hen"
 "Mode for editing chicken Scheme code.
\\[hen-csi-eval-last-sexp] evaluates the sexp at/preceding point in csi.
\\[hen-csi-eval-region] evaluates the region in csi.
\\[hen-csi-eval-buffer] evaluates current buffer in csi.
\\[hen-csi-eval-definition] evaluates the toplevel definition at point in csi.
\\[hen-csi-send] reads a sexp from the user and evaluates it csi.
\\[hen-csi-proc-delete] terminates csi subprocess.
\\[hen-close-parens-at-point] closes parentheses for top-level sexp at point.
\\[hen-build-extension] compiles the current file as a shared object
\\[hen-build-program] compiles the current file as a program
"

 (set-syntax-table hen-syntax-table)
 (setq local-abbrev-table scheme-mode-abbrev-table)

  (define-key hen-mode-map (kbd "C-c C-e") 'hen-csi-eval-last-sexp)
  (define-key hen-mode-map (kbd "C-c C-r") 'hen-csi-eval-region)
  (define-key hen-mode-map (kbd "C-c C-b") 'hen-csi-eval-buffer)
  (define-key hen-mode-map (kbd "C-c C-d") 'hen-csi-eval-definition)
  (define-key hen-mode-map (kbd "C-c C-l") 'hen-build-unit)
  (define-key hen-mode-map (kbd "C-c C-x") 'hen-csi-send)
  (define-key hen-mode-map (kbd "C-c C-q") 'hen-csi-proc-delete)
  (define-key hen-mode-map (kbd "C-c C-l") 'hen-build-extension)
  (define-key hen-mode-map (kbd "C-c C-c") 'hen-build-program)
  (define-key hen-mode-map (kbd "C-c C-]") 'hen-close-parens-at-point)

  (define-key hen-mode-map [menu-bar scheme run-scheme] nil)
  (define-key hen-mode-map [menu-bar shared build-prog] '("Compile File" hen-build-program))
  (define-key hen-mode-map [menu-bar shared send-to-csi] '("Evaluate" . hen-csi-send))
  (define-key hen-mode-map [menu-bar scheme build-as-extension]
    '("Compile File as Extension" . hen-build-extension))
  (define-key hen-mode-map [menu-bar scheme eval-buffer] '("Eval Buffer" . hen-csi-eval-buffer))
  (define-key hen-mode-map [menu-bar scheme eval-region] '("Eval Region" . hen-csi-eval-region))
  (define-key hen-mode-map [menu-bar scheme eval-last-sexp]
    '("Eval Last S-Expression" . hen-csi-eval-last-sexp))

  (setq font-lock-defaults
	'((hen-font-lock-keywords
	   hen-font-lock-keywords-1 hen-font-lock-keywords-2)
	  nil t
	  ((?+ . "w") (?- . "w") (?* . "w") (?/ . "w")
	   (?. . "w") (?< . "w") (?> . "w") (?= . "w")
	   (?? . "w") (?$ . "w") (?% . "w") (?_ . "w")
	   (?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))
	  beginning-of-defun
	  (font-lock-mark-block-function . mark-defun)))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)

  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;;;* \\|(")

  (make-local-variable 'comment-start)
  (setq comment-start ";")

  (make-local-variable 'comment-column)
  (setq comment-column 40)

  (make-local-variable 'comment-add)
  (setf comment-add 1)
  )

;;stolen from cxref
(defun hen-looking-backward-at (regexp)
 "Return t if text before point matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
 (save-excursion
   (let ((here (point)))
     (if (re-search-backward regexp (point-min) t)
         (if (re-search-forward regexp here t)
             (= (point) here))))))

(defun hen-proc-wait-prompt (proc prompt-re &optional timeout msg)
  "Wait for the prompt of interactive process PROC. PROMPT-RE must be
a regexp matching the prompt. TIMEOUT is the amount of time to wait in
secs before giving up. MSG is the message to display while waiting."
  (setq timeout (if (numberp timeout) (* timeout 2) 60))
  (unless (stringp msg)
    (setq msg (concat "wait for " hen-csi-proc-name "'s prompt")))
  (goto-char (process-mark proc))
  (if (hen-looking-backward-at prompt-re)
      t
    (while (and (> timeout 0) (not (hen-looking-backward-at prompt-re)))
      (with-temp-message (setq msg (concat msg "."))
	(accept-process-output proc 0 timeout))
      (setq timeout (1- timeout))
      (goto-char (process-mark proc)))
    (with-temp-message (concat msg (if (> timeout 0)
				       " got it!" " timeout!"))
      (sit-for 0 100))
    (> timeout 0))
  )

(defun hen-proc-send (question proc prompt-re &optional timeout msg)
 "Send the string QUESTION to interactive process proc. PROMPT-RE is
the regexp matching PROC's prompt. TIMEOUT is the amount of time to
wait in secs before giving up. MSG is the message to display while
waiting."
  (setq timeout (if (numberp timeout) (* timeout 2) 60))
  (save-excursion
    (set-buffer (process-buffer proc))
    (widen)
    (save-match-data
      (goto-char (process-mark proc))
      (if (hen-looking-backward-at prompt-re)
	  (let ((start (match-end 0)))
	    (narrow-to-region start (point-max))
	    (process-send-string proc (concat question "\n"))
	    (hen-proc-wait-prompt proc prompt-re timeout msg)
	    (narrow-to-region start (match-beginning 0))
	    (current-buffer))))))

(defconst hen-csi-prompt-pattern "#;[0-9]*> ")
(defconst hen-csi-proc-name "csi")
(defconst hen-csi-buffer-name "*csi*")

(defun hen-csi-buffer-create ()
  "Creates a new buffer for csi, make it read-only."
  (let ((buffer (get-buffer-create hen-csi-buffer-name)))
    (with-current-buffer buffer
      (make-local-variable 'buffer-read-only)
      (setf buffer-read-only t))
    buffer))

(defun hen-csi-buffer-erase ()
  "Erases csi buffer's content, used mainly when its process was being
reset."
  (let ((buffer (get-buffer hen-csi-buffer-name)))
    (unless (null buffer) (with-current-buffer buffer
			    (setf buffer-read-only '())
			    (erase-buffer)
			    (setf buffer-read-only t)))))

(defun hen-csi-buffer ()
  (let ((buffer (or (get-buffer hen-csi-buffer-name) ;check if exists
		    (hen-csi-buffer-create)))) ;... or create one
    (display-buffer buffer)
    buffer))

(defun hen-csi-proc ()
  (let ((proc (get-process hen-csi-proc-name)))
    (if (and (processp proc)
	     (eq (process-status proc) 'run))
	proc
      (setq proc
	    (eval `(start-process hen-csi-proc-name (hen-csi-buffer)
				  hen-csi-program
				  "-no-init" "-quiet" "-:c" "-R" "srfi-1" "-R" "regex" "-R" "utils"
				  ,@(split-string hen-eval-init-arg))))
      (with-current-buffer (hen-csi-buffer)
	(hen-proc-wait-prompt proc hen-csi-prompt-pattern)
	proc))))

(defun hen-csi-proc-delete ()
  (interactive)
  (let ((proc (get-process hen-csi-proc-name)))
    (when (and (processp proc)
	       (eq (process-status proc) 'run))
      (delete-process proc))
    (hen-csi-buffer-erase)
    ()))

(defun hen-csi-send (sexp)
  "Evaluate SEXP in CSI"
  (interactive
   (let ((sexp (read-string "Evaluate S-expression: "))
	 (send-sexp-p nil))
     (unwind-protect
	 (progn
	   (let ((obarray (make-vector 11 0)))
	     (read sexp)
	     (setq send-sexp-p t)))
       (unless send-sexp-p
	 (setq send-sexp-p
	       (y-or-n-p (format "`%s' is not a valid sexp! evaluate anyway? " sexp)))))
     (list (if send-sexp-p sexp nil))))
  (when (stringp sexp)
    (let* ((proc (hen-csi-proc))
	   (buf (hen-proc-send (concat sexp "\n") proc hen-csi-prompt-pattern))
	   result len)
      (unless (buffer-live-p buf)
	(error "Internal hen-mode failure"))

      (save-excursion
	(with-current-buffer buf
	  (setq result (buffer-string))
	  (setq len (length result))
	  (if (and (> len 0)
		   (eq (aref result (1- len)) ?\n))
	      (setq result (substring result 0 -1)))
	  result)))))

(defun hen-csi-eval-buffer ()
  "Evaluate the current buffer in CSI"
  (interactive)
  (hen-csi-send (buffer-string)))

(defun hen-csi-eval-region (beg end)
  "Evaluate the current region in CSI."
  (interactive "r")
  (hen-csi-send (buffer-substring beg end)))

(defun hen-csi-eval-last-sexp ()
  "Evaluate the s-expression at point in CSI"
  (interactive)
  (hen-csi-eval-region (save-excursion (backward-sexp) (point))
		       (point)))

(defun hen-csi-eval-definition ()
  "Evaluate the enclosing top-level form in CSI."
  (interactive)
  (hen-csi-eval-region (save-excursion
			 (end-of-defun) (beginning-of-defun)
			 (point))
		       (save-excursion
			 (end-of-defun) (point))))

;; from SLIME
(defun hen-close-parens-at-point ()
  "Close parenthesis at point to complete the top-level-form.  Simply
inserts ')' characters at point until `beginning-of-defun' and
`end-of-defun' execute without errors, or internal variable
`close-parens-limit' is exceeded."
  (interactive)
  (let ((close-parens-limit 16))
    (loop for i from 1 to close-parens-limit
	  until (save-excursion
		  (beginning-of-defun)
		  (ignore-errors (end-of-defun) t))
	  do (insert ")"))))

(provide 'hen)
(run-hooks 'hen-load-hook)
;;; HEN.EL ends here
