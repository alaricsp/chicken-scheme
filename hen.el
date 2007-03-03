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
;;     - the #csi##oblist and co are available from oblist library

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


;;; Code:

(defconst hen-version (substring "$Revision: 1.13 $" 11 -2)
 "$Id: hen.el,v 1.13 2004/11/22 22:36:11 flw Exp $

Report bugs to: Linh Dang <linhd@>")
(defvar hen-load-hook nil
 "*Hooks run after loading hen.")

(require 'scheme)
(require 'info-look)
(require 'compile)

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
                           "define-external-variable"
                           "define-foreign-type"
                           "define-foreign-variable"
                           "define-foreign-record"
                           "define-functor"
                           "define-generic"
                           "define-handy-method"
                           "define-inline"
                           "define-internal-meroon-macro"
                           "define-macro"
                           "define-method"
                           "define-optionals"
                           "define-reader-ctor"
                           "define-record"
                           "defstruct"
                           "define-record-printer"
                           "define-record-type"
                           "define-record-scheme"
                           "define-signature"
                           "define-structure"
                           "define-syntax"
                           "define-for-syntax"
                           "define-syntax-form"
                           "define-optimizer"
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
            '("begin" "begin0" "begin-form" "else"
              "call-with-current-continuation" "call/cc"
              "call-with-input-pipe" "call-with-output-pipe"
              "call-with-input-file" "call-with-output-file"
              "call-with-input-string" "call-with-output-string"
              "call-with-values"
              "else"
              "foreign-lambda*" "foreign-safe-lambda*" "foreign-primitive"
	      "foreign-declare"
              "foreign-lambda" "foreign-safe-lambda" "foreign-code"
              "match" "match-lambda" "match-lambda*" "match-define" "match-let" "match-let*"

              "case" "case-lambda" "cond" "cond-expand" "condition-case" "switch"
	      "handle-exceptions"
	      "record-compose"
              "cut" "cute" "time" "regex-case"

              "do" "else" "for-each" "if" "lambda" "when" "while" "if*" "unless"

	      "let-location" "location" "rec"
              "let" "let*" "let-syntax" "letrec" "letrec-syntax" "set!-values"
              "and-let*" "let-optionals" "let-optionals*" "let-macro"
              "fluid-let" "let-values" "let*-values" "letrec-values"
              "parameterize"
              "module" "import-only" "import" "import*"

              "and" "or" "delay" "andmap" "ormap" "receive"

              "assert" "ignore-errors" "critical-section" "ensure" "eval-when"

              "with-input-from-file" "with-output-to-file"
              "with-input-from-pipe" "with-output-to-pipe"
              "with-input-from-string" "with-output-to-string"

	      "loop"

              "declare" "include" "require-extension" "require" "require-for-syntax" "use" "quasiquote"

              "map" "syntax" "with-syntax" "syntax-case" "identifier-syntax" "syntax-rules") t)
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
     '((begin0 . 0) (begin-form . 0)

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
 (compile-internal (mapconcat 'identity (cons cmd args) " ")
                   "No more errors" "csc" nil
                   `(("Error:.+in line \\([0-9]+\\):" 0 1 nil ,(buffer-file-name)))
                   (lambda (ignored) "*csc*")))

(defun hen-build-unit ()
 (interactive)
 (let* ((file-name (file-name-nondirectory
                     (buffer-file-name)))
        (base-name (file-name-sans-extension file-name)))
   (hen-build "csc" (list "-s" file-name "-o" (concat base-name ".so")) )))

(defun hen-build-program ()
 (interactive)
 (let* ((file-name (file-name-nondirectory
                     (buffer-file-name)))
        (base-name (file-name-sans-extension file-name)))
   (hen-build "csc" (list file-name) )))

(define-derived-mode hen-mode scheme-mode "Hen"
 "Mode for editing chicken Scheme code.
\\[hen-complete-symbol] completes symbol base on the text at point.
\\[hen-csi-eval-last-sexp] evaluates the sexp at/preceding point in csi.
\\[hen-csi-eval-region] evaluates the region in csi.
\\[hen-csi-apropos] lists the csi's symbols matching a regex.
\\[hen-csi-send] reads a s-exp from the user and evaluates it csi.
\\[hen-describe-symbol] looks up info documentation for a symbol from.
the R5RS and Chicken info files.
\\[hen-build-unit] compiles the current file as a shared object
\\[hen-describe-symbol] compiles the current file as a program
"

 (set-syntax-table hen-syntax-table)
 (setq local-abbrev-table scheme-mode-abbrev-table)

 (define-key hen-mode-map (kbd "M-TAB")   'hen-complete-symbol)
 (define-key hen-mode-map (kbd "C-c C-e") 'hen-csi-eval-last-sexp)
 (define-key hen-mode-map (kbd "C-c C-r") 'hen-csi-eval-region)
 (define-key hen-mode-map (kbd "C-c C-a") 'hen-csi-apropos)
 (define-key hen-mode-map (kbd "C-c C-h") 'hen-describe-symbol)
 (define-key hen-mode-map (kbd "C-c C-l") 'hen-build-unit)
 (define-key hen-mode-map (kbd "C-c C-x") 'hen-csi-send)
 (define-key hen-mode-map (kbd "C-c C-l") 'hen-build-unit)
 (define-key hen-mode-map (kbd "C-c C-c") 'hen-build-program)

 (define-key hen-mode-map [menu-bar scheme run-scheme] nil)
 (define-key hen-mode-map [menu-bar shared build-prog] '("Compile File" hen-build-program))
 (define-key hen-mode-map [menu-bar shared send-to-csi] '("Evaluate" . hen-csi-send))
 (define-key hen-mode-map [menu-bar scheme build-as-unit] '("Compile File as Unit" . hen-build-unit))
 (define-key hen-mode-map [menu-bar scheme describe-sym] '("Lookup Documentation for Symbol" . hen-describe-symbol))
 (define-key hen-mode-map [menu-bar scheme apropos] '("Symbol Apropos" . hen-csi-apropos))
 (define-key hen-mode-map [menu-bar scheme eval-region] '("Eval Region" . hen-csi-eval-region))
 (define-key hen-mode-map [menu-bar scheme eval-last-sexp] '("Eval Last S-Expression" . hen-csi-eval-last-sexp))

 (setq font-lock-defaults
       '((hen-font-lock-keywords
          hen-font-lock-keywords-1 hen-font-lock-keywords-2)
         nil t
         ((?+ . "w") (?- . "w") (?* . "w") (?/ . "w")
          (?. . "w") (?< . "w") (?> . "w") (?= . "w")
          (?? . "w") (?$ . "w") (?% . "w") (?_ . "w")
          (?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun))))

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
   (setq msg (concat "wait for "
                     (process-name proc)
                     "'s prompt")))
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
   (> timeout 0)))

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

(defun hen-csi-buffer () (get-buffer-create " *csi*"))

(defconst hen-prompt-pattern "#;[0-9]*> ")

(defun hen-csi-proc ()
 (let ((proc (get-buffer-process (hen-csi-buffer))))
   (if (and (processp proc)
            (eq (process-status proc) 'run))
       proc
     (setq proc (start-process "csi" (hen-csi-buffer) "csi" "-no-init" "-quiet"))
     (with-current-buffer (hen-csi-buffer)
       (hen-proc-wait-prompt proc hen-prompt-pattern)
       ;(hen-proc-send "(require 'oblist)" proc hen-prompt-pattern)
       proc))))

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
          (buf (hen-proc-send (concat sexp "\n") proc hen-prompt-pattern))
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

(defun hen-csi-eval-region (beg end)
 "Evaluate the current region in CSI."
 (interactive "r")
 (message
  (hen-csi-send (buffer-substring beg end))))

(defun hen-csi-eval-last-sexp ()
 "Evaluate the s-expression at point in CSI"
 (interactive)
 (message
  (hen-csi-eval-region (save-excursion (backward-sexp) (point))
                       (point))))

(defun hen-csi-eval-definition ()
 "Evaluate the enclosing top-level form in CSI."
 (interactive)
 (save-excursion
   (message
    (hen-csi-eval-region (progn (beginning-of-defun) (point))
                         (progn (forward-sexp 1) (point))))))

(defun hen-csi-completions-alist (prefix)
 (read (hen-csi-send
        ;; this used to not work because srfi-1 and regex were needed. I don't want to
        ;; require them inside this expression because that would pollute the namespace and
        ;; the user will have no choice but to develop his programs with those libs in the
        ;; environment.
        ;; I should check the csi compilation code - why it doesn't statically link in regex
        ;; and srfi-1, since it seems to rely on them.
        (concat "(begin (require 'regex) (require 'srfi-1)"
                ;; TODO: this is an ugly hack that pollutes the namespace. should be done
                ;; in a seperate process, but process-fork requires posix, which would pollute the namespace too.
                ;; The solution is to statically link the required routines (either posix or srfi-1+regex) into the csi
                ;; executable.
                "(pp (map list (delete-duplicates (##csi#name-of-symbols-starting-with \""
                prefix
                "\")))))"))))

(defun hen-complete-symbol (thing)
 "Complete symbol at point in Hen mode. THING is used as the prefix."
 (interactive (list (hen-identifier-at-point)))
 (let* ((matching-names-alist (hen-csi-completions-alist thing))
        (completion (try-completion thing matching-names-alist))
        (show-box #'(lambda ()
                      (message "Making completion list...")
                      (with-output-to-temp-buffer "*Completions*"
                        (display-completion-list (all-completions thing matching-names-alist))))))
   (cond ((eq completion t) (message "exact match"))
         ((null completion)
          (error "Can't find completion for \"%s\"" thing))
         ((not (string= thing completion))
          (delete-region (progn (backward-sexp 1) (point))
                         (progn (forward-sexp 1) (point)))
          (insert completion)
          (if (null (cdr matching-names-alist)) (message "")
            (funcall show-box)))
         (t (funcall show-box)))))

;(defun hen-csi-try-complete (string ignore1 &optional ignore2)
;  (let ((matches (hen-csi-get-completions-alist string)))
;    (cond ((null matches) nil)
;          ((and (= (length matches) 1)
;                (string-equal (caar matches) string))
;           t)
;          (t (try-completion string matches)))))

(defun hen-csi-completion-table (str pred opcode)

 (let ((coll (if (equal str "") '() ; producing a list of everything would take too long.
               (hen-csi-completions-alist str))))
   (case opcode
     ('nil (try-completion str coll pred))
     ('t (all-completions str coll pred))
     ('lambda (and (or (null pred) (funcall pred str)) (assoc str coll)))
     (t (error "invalid opcode (%S) in read-completion" opcode)))))

(defvar hen-lookup-history nil)
(defsubst hen-csi-symbol-completing-read (prompt)
 (list (completing-read prompt 'hen-csi-completion-table
                        nil nil nil 'hen-lookup-history (hen-identifier-at-point))))

(defun hen-describe-symbol (name)
 "Lookup documentation for symbol NAME."
 (interactive (hen-csi-symbol-completing-read "Describe symbol: "))
 (info-lookup-symbol name 'hen-mode))

(defun hen-csi-apropos (regex)
 "List the symbols matching REGEX."
 (interactive "sApropos (chicken's global symbols): ")
 (with-current-buffer (get-buffer-create "*Chicken Apropos*")
   (widen)
   (erase-buffer)
   (let* ((query (concat "(pp (map\n"
                         "  (lambda (sym) (cons (->string sym)\n"
                         "      (->string (if (##sys#symbol-has-toplevel-binding? sym)\n "
                         "                 (##sys#slot sym 0) '<unbound> ))))\n"
                         "  (delete-duplicates! (##csi#symbols-matching \"" regex  "\"))))"))
          (results-alist (read (hen-csi-send query))))
     (if (display-mouse-p)
         (insert "If moving the mouse over text changes the text's color,\n"
                 (substitute-command-keys
                  "you can click \\[apropos-mouse-follow] on that text to get more information.\n")))
     (insert "In this buffer, go to the name of the command, or function,"
             " or variable,\n"
             (substitute-command-keys
              "and type \\[apropos-follow] to get full documentation.\n\n"))

     (dolist (item results-alist)
       (let ((name (car item))
             (obj (cdr item)))
         (insert (car item) " ")
         (add-text-properties (line-beginning-position) (1- (point))
                              `(item ,name action hen-describe-symbol
                                     face bold mouse-face highlight
                                     help-echo "mouse-2: display help on this item"))
         (indent-to-column 40)
         (insert (cdr item) "\n")))

     (apropos-mode)))
 (pop-to-buffer "*Chicken Apropos*" t))

(info-lookup-add-help
:mode 'hen-mode
:regexp "[^()'\" \t\n]+"
:ignore-case t
;; Aubrey Jaffer's rendition from <URL:ftp://ftp-swiss.ai.mit.edu/pub/scm>
:doc-spec '(("(chicken)Index" nil
             "^[ \t]+- [^:\n]+:[ \t]*" "")
            ("(r5rs)Index" nil
             "^[ \t]+- [^:\n]+:[ \t]*" "")))

(provide 'hen)
(run-hooks 'hen-load-hook)
;;; HEN.EL ends here
