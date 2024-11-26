;;; acme-mode.el --- asm-mode customized to work better with ACME syntax -*- lexical-binding: t; -*-


;; Author: theyamo
;; URL: https://github.com/theyamo/acme-mode.el

;; Licensed under GNU General Public License.

;;; Commentary:

;; This mode is based on Emacs' standard asm-mode with extensive customizations
;; to support 6502/ACME syntax better.
;; Also includes a helper function `acme-rename-label' to rename all instances
;; of a label in the buffer.  Another one `acme-clean-labels' tries to improve
;; labels by removing unnecessary tabs and appending ':' where one does not exist.

;; The whole indentation logic is a bit of a mess; some parts taken from asm-mode
;; with custom stuff around it, coded in a different style.

;; It's too complex for what it tries to achieve, but seems to work, so I consider
;; it good enough.

;; Known issues:
;; `acme-clean-labels' does not work very well.
;; and probably more.

;;; Code:

(defgroup acme-mode nil
  "Mode for editing Acme assembler code."
  :group 'languages)

(defconst acme-comment-char ?\;
  "The `comment-start' character assumed by Acme mode.")

(defcustom acme-code-column 24
  "The column where code is intended to."
  :type 'number
  :group 'acme-mode)

(defcustom acme-comment-column 50
  "The column where comments are intended to."
  :type 'number
  :group 'acme-mode)

(defcustom acme-indent-level-width 4
  "How many spaces are code blocks indented by."
  :type 'number
  :group 'acme-mode)

(defvar acme-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)              ;; Underscore is a word character
    (modify-syntax-entry ?a "w" st)              ;; Letters a-z are word characters
    (modify-syntax-entry ?A "w" st)              ;; Letters A-Z are word characters
    (modify-syntax-entry ?0 "w" st)              ;; Digits 0-9 are word characters
    
    ;; Mark all other characters as punctuation
    (modify-syntax-entry ?+ "." st)              ;; Treat '+' as punctuation
    (modify-syntax-entry ?- "." st)              ;; Treat '-' as punctuation
    (modify-syntax-entry ?= "." st)              ;; Treat '=' as punctuation
    (modify-syntax-entry ?< "." st)              ;; Treat '<' as punctuation
    (modify-syntax-entry ?> "." st)              ;; Treat '>' as punctuation
    (modify-syntax-entry ?& "." st)              ;; Treat '&' as punctuation
    (modify-syntax-entry ?% "." st)              ;; Treat '%' as punctuation
    
    
    (modify-syntax-entry ?\n "> b" st)
    ;; (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?\. "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?* "_" st)
    st)
  "Syntax table used while in Acme mode.")

(defvar acme-mode-abbrev-table nil
  "Abbrev table used while in Acme mode.")
(define-abbrev-table 'acme-mode-abbrev-table ())

(defvar acme-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Note that the comment character isn't set up until acme-mode is called.
    (define-key map ":"		'acme-colon)
    (define-key map "="		'acme-equals)
    (define-key map "\C-c;"	'comment-region)
    (define-key map "\C-m"	'newline-and-indent)
    (define-key map (kbd "<backspace>")	'acme-delete-char)
    ;; Also map DEL to acme-delete-char because some terminals send del instead of backspace
    (define-key map (kbd "DEL")	'acme-delete-char)
    (define-key map "\C-k"	'acme-kill-line-preserve-label)
    map)
  "Keymap for Acme mode.")

(defconst acme-expression-regexp
  "\\(\\sw\\|\\s_\\)+"
  "Rough matching for expressions (labels, operands, numbers).
Used for matching arguments.")

(defconst acme-label-regexp
  "\\([-\\+]+\\|[\\.\\@a-zA-Z_][a-zA-Z0-9_]*\\):?"
  "Acme label regexp for font locking.")

(defconst acme-opcode-regexp
  "\\(\\(\\sw\\)+\\(\\+[0-9]\\)?\\)"
  "Acme opcode regexp for font locking.")

(defconst acme-directive-regexp
  "\\(\\!\\_<\\sw+\\_>\\)"
  "Acme directive regexp for font locking.")

(defconst acme-function-regexp
  "\\(\\_<\\sw+\\_>\\)("
  "Acme function regexp for font locking.")

(defconst acme-constant-regexp
  "\\_<\\([$%]?[0-9A-Fa-f]+\\(\\.[0-9A-Fa-f]+\\)?\\)\\_>"
  "Acme numeric constant regexp for font locking.")

(defconst acme-font-lock-keywords
   `(
     ;; Label plus maybe instruction
     (,(concat "^" acme-label-regexp "\\s-*" acme-opcode-regexp "?")
      (1 'font-lock-function-name-face) (2 'font-lock-builtin-face nil t))
     ;; Instruction only
     (,(concat "^\\s-+" acme-opcode-regexp) 1 'font-lock-builtin-face)
     ;; Directive
     (,acme-directive-regexp . 'font-lock-keyword-face)
     ;; Function
     (,acme-function-regexp 1 'font-lock-keyword-face)
     ;; Program counter. Dumb matching pattern because it highlights wrong stuff too.
     ("*" . 'font-lock-keyword-face)
     ;; Immediate values
     ("#" . 'font-lock-type-face)
     ;; Indirect y
     (,(concat "\\((\\)" acme-expression-regexp "\\(),y\\)")
      (1 'font-lock-type-face)
      (3 'font-lock-type-face))
     ;; Indirect x
     (,(concat "\\((\\)" acme-expression-regexp "\\(,x)\\)")
      (1 'font-lock-type-face)
      (3 'font-lock-type-face))
     ;; Absolute x & y
     (,(concat acme-expression-regexp "\\(,[xy]\\)")
      2 'font-lock-type-face)
     ;; Numbers. Not using this, works too haphazardly due to syntax settings.
     ;;(,acme-constant-regexp . 'font-lock-constant-face)
     )
  "Additional expressions to highlight in Assembler mode.")

;;;###autoload
(define-derived-mode acme-mode prog-mode "ACME"
  "Major mode for editing ACME assembler code.

Turning on Acme mode runs the hook `acme-mode-hook' at the end of
initialization.

Special commands:
\\{acme-mode-map}"
  (kill-all-local-variables)
  (setq mode-name "Acme")
  (setq major-mode 'acme-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(acme-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'acme-indent-line)
  (set (make-local-variable 'tab-always-indent) t) ;; Tab just indents.
  (use-local-map (nconc (make-sparse-keymap) acme-mode-map))
  (local-set-key (vector acme-comment-char) 'acme-comment)
  (set-syntax-table (make-syntax-table acme-mode-syntax-table))
  (modify-syntax-entry acme-comment-char "< b")
  (make-local-variable 'comment-start)
  (setq comment-start (string acme-comment-char))
  (make-local-variable 'comment-add)
  (setq comment-add 1)
  (make-local-variable 'comment-start-skip)
  ;; TODO: this also matches C style comments, but leaving here them unless it causes problems
  (setq comment-start-skip "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (make-local-variable 'comment-end-skip)
  ;; TODO: this also matches C style comments, but leaving here them unless it causes problems
  (setq comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  ; 2024: affects region indentation
  (setq fill-prefix nil)
  (set (make-local-variable 'tab-width) acme-code-column)
  (set (make-local-variable 'comment-column) acme-comment-column)
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; Extract labels and constants into imenu
  (setq imenu-generic-expression '((nil "^\\([a-zA-Z0-9_]+\\)+" 1)))
  (run-mode-hooks 'acme-mode-hook))

(defun acme-indent-line ()
  "Indent current line as assembly code, forcing labels to column 0."
  (interactive)
  (let* ((savep (point)))
    (beginning-of-line)
    (cond
     ;; Does the line contain a label? (yet another new regexp for a label...)
     ((looking-at "^\\(\\.?\\(\\sw\\|\\s_\\)*\\s-*:\\)")
      (progn
        ;; Go to end of label
        (goto-char (match-end 1))
        ;; Executes the usual indentation rule.
        (delete-horizontal-space)
        (let ((indent-level-in-chars (acme-get-indent-level-for-point)))
          (indent-to indent-level-in-chars))))
     ;; Does the line contain a symbol assignment?
     ;; * is specifically disallowed even though it looks like an assignment
     ((looking-at "^\\([^*\n]\\.?\\(\\sw\\|\\s_\\)*\\s-*=\\)")
      (progn
        ;; Go on top of the equals char
        (goto-char (1- (match-end 1)))
        ;; Assignment always goes to code column; no indentation.
        (indent-to acme-code-column)))
     (t (progn
          ;; Everything else is indented according to usual indentation rules.
          (goto-char savep)
          (acme-do-indent-line))))
    ;; Verify that ; comment is in the comment column
    (save-excursion
      (let ((comment-pos (comment-search-forward (line-end-position) t)))
        (when comment-pos
           (goto-char comment-pos)
           (when (looking-at ";[^;]+")
             (indent-to acme-comment-column)))))))

(defun acme-do-indent-line ()
  "Perform the indentation.  Helper for `acme-indent-line'."
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (acme-calculate-indentation (point)) 0))
		     (error 0))))
    (if savep (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun acme-calculate-indentation (_)
  "Calculate indent level for current line."
  (let ((indent-level-in-chars (acme-get-indent-level-for-point)))
    (cond
     ;; directive or * operator
     ((or (looking-at (concat acme-directive-regexp "\\|\\*"))
          (looking-at "[{}]"))
      (if (= (current-column) 0) indent-level-in-chars 0))
     ;; Flush labels and symbols to the left margin.
     ;; [2024: to fix this, would need a list of mnemonics to differentiate between labels without ':' and opcodes]
     ((looking-at "\\(\\(\\.\\)?\\sw\\|\\s_\\)+\\([ \t]*=\\|:\\)") 0)
     ;; Same thing for `;;;' comments.
     ((looking-at "\\s<\\s<\\s<") 0)
     ;; Simple `;' comments go to the comment-column.
     ((looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     ;; The rest goes at the code column + indent level.
     (t indent-level-in-chars))))

;; This is rough: it walks through the entire buffer up until point, searching for braces. I haven't noticed
;; any remarkable slowdowns on 1000+ line sources.
(defun acme-get-indent-level-for-point ()
  "Return the indent level by counting opening and closing braces."
  (interactive)
  (let ((indent-level 0))
    (save-excursion
      (beginning-of-line)
      (let ((point-at-bol (point)))
        (goto-char (point-min))
        ;; Count braces up to the current line start
        (while (< (point) point-at-bol)
          (cond
           ((looking-at "{") (setq indent-level (1+ indent-level)))
           ((looking-at "}") (setq indent-level (1- indent-level))))
     (forward-char 1)))
      ;; Current line starts with a closing brace
      (back-to-indentation)
      (if (looking-at "}")
          (setq indent-level (max 0 (1- indent-level)))))
    ;; Calculate and return indentation column.
    (+ (* acme-indent-level-width indent-level) tab-width)))

;; This mostly taken from asm-mode; implementation is way different from the rest of the indentation.
(defun acme-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-syntax-backward "w_.")
      (skip-syntax-backward " ")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (call-interactively 'self-insert-command)
    (when labelp
      (delete-horizontal-space)
      (let ((indent-level-in-chars (acme-get-indent-level-for-point)))
        (indent-to indent-level-in-chars)))))

;; Obsolete since Emacs-22.1.
(defalias 'acme-newline 'newline-and-indent)

(defun acme-equals ()
  "Indent and insert '='."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9-_ ")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (when labelp
      (delete-horizontal-space)
      (tab-to-tab-stop))
    (call-interactively 'self-insert-command)))

(defun acme-delete-char ()
  "Delete char or all tabs and spaces until beginning of line."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion
                (skip-chars-backward " \t\r" 0)
                (point)))
         (delta (- beg end)))
    (if (= delta 0) (delete-char -1)
      (delete-char delta))))

(defun acme-rename-label ()
  "Rename all occurrences of a label in the source."
  (interactive)
  (save-excursion
        (let ((label (thing-at-point 'symbol)))
          (when label
            (let ((new-name (read-string (format "Rename label %s to: " label))))
              (goto-char (point-min))
              (when (and new-name (not (string-empty-p new-name)))
                (while (re-search-forward (concat "\\b" label "\\b") nil t)
                  (replace-match new-name t t nil 0))))))))

;; Below taken and modified from asm-mode.
(defun acme-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the `comment-column')
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger acme-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (comment-normalize-vars)
  (let (comempty comment)
    (save-excursion
      (beginning-of-line)
      (with-no-warnings
	(setq comment (comment-search-forward (line-end-position) t)))
      (setq comempty (looking-at "[ \t]*$")))

  (cond

   ;; Blank line?  Then start comment at code indent level.
   ;; Just like `comment-dwim'.  -stef
   ((save-excursion (beginning-of-line) (looking-at "^[ \t]*$"))
    (indent-according-to-mode)
    (insert acme-comment-char acme-comment-char ?\ ))

   ;; Nonblank line w/o comment => start a comment at comment-column.
   ;; Also: point before the comment => jump inside.
   ((or (null comment) (< (point) comment))
    (indent-for-comment))

   ;; Flush-left or non-empty comment present => just insert character.
   ((or (not comempty) (save-excursion (goto-char comment) (bolp)))
    (insert acme-comment-char))

   ;; Empty code-level comment => upgrade to next comment level.
   ((save-excursion (goto-char comment) (skip-chars-backward " \t") (bolp))
    (goto-char comment)
    (insert acme-comment-char)
    (indent-for-comment))

   ;; Empty comment ends non-empty code line => new comment above.
   (t
    (goto-char comment)
    (skip-chars-backward " \t")
    (delete-region (point) (line-end-position))
    (beginning-of-line) (insert "\n") (backward-char)
    (acme-comment)))))

(defun acme-kill-line-preserve-label ()
  "Kill line but preserve label on the current line."
  (interactive)
  (kill-line)
  (while (looking-at "[[:space:]]+")
    (delete-region (match-beginning 0) (match-end 0))))

;; This really doesn't work very well.
(defun acme-clean-labels ()
  "Convert tabs to spaces and add semicolon to labels with query-regexp."
  (interactive)
  ;; (beginning-of-buffer)
  (untabify (point-min) (point-max))
  (query-replace-regexp "^\\([-\\.\\+a-zA-Z_][a-zA-Z0-9_]*\\)\\(\\s-+\\)" "\\1:\\2"))

;; (defun acme-comment-box (start end)
;;   "Hack comment box to work properly."
;;   (interactive "r")
;;   (let ((current-prefix-arg '(3))
;;         (region-start start)
;;         (region-end end))
;;     (comment-box region-start region-end 2)
;;     (indent-region region-start region-end)
;;     ))

(provide 'acme-mode)

;;; acme-mode.el ends here
