;;; acme-mode.el --- asm-mode customized to work better with ACME syntax

;; Copyright (C) 2019 by Timo T.

;; Author: Timo T.
;; URL: https://github.com/theyamo/acme-mode.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This mode is based on Emacs' standard asm-mode with following differences:

;; - Coloring modified to 6502 asm syntax more closely
;; - Code column has fixed indentation of 20 spaces, allowing long label names in the first column
;; - Comment column is fixed to 26 spaces right of code column
;; - ! -directives are indented either to leftmost column or 4 spaces left of code
;; - Backspace deletes a single char or all tabs and spaces until beginning of line
;; - C-t is mapped to quick jump to code column

;;; Code:

(defgroup acme nil
  "Mode for editing Acme assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom acme-comment-char ?\;
  "*The comment-start character assumed by Acme mode."
  :type 'character
  :group 'acme)

(defvar acme-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in Acme mode.")

(defvar acme-mode-abbrev-table nil
  "Abbrev table used while in Acme mode.")
(define-abbrev-table 'acme-mode-abbrev-table ())

(defvar acme-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Note that the comment character isn't set up until acme-mode is called.
    (define-key map ":"		'acme-colon)
    (define-key map "\C-c;"	'comment-region)
    (define-key map "\C-t"	'jump-to-keyword-column)
    (define-key map "\C-m"	'newline-and-indent)
    (define-key map (kbd "<backspace>")	'acme-delete-char)
    ;; Also map DEL to acme-delete-char because some terminals send del instead of backspace
    (define-key map (kbd "DEL")	'acme-delete-char)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar acme-mode] (cons "Acme" map))
    (define-key map [comment-region]
      '(menu-item "Comment Region" comment-region
		  :help "Comment or uncomment each line in the region"))
    (define-key map [newline-and-indent]
      '(menu-item "Insert Newline and Indent" newline-and-indent
		  :help "Insert a newline, then indent according to major mode"))
    (define-key map [acme-colon]
      '(menu-item "Insert Colon" acme-colon
		  :help "Insert a colon; if it follows a label, delete the label's indentation"))
    (define-key map [jump-to-keyword-column]
      '(menu-item "Jump to keyword column" jump-to-keyword-column
		  :help ""))
    (define-key map [acme-delete-char]
      '(menu-item "Delete char" acme-delete-char
		  :help "Delete char or all tabs and spaces until beginning of line"))
    
    map)
  "Keymap for Acme mode.")

(defconst acme-font-lock-keywords
  (append 
   '(("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
      (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
     ;; label started from ".".
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:"
      1 font-lock-function-name-face)
     ("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
      2 font-lock-keyword-face)
     ;; directive started from "!".
     ("\\([\\!\\+]\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
      1 font-lock-keyword-face)
     ;; index registers
     (",[xy]"
      . font-lock-keyword-face)
     ;; immediate value, '#' -prefix
     ("#\\sw+" . font-lock-variable-name-face))
   cpp-font-lock-keywords)
  "Additional expressions to highlight in Assembler mode.")

;;;###autoload
(defun acme-mode ()
  "Major mode for editing ACME assembler code.
Features a private abbrev table and the following bindings:

\\[acme-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[acme-newline]\tnewline, then tab to next tab stop.
\\[acme-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`acme-comment-char' (which defaults to `?\\;').

Alternatively, you may set this variable in `acme-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Acme mode runs the hook `acme-mode-hook' at the end of initialization.

Special commands:
\\{acme-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Acme")
  (setq major-mode 'acme-mode)
  (setq local-abbrev-table acme-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(acme-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'acme-indent-line)
  ;; Stay closer to the old TAB behavior (was tab-to-tab-stop).
  (set (make-local-variable 'tab-always-indent) nil)

  (run-hooks 'acme-mode-set-comment-hook)
  ;; Make our own local child of acme-mode-map
  ;; so we can define our own comment character.
  (use-local-map (nconc (make-sparse-keymap) acme-mode-map))
  (local-set-key (vector acme-comment-char) 'acme-comment)
  (set-syntax-table (make-syntax-table acme-mode-syntax-table))
  (modify-syntax-entry	acme-comment-char "< b")

  (make-local-variable 'comment-start)
  (setq comment-start (string acme-comment-char))
  (make-local-variable 'comment-add)
  (setq comment-add 1)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (setq fill-prefix "\t")
  (set (make-local-variable 'tab-width) 20)
  (set (make-local-variable 'comment-column) 46)
  (set (make-local-variable 'tab-stop-list) '(20 46 50 54 58 62 66 70 74 78 82))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (run-mode-hooks 'acme-mode-hook))

(defun acme-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (acme-calculate-indentation (point)) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun acme-calculate-indentation (def-val)
  (or
   ;; !-directive
   (and (looking-at "\\!\\(\\sw\\)+\\|[{}]")
        (if (= (current-column) 0) (0)
          (- tab-width 4)))
   ;; Flush labels and symbols to the left margin.
   (and (looking-at "\\(\\(\\.\\)?\\sw\\|\\s_\\)+\\([ \t]*=\\|:\\)") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; Simple `;' comments go to the comment-column.
   (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at the first tab stop.
   (or (car tab-stop-list) tab-width)))

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
      (tab-to-tab-stop))))

;; Obsolete since Emacs-22.1.
(defalias 'acme-newline 'newline-and-indent)

(defun jump-to-keyword-column ()
  "Jump to keyword column."
  (interactive)
  (beginning-of-line)
  (skip-syntax-forward "w_.")
  (skip-syntax-forward " "))

(defun acme-delete-char ()
  "Delete char or all tabs and spaces until beginning of line."
  (interactive)
  (setq end (point))
  (save-excursion
    (skip-chars-backward " \t\r" 0)
    (setq beg (point)))
  (setq delta (- end beg))
  (if (= delta 0) (setq delta 1))
  (delete-backward-char delta))


(defun acme-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
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

(provide 'acme-mode)

;;; acme-mode.el ends here
