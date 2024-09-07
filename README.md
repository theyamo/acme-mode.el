# acme-mode.el --- asm-mode customized to work better with 6502/ACME syntax

Based on Emacs' standard asm-mode with following differences:

  * Coloring modified to 6502 assembly and ACME syntax more closely
  * Code column has by default an indentation of 20 spaces, allowing long label names without messing formatting
  * Comment column is by default fixed to 30 spaces right of code column
  * Proper indentation for code blocks
  * Backspace deletes a single char or all tabs and spaces until beginning of line
  * Kill-line (C-k) tries to preserve label if the point to the right of it
  * Variable assignments are formatted opinionatedly

# Installation

Clone/copy file to your emacs' elisp folder and add following:

```
(load "~/.emacs.d/acme-mode")
(add-to-list 'auto-mode-alist '("\\.\\(acme\\|a\\)\\'" . acme-mode))
```
Replace .emacs.d with dir of your choice.

# License

GNU GPL license.

