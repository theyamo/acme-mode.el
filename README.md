# acme-mode.el --- asm-mode customized to work better with ACME syntax

Based on Emacs' standard asm-mode with following differences:

  * Coloring modified to 6502 asm syntax more closely
  * Code column has fixed indentation of 20 spaces, allowing long label names in the first column
  * Comment column is fixed to 26 spaces right of code column
  * ! -directives are indented either to leftmost column or 4 spaces left of code
  * Backspace deletes a single char or all tabs and spaces until beginning of line
  * C-t is mapped to quick jump to code column

## Installation

Copy file to your emacs' elisp folder and add following:

```
(load "~/.emacs.d/acme-mode")
(add-to-list 'auto-mode-alist '("\\.acme\\'" . acme-mode))
```

Replace .emacs.d with dir of your choice.

## License

GPL3




