;;; golfscript-mode.el --- A GolfScript editing mode    -*- lexical-binding: t; -*-

;;; Copyright (c) 2011-2023, kaki

;; Author: 2011-2023 kaki
;; Package-Requires: ((cl-lib "1.0") (rx))
;; Created: 8 Sep 2011
;; Version: 0.2.0
;; Keywords: languages, GolfScript

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(defconst golfscript-mode-version "0.2.0")

(defface golfscript-mode-assignment-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight assignment operators."
  :group 'golfscript-mode)

(defface golfscript-mode-block-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight start and end of blocks."
  :group 'golfscript-mode)

(defface golfscript-mode-builtin-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight built-in variables with alphabetic names."
  :group 'golfscript-mode)

(defface golfscript-mode-stack-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight built-in variables that are bound to stack operators."
  :group 'golfscript-mode)

(defface golfscript-mode-string-interpolation-placeholder-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight placeholders of string-interpolation."
  :group 'golfscript-mode)

(defvar golfscript-mode--builtin-variable-alist
  '(("n" 0 "newline")
    ("abs" 1 "abs")
    ("zip" 1 "Transpose array rows with columns.")
    ("base" 2 "X I: Unsigned base conversion.")
    ("rand" 1 "Push random integer up but not including.")
    ("if" 3 "X THEN ELSE: If true execute first, otherwise execute second.")
    ("and" 2 "Lazy boolean.")
    ("or" 2 "Lazy boolean.")
    ("xor" 2 "Lazy boolean.")
    ("print" 1 "Convert to a string and print.")
    ("p" 1 "Convert to a string using ` (inspect) and print followed by variable n.")
    ("puts" 1 "Convert to a string and print followed by variable n.")
    ("do" 1 "Execute a block, pop value, if this is true then continue.")
    ("while" 2 "COND BODY: Execute a condition block, pop a value, if true execute body block.")
    ("until" 2 "COND BODY: Execute a condition block, pop a value, if false execute body block.")))

(defvar golfscript-mode--builtin-command-table
  (let ((vec (make-vector 128 nil)))
    (cl-loop for (ch . spec)
             in '((?~ 1 "I: bitwise not  | S: eval  | B: funcall  | A: splice")
                  (?\` 1 "inspect")
                  (?! 1 "logical not")
                  (?@ 3 "X Y Z: rotate to Y Z X")
                  (?$ 1~2 "I: stack nth  | A: sort  | A B: sort-by")
                  (?+ coerce "I I: add  | A A: concat")
                  (?- coerce "I I: sub  | A A: set diff")
                  (?* order "I I: mul  | B I: iterate  | A I: cycle  | A A: join  | A B: fold")
                  (?/ order "I I: div  | A A: split  | A I: slices  | X B B: unfold  | A B: each")
                  (?% order "I I: mod  | A B: map  | A I: every nth element  | clean-split")
                  (?\| coerce "bitwise/setwise or")
                  (?& coerce "bitwise/setwise and")
                  (?^ coerce "bitwise/setwise xor")
                  (?\\ 2 "swap")
                  (?: 0 "assignment")
                  (?\; 1 "pop")
                  (?< order "X Y: less than  | A I: take")
                  (?> order "X Y: greater than  | A I: drop")
                  (?= order "X Y: equal to  | A I: element at index")
                  (?, 1~2 "I: iota  | A: length  | A B: filter")
                  (?. 1 "dup")
                  (?? order "I I: pow  | X A: find-index  | A B: find")
                  (?\( 1 "I: deincrement  | A: left uncons")
                  (?\) 1 "I: increment  | A: right uncons"))
             do (aset vec ch spec))
    (cl-loop for ch from ?a to ?z
             do (aset vec ch 'symbol)
             do (aset vec (upcase ch) 'symbol))
    (aset vec ?_ 'symbol)
    vec))

(defun golfscript-mode-info-at-point ()
  (let* ((ch (following-char))
         (thing (and (< ch 128)
                        (aref golfscript-mode--builtin-command-table ch))))
    (pcase (if (eq thing 'symbol)
               (assoc (thing-at-point 'symbol)
                      golfscript-mode--builtin-variable-alist)
             (cons (char-to-string ch) thing))
      (`(,name ,args ,info)
       (format "%s :: %s  # %s" name args info)))))

(rx-define golfscript-mode-string (delim)
  (seq delim (0+ (or (seq ?\\ anychar)
                     (not (in ?\\ delim))))
       delim))

(rx-define golfscript-mode-comment
  (seq ?#
       (0+ (not (in ?\r ?\n)))))

(defun golfscript-mode-search-string-interpolation-placeholder (end)
  (let ((state (syntax-ppss)))
    (while (and (not (eql ?\" (nth 3 state)))
                (< (point) end))
      (setq state
            (parse-partial-sexp (point) end nil nil state 'syntax-table)))
    (re-search-forward (rx "#"
                           (or (seq "{"
                                    (0+ (or (seq ?\\ anychar)
                                            (not (in ?\\ ?\"))))
                                    "}")
                               (seq "$"
                                    ;; Ruby interprets "#$0a" as "".
                                    (or (seq (1+ (syntax word)))
                                        ;; Ruby 1.8 accepts "#$-0".
                                        (seq ?- (in "0-9A-Za-z_"))
                                        ;; GolfScript rejects "#$"".
                                        (in "!$&'*-/:-@`~")))
                               (seq "@"
                                    (in "A-Za-z_")
                                    (0+ (syntax word)))))
                       end t)))

(defvar golfscript-mode-font-lock-keywords
  `((,(rx (submatch ?:)
          (submatch (or (1+ (syntax word))
                        (seq ?- (0+ (in "0-9")))
                        (syntax symbol)
                        (syntax whitespace)
                        golfscript-mode-comment
                        (golfscript-mode-string ?\")
                        (golfscript-mode-string ?\')
                        ?\[
                        ?\])))
     (1 'golfscript-mode-assignment-face)
     (2 font-lock-variable-name-face))
    (,(rx (or ?- word-start)
          (1+ (in "0-9")))
     . font-lock-constant-face)
    (,(rx word-start
          (regexp
           (regexp-opt
            (mapcar #'car golfscript-mode--builtin-variable-alist)))
          word-end)
     . 'golfscript-mode-builtin-face)
    ("[{}]" . 'golfscript-mode-block-face)
    ("[][@\\;.]" . 'golfscript-mode-stack-operator-face)
    (golfscript-mode-search-string-interpolation-placeholder
     (0 'golfscript-mode-string-interpolation-placeholder-face prepend))))

(defvar golfscript-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?\( "_" table)
    (modify-syntax-entry ?\) "_" table)
    (modify-syntax-entry ?, "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?\; "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?^ "_" table)
    (modify-syntax-entry ?` "_" table)
    (modify-syntax-entry ?~ "_" table)
    table))

(defalias 'golfscript-mode-syntax-propertize
  (syntax-propertize-rules
   ((rx ?\\)
    (0 "_"))
   ((rx (or golfscript-mode-comment
            (golfscript-mode-string ?\")
            (golfscript-mode-string ?\')))
    (0 nil))))

(defvar golfscript-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    map))

;;;###autoload
(define-derived-mode golfscript-mode prog-mode
  "GolfScript" "Major mode for GolfScript."
  (setq-local comment-start "# ")
  (setq font-lock-defaults
        `((golfscript-mode-font-lock-keywords)
          nil nil nil beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (setq-local syntax-propertize-function
              #'golfscript-mode-syntax-propertize)

  (setq-local require-final-newline nil)

  (setq-local eldoc-documentation-function
              #'golfscript-mode-info-at-point)
  (eldoc-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gs\\'" . golfscript-mode))

(provide 'golfscript-mode)

;;; golfscript-mode.el ends here
