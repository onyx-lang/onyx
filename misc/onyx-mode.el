;;; lang/onyx/autoload/onyx.el -*- lexical-binding: t; -*-


;; onyx-mode.el - very basic onyx mode

(require 'cl)
(require 'rx)
(require 'js)

(defconst onyx-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst onyx-builtins
  '("cast" "it" "sizeof" "alignof" "typeof"))

(defconst onyx-keywords
  '("if" "elseif" "else" "do" "while" "for" "in" "switch" "case" "as" "struct" "enum" "union"
    "return" "continue" "break" "fallthrough" "defer" "macro" "package"
    "use" "interface" "where"))

(defconst onyx-constants
  '("null" "true" "false" "null_str" "null_proc"))

(defconst onyx-typenames
  '("u64" "u32" "u16" "u8"
    "i64" "i32" "i16" "i8"
    "f32" "f64" "str" "cstr" "dyn_str" "any" "type_expr"
    "bool" "void" "rawptr"
    "i8x16" "i16x8" "i32x4" "i64x2"
    "f32x4" "f64x2" "v128"))

(defun onyx-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun onyx-keywords-rx (keywords)
  "build keyword regexp"
  (onyx-wrap-word-rx (regexp-opt keywords t)))

(defconst onyx-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst onyx-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst onyx-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst onyx-font-lock-defaults
  `(
    ;; Keywords
    (,(onyx-keywords-rx onyx-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(onyx-keywords-rx onyx-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(onyx-keywords-rx onyx-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ("\\\'.*\\\'" . font-lock-string-face)

    ;; Numbers
    (,(onyx-wrap-word-rx onyx-number-rx) . font-lock-constant-face)

    ;; Types
    (,(onyx-keywords-rx onyx-typenames) 1 font-lock-type-face)
    (,onyx-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst onyx--defun-rx "\(.*\).*\{")

(defmacro onyx-paren-level ()
  `(car (syntax-ppss)))

(defun onyx-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at onyx--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun onyx-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (onyx-paren-level)))
    (while (and
            (not (onyx-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (onyx-paren-level))
      (while (>= (onyx-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (onyx-line-is-defun)
      (beginning-of-line)))

(defun onyx-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (onyx-paren-level)))
    (when (> orig-level 0)
      (onyx-beginning-of-defun)
      (end-of-line)
      (setq orig-level (onyx-paren-level))
      (skip-chars-forward "^}")
      (while (>= (onyx-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'onyx-parent-mode
 (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; imenu hookup
(add-hook 'onyx-mode-hook
      (lambda ()
        (setq imenu-generic-expression
          '(
            ("type" "^\\(.*:*.*\\) : " 1)
	    ("function" "^\\(.*\\) :: " 1)
	    ("struct" "^\\(.*\\) *:: *\\(struct\\)\\(.*\\){" 1)
	    )
        )
      )
)

;; NOTE: taken from the scala-indent package and modified for Onyx.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun onyx--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun onyx--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'onyx--indent-on-parentheses)
  )

(require 'compile)
(add-hook 'onyx-mode-hook (lambda ()
        (add-to-list 'compilation-error-regexp-alist 'onyx)
        (add-to-list 'compilation-error-regexp-alist-alist '(onyx "^(\\(.*\\):\\([0-9]+\\),\\([0-9]+\\)) \\(.*\\)" 1 2 3))
  ))

;; (add-hook 'onyx-mode-hook (lambda()
;;        (rainbow-delimiters-mode)
;; ))

;;;###autoload
(define-derived-mode onyx-mode onyx-parent-mode "Onyx"
 :syntax-table onyx-mode-syntax-table
 :group 'onyx
 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 (setq-local comment-start "/*")
 (setq-local comment-end "*/")
 (setq-local indent-line-function 'js-indent-line)
 (setq-local font-lock-defaults '(onyx-font-lock-defaults))
 (setq-local beginning-of-defun-function 'onyx-beginning-of-defun)
 (setq-local end-of-defun-function 'onyx-end-of-defun)

 ;; add indent functionality to some characters
 (onyx--add-self-insert-hooks)

 (display-line-numbers-mode nil)
 (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.onyx\\'" . onyx-mode))

(provide 'onyx-mode)
