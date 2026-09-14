;;; fff-dumb-jump.el --- Provides dumb-jump backend using the fff index -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: fff.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (dumb-jump "0.5"))
;; Keywords: matching, tools
;; URL: https://github.com/dmtrKovalenko/fff

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides dumb-jump backend using the fff index.
;; (fff-dumb-jump-setup) routes jumps through the index.
;; (fff-dumb-jump-teardown) removes it.
;; PCRE2-only patterns and off-root paths fall back to shell.

;;; Code:

(require 'fff)
(require 'dumb-jump)

(declare-function fff--grep-raw-ex "fff-core" (id query mode max-matches time-budget-ms
                                                   max-per-file max-file-size enforce-budget
                                                   smart-case))

(defun fff-dumb-jump--references-p ()
  "Non-nil in dumb-jump find-references mode.
Old versions lack the mode and always search definitions."
  (and (boundp 'dumb-jump--search-mode)
       (eq dumb-jump--search-mode 'references)))

(defgroup fff-dumb-jump nil
  "dumb-jump rules over the fff index."
  :group 'fff
  :group 'dumb-jump)

(defcustom fff-dumb-jump-enabled t
  "Route dumb-jump through fff while setup advice is active."
  :type 'boolean)

(defcustom fff-dumb-jump-fallback-to-shell t
  "Fall back to shell for patterns fff cannot run natively.
Covers `fff-dumb-jump--pcre2-only-p' constructs and uppercase
symbols in dumb-jump's case-insensitive languages. When nil,
error instead."
  :type 'boolean)

(defconst fff-dumb-jump--pcre2-only-re
  (rx (or (seq "(?" (any "=!<>"))
          "(?P="
          "(?("
          (seq "\\" (or "k" "g" (any "0-9")))
          ;; `[]' is literal `]' in PCRE2 (D function rule), an error in
          ;; Rust regex. Escaped `\\[\\]' is valid Rust, skip it.
          (seq (or string-start (not (any "\\"))) "[]")))
  "PCRE2 constructs the Rust engine rejects: look-around, atomic groups,
conditionals, backrefs (`\\1'-`\\9', `\\k<', `\\g{}', `(?P=name'), `[]'.
Named groups (`(?P<name>') are fine and not matched.")

(defun fff-dumb-jump--bad-brace-p (pattern)
  "Non-nil if PATTERN has a `{' Rust regex rejects.
Skips `\\X' escapes and `[...]' classes, where braces are literal.
Other unescaped `{' must open `{M}', `{M,}', or `{M,N}'. PCRE2
reads the rest literally, Rust errors (fff then degrades to literal)."
  (let ((len (length pattern)) (i 0) (in-class nil))
    (catch 'bad
      (while (< i len)
        (let ((c (aref pattern i)))
          (cond
           ((= c ?\\)
            (setq i (+ i 2)))
           ((and in-class (= c ?\]))
            (setq in-class nil)
            (setq i (1+ i)))
           ((and (not in-class) (= c ?\[))
            (setq in-class t)
            (setq i (1+ i)))
           ((and (not in-class) (= c ?\{))
            (if (string-match "\\`{[0-9]+\\(,[0-9]*\\)?}"
                              (substring pattern i))
                (setq i (+ i (match-end 0)))
              (throw 'bad t)))
           (t (setq i (1+ i))))))
      nil)))

(defun fff-dumb-jump--pcre2-only-p (pattern)
  "Non-nil if PATTERN needs PCRE2 and fff cannot run it."
  (and (stringp pattern)
       (or (string-match-p fff-dumb-jump--pcre2-only-re pattern)
           (fff-dumb-jump--bad-brace-p pattern))))

(defun fff-dumb-jump--for-project-p (proj)
  "Non-nil if PROJ is the current buffer's project root.
Other paths keep shell search, so first use never starts a scan."
  (and (stringp proj)
       (not (file-remote-p proj))
       (file-directory-p proj)
       (equal (directory-file-name (expand-file-name proj))
              (directory-file-name (fff-project-root)))))

(defun fff-dumb-jump--insensitive-lang-p (lang)
  "Non-nil if LANG searches case-insensitively in stock dumb-jump.
Mirrors the `--ignore-case' languages (`commonlisp', `cobol')."
  (and (boundp 'dumb-jump--case-insensitive-languages)
       (member lang dumb-jump--case-insensitive-languages)))

(defun fff-dumb-jump--has-upper-p (pattern)
  "Non-nil if PATTERN holds an uppercase letter.
Same test the index applies for smart-case: all-lowercase
patterns match insensitively when smart-case is on."
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" pattern)))

(defun fff-dumb-jump--smart-arg (lang pattern)
  "Module smart-case flag (1/0) for LANG and PATTERN.
Case-sensitive languages always search sensitively (0), like
stock dumb-jump without `--ignore-case'. Insensitive languages
search smart (1): an all-lowercase PATTERN then matches
insensitively, exactly `--ignore-case'. Uppercase patterns in
insensitive languages cannot run on the index, see
`fff-dumb-jump--shell-only-p'."
  (if (and (fff-dumb-jump--insensitive-lang-p lang)
           (not (fff-dumb-jump--has-upper-p pattern)))
      1
    0))

(defun fff-dumb-jump--shell-only-p (patterns lang)
  "Reason PATTERNS for LANG must run on shell search, or nil.
PCRE2-only constructs (see `fff-dumb-jump--pcre2-only-p') never
compile on the Rust engine. Uppercase patterns in dumb-jump's
case-insensitive languages need `--ignore-case', which the index
cannot express: smart-case turns sensitive on uppercase."
  (cond ((seq-some #'fff-dumb-jump--pcre2-only-p patterns)
         "unrunnable pattern")
        ((and (fff-dumb-jump--insensitive-lang-p lang)
              (seq-some #'fff-dumb-jump--has-upper-p patterns))
         "case-insensitive pattern")
        (t nil)))

(defun fff-dumb-jump--query (id pattern lang)
  "Run PATTERN as fff regex in session ID for LANG. Nil on error."
  (condition-case err
      (fff--grep-raw-ex id pattern "regex"
                        (fff--limit-arg fff-max-grep-results)
                        (fff--limit-arg fff-time-budget-ms)
                        (fff--per-file-cap-arg)
                        (fff--file-size-arg)
                        (fff--flag fff-enforce-time-budget)
                        (fff-dumb-jump--smart-arg lang pattern))
    (error (message "fff-dumb-jump: %s" (error-message-string err))
           nil)))

(defun fff-dumb-jump--format (root raw)
  "Format RAW match as absolute `file:line:col:content'. ROOT is base dir."
  (pcase-let ((`(,rel ,line ,content ,col) (fff--parse-raw-match raw)))
    (concat (expand-file-name rel root) ":" line ":" col ":" content)))

(defun fff-dumb-jump--excluded-p (abs-path exclude-dirs)
  "Non-nil if ABS-PATH is under EXCLUDE-DIRS.
EXCLUDE-DIRS are canonical prefixes, built once per search."
  (seq-some (lambda (dir) (string-prefix-p dir abs-path))
            exclude-dirs))

(defun fff-dumb-jump--wrong-ext-p (rel exts)
  "Non-nil if REL misses the language EXTS.
Nil EXTS keeps all, like an unfiltered searcher."
  (and exts
       (not (seq-some (lambda (e) (string-suffix-p (concat "." e) rel))
                      exts))))

(defun fff-dumb-jump--search (look-for root id patterns lang
                                      exclude-args cur-file line-num parse-fn)
  "Run PATTERNS (rg regexes) in session ID.
ROOT is base dir. Filters by language extension and excluded dirs,
parses with PARSE-FN, then applies `:context' filtering as stock."
  (let* ((joined (mapconcat #'identity patterns "|"))
         (raw (fff-dumb-jump--query id joined lang))
         (exts (and lang (fboundp 'dumb-jump-get-file-exts-by-language)
                    (dumb-jump-get-file-exts-by-language lang)))
         (exclude-dirs (mapcar (lambda (ex)
                                 (file-name-as-directory
                                  (expand-file-name ex root)))
                               exclude-args))
         (lines
          (delq nil
                (mapcar
                 (lambda (m)
                   (let ((rel (car (fff--parse-raw-match m))))
                     (unless (or (fff-dumb-jump--excluded-p
                                  (expand-file-name rel root) exclude-dirs)
                                 (fff-dumb-jump--wrong-ext-p rel exts))
                       (fff-dumb-jump--format root m))))
                 raw)))
         (results (and lines
                       (funcall parse-fn (mapconcat #'identity lines "\n")
                                cur-file line-num)))
         (ignore-case (member lang dumb-jump--case-insensitive-languages)))
    (seq-filter (lambda (it)
                  (dumb-jump--contains-p look-for
                                         (plist-get it :context) ignore-case))
                results)))

(defun fff-dumb-jump--run-command-advice (orig look-for proj regexes lang
                                               exclude-args cur-file
                                               line-num parse-fn generate-fn)
  "Run `dumb-jump-run-command' via fff when possible.
Falls back to ORIG outside the project root, without rules, or for
PCRE2-only patterns. Keeps ORIG fallback search and filtering."
  (if (or (not fff-dumb-jump-enabled)
          (null regexes)
          (not (fboundp 'fff--grep-raw-ex))
          (not (fff-dumb-jump--for-project-p proj)))
      (funcall orig look-for proj regexes lang
               exclude-args cur-file line-num parse-fn generate-fn)
    (let* ((root (expand-file-name proj))
           (id (fff--session root))
           (patterns (dumb-jump-populate-regexes look-for regexes 'rg))
           (reason (fff-dumb-jump--shell-only-p patterns lang)))
      (dumb-jump-debug-message
       (format "fff-dumb-jump: %s in %s" patterns root))
      (cond
       (reason
        (if fff-dumb-jump-fallback-to-shell
            (funcall orig look-for proj regexes lang
                     exclude-args cur-file line-num parse-fn generate-fn)
          (user-error "fff-dumb-jump: %s: %s" reason patterns)))
       (t
        (let ((results (fff-dumb-jump--search
                        look-for root id patterns lang
                        exclude-args cur-file line-num parse-fn)))
          (when (and (null results)
                     dumb-jump-fallback-search
                     (not (fff-dumb-jump--references-p)))
            (setq patterns (dumb-jump-populate-regexes
                            look-for (list dumb-jump-fallback-regex) 'rg))
            (setq reason (fff-dumb-jump--shell-only-p patterns lang))
            (cond
             (reason
              (if fff-dumb-jump-fallback-to-shell
                  (setq results
                        (funcall orig look-for proj regexes lang
                                 exclude-args cur-file line-num
                                 parse-fn generate-fn))
                (user-error "fff-dumb-jump: %s: %s" reason patterns)))
             (t
              (setq results (fff-dumb-jump--search
                             look-for root id patterns lang
                             exclude-args cur-file line-num parse-fn)))))
          results))))))

;;;###autoload
(defun fff-dumb-jump-setup ()
  "Route `dumb-jump-run-command' through fff.
Covers `dumb-jump-go' and xref. Safe to repeat.
Undo with `fff-dumb-jump-teardown'."
  (interactive)
  (advice-add #'dumb-jump-run-command
              :around #'fff-dumb-jump--run-command-advice))

;;;###autoload
(defun fff-dumb-jump-teardown ()
  "Remove `fff-dumb-jump-setup' advice."
  (interactive)
  (advice-remove #'dumb-jump-run-command
                 #'fff-dumb-jump--run-command-advice))

(provide 'fff-dumb-jump)
;;; fff-dumb-jump.el ends here
