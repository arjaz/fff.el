;;; fff.el --- Fast fuzzy files and grep via fff -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Eugene Rossokha
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files, search, matching
;; URL: https://github.com/arjaz/fff.el

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

;; Frontend for fff (https://github.com/dmtrKovalenko/fff).
;;
;; For Consult preview and Embark export see `fff-consult.el'.

;;; Code:

;; TODO: unslopify
(require 'project)
(require 'seq)
(require 'grep)

(defgroup fff nil
  "Fuzzy files and grep via fff."
  :group 'files
  :group 'matching)

(defcustom fff-core-file
  (expand-file-name "fff-core.so"
                    (file-name-directory (or load-file-name
                                             byte-compile-current-file
                                             default-directory)))
  "Path to the fff dynamic module."
  :type 'file)

;; TODO: I don't like the nil case
(defcustom fff-max-file-results 100
  "File candidates per query. Nil means 20000."
  :type '(choice (const :tag "Up to 20000" nil) natnum))

;; TODO: I don't like the nil case
(defcustom fff-max-grep-results 10000
  "Grep matches per one-shot query. Nil means 20000."
  :type '(choice (const :tag "Up to 20000" nil) natnum))

(defcustom fff-grep-page-size nil
  "Grep matches per Consult page. Nil means one screenful."
  :type '(choice (const :tag "One screenful" nil) natnum))

(defcustom fff-min-grep-input 2
  "Shortest input to trigger a query."
  :type 'natnum)

(defcustom fff-grep-mode 'literal
  "How grep reads input: `literal', `regex', or `fuzzy'."
  :type '(choice (const literal) (const regex) (const fuzzy)))

(defcustom fff-grep-smart-case t
  "Non-nil matches lowercase queries case-insensitively."
  :type 'boolean)

(defcustom fff-grep-filename-constraint nil
  "Non-nil treats `main.rs'-like tokens as file filters in grep."
  :type 'boolean)

(defcustom fff-debounce 0.05
  "Delay in seconds before a query runs."
  :type 'number)

(defcustom fff-throttle 0
  "Minimum gap in seconds between queries."
  :type 'number)

(defcustom fff-time-budget-ms 150
  "Time limit in ms per grep query. Nil means no limit."
  :type '(choice (const :tag "No limit" nil) natnum))

(defcustom fff-max-matches-per-file 100
  "Max grep matches per file. 0 or nil means unlimited."
  :type '(choice (const :tag "Unlimited" nil)
                 (const :tag "Unlimited (0)" 0)
                 natnum))

;; TODO: I don't like the nil case
(defcustom fff-max-file-size (* 10 1024 1024)
  "Largest file searched, in bytes. Nil means 10 MB."
  :type '(choice (const :tag "Default 10 MB" nil) natnum))

(defcustom fff-enforce-time-budget nil
  "Non-nil applies the time budget even with zero matches."
  :type 'boolean)

(defcustom fff-scan-timeout-ms 500
  "Wait for the initial scan, in ms."
  :type 'natnum)

(defcustom fff-follow-symlinks nil
  "Non-nil follows symlinks while indexing."
  :type 'boolean)

(defcustom fff-enable-home-dir-scanning t
  "Non-nil allows `$HOME' as a project root."
  :type 'boolean)

(defcustom fff-enable-fs-root-scanning nil
  "Non-nil allows `/' as a project root."
  :type 'boolean)

(defcustom fff-max-threads 4
  "Worker threads for file search. Nil means all CPUs."
  :type '(choice (const :tag "All CPUs" nil) natnum))

(defun fff--cache-dir (name)
  "Cache dir NAME under `user-emacs-directory'."
  (expand-file-name (concat "cache/fff/" name) user-emacs-directory))

(defcustom fff-frecency-db-path (fff--cache-dir "frecency")
  "Directory for the frecency LMDB database.
Nil means keep frecency in memory only."
  :type '(choice (const :tag "In-memory" nil) directory))

(defcustom fff-history-db-path (fff--cache-dir "history")
  "Directory for the query-history LMDB database.
Nil means keep history in memory only."
  :type '(choice (const :tag "In-memory" nil) directory))

(defcustom fff-combo-boost-multiplier 100
  "Score boost for files often opened after the same query."
  :type 'natnum)

(defcustom fff-combo-min-count 3
  "Co-open count that counts as a query/file combo."
  :type 'natnum)

(defvar fff--loaded nil)
(defvar fff--sessions (make-hash-table :test #'equal)
  "Project root to session id.")

;; Module functions, defined in fff-core.so at load.
(declare-function fff--init "fff-core" (base-path frecency-path history-path
                                                   follow-symlinks enable-home enable-fs-root))
(declare-function fff--wait-for-scan "fff-core" (id timeout-ms))
(declare-function fff--search "fff-core" (id query max-results max-threads))
(declare-function fff--search-ex "fff-core" (id query max-results combo-boost min-combo max-threads))
(declare-function fff--search-directories "fff-core" (id query max-results max-threads))
(declare-function fff--track-access "fff-core" (id file))
(declare-function fff--track-query-completion "fff-core" (id query file))
(declare-function fff--refresh-git-status "fff-core" (id))
(declare-function fff--db-status "fff-core" (id))
(declare-function fff--clear-caches "fff-core" (id))
(declare-function fff--grep "fff-core" (id query mode max-matches time-budget-ms filename-constraint smart-case))
(declare-function fff--grep-raw "fff-core" (id query mode max-matches time-budget-ms smart-case))
(declare-function fff--grep-ex "fff-core" (id query mode max-matches time-budget-ms
                                                max-per-file max-file-size enforce-budget
                                                filename-constraint smart-case))
(declare-function fff--grep-raw-ex "fff-core" (id query mode max-matches time-budget-ms
                                                    max-per-file max-file-size enforce-budget smart-case))
(declare-function fff--grep-page "fff-core" (id query mode max-matches time-budget-ms
                                                  max-per-file max-file-size enforce-budget
                                                  filename-constraint file-offset smart-case))
(declare-function fff--rescan "fff-core" (id))
(declare-function fff--base-path "fff-core" (id))
(declare-function fff--scanning-p "fff-core" (id))
(declare-function fff--file-count "fff-core" (id))
(declare-function fff--scan-progress "fff-core" (id))
(declare-function fff--destroy "fff-core" (id))

;;; Sessions

(defun fff--load ()
  "Load the module if not loaded yet."
  (unless fff--loaded
    (unless (file-exists-p fff-core-file)
      (error "fff: module not found at %s; run `make'" fff-core-file))
    (module-load fff-core-file)
    (setq fff--loaded t)))

(defun fff-project-root (&optional maybe-prompt)
  "Root to index for the current buffer.
Like `project-current': default to the project root, ask to
choose a project if not in one when MAYBE-PROMPT is non-nil."
  (or (when-let* ((p (project-current maybe-prompt)))
        (expand-file-name (project-root p)))
      (expand-file-name default-directory)))

(defun fff--warn-db-fallback (id)
  "Warn once per session whose DB fell back to in-memory.
Skips DBs the user left unconfigured (nil path)."
  (let ((status (condition-case nil (fff--db-status id) (error nil))))
    (dolist (line status)
      (when (and (stringp line) (string-match-p "open failed" line))
        (message "fff: %s; using in-memory" line)))))

(defun fff--scan-poll-interval (elapsed-ms)
  "Seconds to wait before the next scan poll, given ELAPSED-MS.
Decays like nvim: 100ms under 1s, 300ms under 3s, 500ms after."
  (cond ((< elapsed-ms 1000) 0.1)
        ((< elapsed-ms 3000) 0.3)
        (t 0.5)))

(defun fff--scan-progress-state (id)
  "(SCANNED IS-SCANNING) for session ID, or nil on error.
Prefers `fff--scan-progress'; older modules fall back to
`fff--scanning-p' and `fff--file-count'."
  (condition-case nil
      (if (fboundp 'fff--scan-progress)
          (fff--scan-progress id)
        (list (or (fff--file-count id) 0)
              (fff--scanning-p id)))
    (error nil)))

(defun fff--wait-for-scan-with-progress (id root timeout-ms)
  "Wait for ID's initial scan up to TIMEOUT-MS, showing progress for ROOT.
Polls `fff--scan-progress-state' on a 100/300/500ms decay like
nvim, messaging `fff: scanning <root> (N files)…' while it runs.
Returns t when the scan finished, nil on timeout. Total wait
stays within TIMEOUT-MS; batch-safe (`sleep-for', no hang). Fast
scans stay silent; slow ones end with a ready message."
  (let* ((timeout (max 0 (or timeout-ms fff-scan-timeout-ms 500)))
         (start (float-time))
         (deadline (+ start (/ timeout 1000.0)))
         (shown nil)
         (done nil)
         (timed-out nil))
    (while (not (or done timed-out))
      (let ((st (fff--scan-progress-state id)))
        (cond
         ((null st)
          (let ((left-ms (max 0 (round (* 1000 (- deadline (float-time)))))))
            (setq done (condition-case nil
                           (fff--wait-for-scan id left-ms)
                         (error nil))))
          (setq timed-out (not done)))
         ((not (nth 1 st))
          (setq done t))
         ((>= (float-time) deadline)
          (setq timed-out t))
         (t
          (setq shown t)
          (message "fff: scanning %s (%s files)…"
                   (abbreviate-file-name root) (or (nth 0 st) 0))
          (let* ((elapsed-ms (* 1000 (- (float-time) start)))
                 (interval (fff--scan-poll-interval elapsed-ms)))
            (sleep-for (min interval (max 0 (- deadline (float-time))))))))))
    (when (and done shown)
      (let ((n (condition-case nil (fff--file-count id) (error nil))))
        (message "fff: scan of %s ready (%s files)"
                 (abbreviate-file-name root) (or n "?"))))
    done))

(defun fff--session (root)
  "Session id for ROOT, starting the index if needed."
  (fff--load)
  (let ((root (expand-file-name root)))
    (or (gethash root fff--sessions)
        (let ((id (fff--init root fff-frecency-db-path fff-history-db-path
                                 (fff--flag fff-follow-symlinks)
                                 (fff--flag fff-enable-home-dir-scanning)
                                 (fff--flag fff-enable-fs-root-scanning))))
          (puthash root id fff--sessions)
          (add-hook 'find-file-hook #'fff--maybe-track-access)
          (fff--warn-db-fallback id)
          (unless (fff--wait-for-scan-with-progress id root fff-scan-timeout-ms)
            (message "fff: initial scan of %s timed out; results may be partial" root))
          id))))

(defun fff--session-for-file (file)
  "Id of the live session whose root contains FILE, or nil.
Never starts a session."
  (let ((file (expand-file-name file))
        (best nil)
        (bestlen -1))
    (maphash (lambda (root id)
               (let ((dir (file-name-as-directory root)))
                 (when (and (string-prefix-p dir file)
                            (> (length dir) bestlen))
                   (setq best id bestlen (length dir)))))
             fff--sessions)
    best))

(defun fff--maybe-track-access ()
  "Record the visited file for frecency, if it is indexed.
Runs on `find-file-hook'; never errors or prompts."
  (when buffer-file-name
    (condition-case nil
        (let ((id (fff--session-for-file buffer-file-name)))
          (when id
            (fff--track-access id (expand-file-name buffer-file-name))))
      (error nil))))

(defun fff--track-completion (id query file)
  "Record that QUERY picked FILE in session ID.
Silent; tracking must never break a selection."
  (when (and (stringp query) (> (length query) 0)
             (stringp file) (integerp id))
    (condition-case nil
        (fff--track-query-completion id query (expand-file-name file))
      (error nil))))

(defun fff-rescan ()
  "Rescan the current project in the background.
Defaults to the project root, asks to choose a project if not in one."
  (interactive)
  (fff--load)
  (let ((id (fff--session (fff-project-root t))))
    (fff--rescan id)
    (message "fff: rescanning %s" (fff--base-path id))))

(defun fff-refresh-git-status ()
  "Refresh git statuses without a full rescan.
Defaults to the project root, asks to choose a project if not in one."
  (interactive)
  (fff--load)
  (let* ((root (fff-project-root t))
         (id (fff--session root))
         (n (fff--guarded (lambda () (fff--refresh-git-status id)))))
    (when n
      (message "fff: git status refreshed for %s (%s files)"
               (abbreviate-file-name root) n))))

(defun fff-clear-caches ()
  "Drop all indexes and on-disk frecency/history caches.
Sessions re-init on next use."
  (interactive)
  (fff--load)
  (let ((ids nil))
    (maphash (lambda (_root id) (push id ids)) fff--sessions)
    (unless ids
      (message "fff: no sessions to clear"))
    (let ((deleted nil))
      (dolist (id ids)
        (let ((paths (fff--guarded (lambda () (fff--clear-caches id)))))
          (if paths
              (setq deleted (append paths deleted))
            ;; Module drop failed; at least release the picker.
            (ignore-errors (fff--destroy id)))))
      (clrhash fff--sessions)
      (when ids
        (message "fff: cleared %s session(s)%s" (length ids)
                 (if deleted
                     (concat "; removed " (string-join deleted ", "))
                   "; no DB dirs removed"))))))

(defun fff-health ()
  "Show index status for the current project.
Covers module load, file count, scan state, git, picker, and DBs.
Defaults to the project root, asks to choose a project if not in one."
  (interactive)
  (fff--load)
  (let* ((root (fff-project-root t))
         (id (fff--session root))
         (n (fff--guarded (lambda () (fff--file-count id))))
         (scanning (and n (fff--guarded (lambda () (fff--scanning-p id)))))
         (ready (and (natnump (or n -1)) (>= n 0)))
         (git-bin (executable-find "git"))
         (git-root (locate-dominating-file root ".git"))
         (dbs (fff--guarded (lambda () (fff--db-status id)))))
    (message "fff: %s\n  module: loaded (%s)\n  index: %s files%s%s\n  git: %s%s\n  %s"
             (abbreviate-file-name root)
             (abbreviate-file-name fff-core-file)
             (or n "?")
             (if scanning " (scan in progress)" "")
             (if ready "" " (not ready)")
             (or git-bin "not found")
             (if git-root
                 (format " (repo at %s)" (abbreviate-file-name git-root))
               " (no repo)")
             (if dbs (string-join dbs "\n  ") "dbs: unknown"))))

;;; Fetch and format

(defconst fff--sep "\x1f"
  "Field separator used by the module.")

(defun fff--limit-arg (limit)
  "Convert LIMIT for the module: nil becomes 0."
  (or limit 0))

(defconst fff--default-max-file-size (* 10 1024 1024)
  "Bytes searched per file when `fff-max-file-size' is nil or 0.")

(defun fff--per-file-cap-arg (&optional unlimited)
  "Per-file cap: 0 means unlimited."
  (if (or unlimited
          (not (and (natnump fff-max-matches-per-file)
                    (> fff-max-matches-per-file 0))))
      0
    fff-max-matches-per-file))

(defun fff--file-size-arg ()
  "File size cap in bytes."
  (if (and (natnump fff-max-file-size) (> fff-max-file-size 0))
      fff-max-file-size
    fff--default-max-file-size))

(defun fff--flag (val)
  "1 when VAL is non-nil, else 0."
  (if val 1 0))

(defun fff--smart-case-arg (&optional smart)
  "1 for smart-case, 0 for case-sensitive.
Nil follows `fff-grep-smart-case'; explicit 0 stays off."
  (cond ((null smart)
         (if fff-grep-smart-case 1 0))
        ((and (numberp smart) (zerop smart)) 0)
        (t 1)))

(defun fff--max-threads-arg ()
  "Thread count: 0 means all CPUs."
  (if (and (natnump fff-max-threads) (> fff-max-threads 0))
      fff-max-threads
    0))

(defun fff--grep-page-size ()
  "Page size for paged grep."
  (if (and (natnump fff-grep-page-size) (> fff-grep-page-size 0))
      fff-grep-page-size
    (max 10 (window-body-height (or (get-largest-window)
                                    (selected-window))))))

(defun fff--guarded (thunk)
  "Run THUNK, return nil and message on error."
  (condition-case err (funcall thunk)
    (error (message "fff: %s" (error-message-string err)) nil)))

(defun fff--file-fetch (id input)
  "File candidates for INPUT in session ID."
  (fff--guarded (lambda () (fff--search-ex id input (fff--limit-arg fff-max-file-results)
                                           fff-combo-boost-multiplier
                                           fff-combo-min-count
                                           (fff--max-threads-arg)))))

(defun fff--dir-fetch (id input)
  "Directory candidates for INPUT in session ID.
Repo-relative paths with trailing slashes, like the index."
  (fff--guarded (lambda () (fff--search-directories id input
                                                   (fff--limit-arg fff-max-file-results)
                                                   (fff--max-threads-arg)))))

(defun fff--grep-fetch (id input mode &optional unlimited-per-file smart-case)
  "Raw grep matches for INPUT in session ID with MODE."
  (if (< (length input) fff-min-grep-input)
      nil
    (fff--guarded (lambda () (fff--grep-ex id input (symbol-name (fff--normalize-grep-mode mode))
                                           (fff--limit-arg fff-max-grep-results)
                                           (fff--limit-arg fff-time-budget-ms)
                                           (fff--per-file-cap-arg unlimited-per-file)
                                           (fff--file-size-arg)
                                           (fff--flag fff-enforce-time-budget)
                                           (fff--flag fff-grep-filename-constraint)
                                           (fff--smart-case-arg smart-case))))))

(defun fff--grep-fetch-page (id input mode page-size file-offset &optional unlimited-per-file smart-case)
  "One grep page for INPUT. Returns (NEXT-OFFSET . MATCHES)."
  (if (< (length input) fff-min-grep-input)
      (cons 0 nil)
    (let ((res (fff--guarded (lambda () (fff--grep-page id input (symbol-name (fff--normalize-grep-mode mode))
                                                       page-size
                                                       (fff--limit-arg fff-time-budget-ms)
                                                       (fff--per-file-cap-arg unlimited-per-file)
                                                       (fff--file-size-arg)
                                                       (fff--flag fff-enforce-time-budget)
                                                       (fff--flag fff-grep-filename-constraint)
                                                        file-offset
                                                        (fff--smart-case-arg smart-case))))))
      (when (and (consp res) (integerp (car res)))
        (cons (car res) (cdr res))))))

(defun fff--filename-constraint-token-p (tok)
  "Non-nil if TOK looks like a filename filter (`main.rs')."
  (and (> (length tok) 0)
       (not (and (string-suffix-p "/" tok)
                 (not (string-prefix-p "." tok))))
        (not (string-match-p "[*?\\[{]" tok))
        ;; Rust takes the part after the last `/` verbatim (`rsplit`),
        ;; so a trailing slash yields an empty base and fails.
        (let ((base (if (string-suffix-p "/" tok)
                        ""
                      (car (last (split-string tok "/" t))))))
         (when (and base (string-match "\\.\\([[:alnum:]]+\\)\\'" base))
           (let ((ext (match-string 1 base)))
             (and (> (length ext) 0) (<= (length ext) 10)))))))

(defun fff--literal-terms (input)
  "Words in INPUT used for highlighting."
  (let* ((toks (split-string input "[ \t\n]+" t))
         (drop-name (and fff-grep-filename-constraint (> (length toks) 1))))
    (seq-filter (lambda (tok)
                  (and (> (length tok) 0)
                       (not (string-prefix-p "!" tok))
                       (not (string-prefix-p "git:" tok))
                       (not (string-match-p "[*?\\[{/]" tok))
                       (not (and drop-name (fff--filename-constraint-token-p tok)))))
                toks)))

(defun fff--highlight-literal-with-face (term str offset face &optional smart)
  "Highlight TERM in STR from OFFSET with FACE.
SMART nil (omitted) or non-zero means smart-case like the index:
case-insensitive when TERM is all lowercase. SMART 0 always
matches case-sensitively, like the index with smart-case off.
The lowercase test binds `case-fold-search' nil: with folding on,
`[[:upper:]]' would match lowercase too."
  (when (> (length term) 0)
    (let ((case-fold-search (and (not (eq smart 0))
                                 (let ((case-fold-search nil))
                                   (not (string-match-p "[[:upper:]]" term)))))
          (pos offset)
          (re (regexp-quote term)))
      (while (string-match re str pos)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                face nil str)
        (setq pos (match-end 0))))))

(defun fff--parse-match-ranges (str)
  "Parse STR \"S-E,S-E\" to ((S . E) ...), else nil."
  (when (and (stringp str)
             (string-match-p "\\`[0-9]+-[0-9]+\\(,[0-9]+-[0-9]+\\)*\\'" str))
    (mapcar (lambda (span)
              (let ((dash (string-match "-" span)))
                (cons (string-to-number (substring span 0 dash))
                      (string-to-number (substring span (1+ dash))))))
            (split-string str "," t))))

(defun fff--byte-to-char-index (str byte)
  "Char index in STR for BYTE offset."
  (if (= (string-bytes str) (length str))
      (min byte (length str))
    (with-temp-buffer
      (insert str)
      (1- (byte-to-position (min (1+ byte) (1+ (string-bytes str))))))))

(defun fff--highlight-byte-ranges-with-face (ranges content offset str face)
  "Highlight RANGES of CONTENT at OFFSET in STR with FACE."
  (when (and ranges (> (length content) 0))
    (let ((bytes (string-bytes content)))
      (dolist (r ranges)
        (let* ((sb (max 0 (min (car r) bytes)))
               (eb (max 0 (min (cdr r) bytes))))
          (when (< sb eb)
            (let ((s (+ offset (fff--byte-to-char-index content sb)))
                  (e (+ offset (fff--byte-to-char-index content eb))))
              (when (< s e)
                (add-face-text-property s (min e (length str)) face nil str)))))))))

(defun fff--parse-raw-match (raw)
  "Split RAW to (FILE LINE CONTENT COL RANGES)."
  (let* ((parts (split-string raw fff--sep))
         (file (or (nth 0 parts) raw))
         (line (or (nth 1 parts) "1"))
         (wire-col (or (nth 2 parts) "0"))
         (col (number-to-string (max 1 (1+ (string-to-number wire-col)))))
         (ranged (> (length parts) 4))
         (content (mapconcat #'identity
                             (if ranged (butlast (nthcdr 3 parts)) (nthcdr 3 parts))
                             fff--sep))
         (ranges (and ranged (fff--parse-match-ranges (car (last parts))))))
    (list file line content col ranges)))

(defun fff--highlight-match (terms ranges content prefix-len str face smart mode)
  "Highlight STR: exact RANGES for regex/fuzzy, TERMS otherwise."
  (if (and ranges (memq mode '(regex fuzzy)))
      (fff--highlight-byte-ranges-with-face ranges content prefix-len str face)
    (dolist (term terms)
      (fff--highlight-literal-with-face term str prefix-len face smart))))

(defun fff--plain-format-match (raw terms &optional mode smart)
  "Format RAW as \"file:line:col:content\"."
  (pcase-let ((`(,file ,line ,content ,col ,ranges) (fff--parse-raw-match raw)))
    (let* ((str (concat file ":" line ":" col ":" content))
            (prefix-len (+ 3 (length file) (length line) (length col))))
      (add-text-properties 0 (length file)
                           `(face grep-match-file-name fff--file ,file fff--line ,line) str)
      (fff--highlight-match terms ranges content prefix-len str 'match smart mode)
      str)))

(defconst fff--grep-error-regexp
  '("^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
  "Match \"file:line:col:\".")

(defvar-local fff--grep-query nil
  "Grep query that produced this buffer, for completion tracking.")
(defvar-local fff--grep-id nil
  "Session id that produced this buffer, for completion tracking.")

(defun fff--grep-track-jump (orig marker &rest args)
  "Track completion on grep jumps. Silent otherwise."
  (let ((target (apply orig marker args)))
    (condition-case nil
        (let* ((src (and (markerp marker) (marker-buffer marker)))
               (query (and src (buffer-live-p src)
                           (buffer-local-value 'fff--grep-query src)))
               (gid (and src (buffer-live-p src)
                         (buffer-local-value 'fff--grep-id src)))
               (file (and target (buffer-live-p target)
                          (buffer-file-name target))))
          (when (and (stringp query) (integerp gid) (stringp file))
            (fff--track-query-completion gid query file)))
      (error nil))
    target))

(defun fff--grep-buffer-setup ()
  "Tune grep buffer for `file:line:col:' and jump tracking."
  (unless (memq fff--grep-error-regexp compilation-error-regexp-alist)
    (setq-local compilation-error-regexp-alist
                 (cons fff--grep-error-regexp compilation-error-regexp-alist)))
  (setq-local compilation-first-column 1)
  (unless (advice-member-p #'fff--grep-track-jump 'compilation-find-file)
    (advice-add 'compilation-find-file :around #'fff--grep-track-jump)))

;;; Commands

(defconst fff--grep-modes
  '((literal . "literal text")
    (regex . "regexp")
    (fuzzy . "typo-tolerant"))
  "Grep modes for `fff-set-grep-mode'.")

(defun fff--normalize-grep-mode (mode)
  "Return MODE if valid, else error."
  (or (and (memq mode '(literal regex fuzzy)) mode)
      (user-error "fff: unknown grep mode %S (literal, regex, fuzzy)" mode)))

(defun fff--next-grep-mode (mode)
  "Next mode after MODE."
  (pcase (fff--normalize-grep-mode mode)
    ('literal 'regex)
    ('regex 'fuzzy)
    ('fuzzy 'literal)))

;;;###autoload
(defun fff-set-grep-mode (mode)
  "Set `fff-grep-mode' to MODE."
  (interactive
   (let* ((names (mapcar #'symbol-name (mapcar #'car fff--grep-modes)))
          (completion-extra-properties
           (list :annotation-function
                 (lambda (name) (concat " - " (cdr (assq (intern name) fff--grep-modes))))))
          (choice (completing-read "grep mode: " names nil t nil nil (symbol-name fff-grep-mode))))
     (list (intern choice))))
  (setq fff-grep-mode (fff--normalize-grep-mode mode))
  (message "fff grep mode: %s" fff-grep-mode))

;;;###autoload
(defun fff-toggle-grep-mode ()
  "Cycle `fff-grep-mode' literal -> regex -> fuzzy."
  (interactive)
  (setq fff-grep-mode (fff--next-grep-mode fff-grep-mode))
  (message "fff grep mode: %s" fff-grep-mode))

;;;###autoload
(defun fff-set-grep-smart-case (enabled)
  "Set `fff-grep-smart-case' to ENABLED."
  (interactive
   (let ((choice (completing-read "smart-case: " '("on" "off") nil t nil nil
                                  (if fff-grep-smart-case "on" "off"))))
     (list (equal choice "on"))))
  (setq fff-grep-smart-case (if enabled t nil))
  (message "fff grep smart-case: %s" (if fff-grep-smart-case "on" "off")))

;;;###autoload
(defun fff-toggle-grep-smart-case ()
  "Flip `fff-grep-smart-case'."
  (interactive)
  (setq fff-grep-smart-case (if fff-grep-smart-case nil t))
  (message "fff grep smart-case: %s" (if fff-grep-smart-case "on" "off")))

(defun fff--single-root (prompt paths edir)
  "Index root for a search.
PROMPT names the command in errors. PATHS is the directory
selection, EDIR its base. Errors on multiple paths or TRAMP."
  (when (file-remote-p edir)
    (user-error "fff %s cannot index remote directories: %s" prompt edir))
  (when (> (length paths) 1)
    (user-error "fff %s covers one directory; pick a single directory (got %s)"
                prompt (string-join paths ", ")))
  (expand-file-name edir))

(defun fff--vanilla-directory-prompt (prompt dir)
  "Build a (PROMPT PATHS EDIR) triple without Consult.
DIR: nil means project root, asking to choose a project if not
in one; string means that directory, prefix means ask for a
directory, double prefix means ask for a project."
  (let ((edir (cond
               ((stringp dir) (expand-file-name dir))
               ((and (listp dir) (> (prefix-numeric-value dir) 4))
                (expand-file-name (project-prompt-project-dir)))
               ((and dir (listp dir))
                (expand-file-name
                 (read-directory-name (format "%s directory: " prompt)
                                      nil nil t)))
               (t (fff-project-root t)))))
    (list (format "%s (%s): " prompt (abbreviate-file-name edir))
          (list edir) edir)))

(defun fff--table-id (string table)
  "Session id in TABLE metadata."
  (cdr (assq 'fff-id (completion-metadata string table nil))))

(defun fff--table-kind (string table)
  "Kind in TABLE metadata: `dir' or `file'."
  (cdr (assq 'fff-kind (completion-metadata string table nil))))

(defun fff--server-fetch (id input kind)
  "Candidates for INPUT. KIND `dir' for dirs, else files."
  (if (eq kind 'dir) (fff--dir-fetch id input) (fff--file-fetch id input)))

(defun fff--server-try (string table pred _point)
  "Exact match passes, rest goes to completions."
  (let* ((id (fff--table-id string table))
         (cands (and id (fff--server-fetch id string (fff--table-kind string table)))))
    (when pred (setq cands (seq-filter pred cands)))
    (and (member string cands) t)))

(defun fff--server-all (string table pred _point)
  "Server candidates, unfiltered to keep typo hits."
  (let* ((id (fff--table-id string table))
         (cands (and id (fff--server-fetch id string (fff--table-kind string table)))))
    (when pred (setq cands (seq-filter pred cands)))
    cands))

(defconst fff--file-completion-style
  '(fff-files fff--server-try fff--server-all "Server results.")
  "Passthrough style for file candidates.")

(defconst fff--dir-completion-style
  '(fff-dirs fff--server-try fff--server-all "Server results.")
  "Passthrough style for directory candidates.")

(defun fff--styles-first (overrides style)
  "OVERRIDES with STYLE first for `file'."
  (let* ((tags (cdr (assq 'file overrides)))
         (rest (assq-delete-all 'file (copy-sequence overrides)))
         (others (assq-delete-all 'styles (copy-sequence tags)))
         (theirs (remq style (cdr (assq 'styles tags)))))
    (cons `(file (styles ,style ,@theirs) ,@others) rest)))

(defun fff--file-styles-first (overrides)
  "OVERRIDES with `fff-files' first."
  (fff--styles-first overrides 'fff-files))

(defun fff--dir-styles-first (overrides)
  "OVERRIDES with `fff-dirs' first."
  (fff--styles-first overrides 'fff-dirs))

(defun fff--completion-table (id kind)
  "Completion table for ID. KIND `dir' for dirs, else files."
  (lambda (string pred action)
    (let ((fetch (if (eq kind 'dir) #'fff--dir-fetch #'fff--file-fetch)))
      (cond
       ((eq action 'metadata)
        `(metadata (category . file)
                   (display-sort-function . identity)
                   (fff-id . ,id) (fff-kind . ,kind)))
       ((eq action 'lambda)
        (let ((cands (and id (funcall fetch id string))))
          (when pred (setq cands (seq-filter pred cands)))
          (member string cands)))
       ((null action)
        (let ((cands (and id (funcall fetch id string))))
          (when pred (setq cands (seq-filter pred cands)))
          (try-completion string cands pred)))
       (t
        (let ((cands (and id (funcall fetch id string))))
          (when pred (setq cands (seq-filter pred cands)))
          cands))))))

(defun fff--file-completion-table (id)
  "Completion table for ID (files)."
  (fff--completion-table id 'file))

(defun fff--dir-completion-table (id)
  "Completion table for ID (dirs)."
  (fff--completion-table id 'dir))

(defun fff--selectable (sel root dir-p)
  "SEL as abs path under ROOT if it exists. DIR-P checks for dirs."
  (when (stringp sel)
    (let ((abs (expand-file-name sel root)))
      (when (and (file-exists-p abs)
                 (if dir-p (file-directory-p abs) (not (file-directory-p abs))))
        abs))))

(defun fff--selectable-file (sel root)
  "SEL as abs file under ROOT, else nil."
  (fff--selectable sel root nil))

(defun fff--selectable-dir (sel root)
  "SEL as abs dir under ROOT, else nil."
  (fff--selectable sel root t))

(defun fff--vanilla-pick (prompt dir kind initial)
  "Read one pick. KIND `dir' for dirs, else files. Returns (ID ROOT SEL QUERY)."
  (pcase-let ((`(,p ,paths ,edir) (fff--vanilla-directory-prompt prompt dir)))
    (let* ((root (fff--single-root prompt paths edir))
           (id (fff--session root))
           (default-directory root)
           (style (if (eq kind 'dir) fff--dir-completion-style fff--file-completion-style))
           (sname (if (eq kind 'dir) 'fff-dirs 'fff-files))
           (completion-styles-alist (cons style completion-styles-alist))
           (completion-styles (list sname))
           (completion-category-overrides
            (fff--styles-first completion-category-overrides sname))
           (table (fff--completion-table id kind))
           (query nil)
           (sel (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'minibuffer-exit-hook
                                (lambda () (setq query (minibuffer-contents-no-properties)))
                                nil t))
                  (completing-read p table nil t initial
                                   'file-name-history (thing-at-point 'filename)))))
      (list id root sel query))))

;;;###autoload
(defun fff-find-file (&optional dir initial)
  "Find a file in the index."
  (interactive "P")
  (fff--load)
  (pcase-let ((`(,id ,root ,sel ,query) (fff--vanilla-pick "Find" dir 'file initial)))
    (let ((abs (or (fff--selectable-file sel root)
                   (user-error "fff: no such file: %s" sel))))
      (fff--track-completion id query abs)
      (find-file abs))))

;;;###autoload
(defun fff-find-dir (&optional dir initial)
  "Find a directory in the index, open it in dired."
  (interactive "P")
  (fff--load)
  (pcase-let ((`(_id ,root ,sel ,_q) (fff--vanilla-pick "Find dir" dir 'dir initial)))
    (let ((abs (or (fff--selectable-dir sel root)
                   (user-error "fff: no such directory: %s" sel))))
      (dired abs))))

;;;###autoload
(defun fff-grep (&optional dir initial)
  "Grep the index, show matches in *fff-grep*.
No subprocess. Input follows `fff-grep-mode': `literal',
`regex', or `fuzzy'. Set it with `fff-set-grep-mode' or
`fff-toggle-grep-mode'; matching follows `fff-grep-smart-case'
(`fff-set-grep-smart-case' / `fff-toggle-grep-smart-case').
The buffer holds exhaustive per-file matches (no per-file cap),
like nvim quickfix. DIR is as in `fff-find-file'. Region becomes
initial input. See `fff-consult-grep' for preview and Embark export."
  (interactive "P")
  (fff--load)
  (pcase-let ((`(,_prompt ,paths ,edir) (fff--vanilla-directory-prompt "fff grep" dir)))
    (let* ((root (fff--single-root "grep" paths edir))
           (id (fff--session root))
           (mode (fff--normalize-grep-mode fff-grep-mode))
           (input (read-string
                   (format "fff grep (%s, %s): "
                           (abbreviate-file-name root) mode)
                   (or initial
                       (and (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))))
                   'grep-history))
           (raw (fff--grep-fetch id input mode t))
           (terms (fff--literal-terms input))
           (smart (fff--smart-case-arg))
           (buf (get-buffer-create "*fff-grep*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (m raw)
            (insert (fff--plain-format-match m terms mode smart) "\n")))
        (setq-local default-directory root)
        (grep-mode)
        (setq-local fff--grep-query input)
        (setq-local fff--grep-id id)
        (fff--grep-buffer-setup))
      (pop-to-buffer buf))))

(provide 'fff)
;;; fff.el ends here
