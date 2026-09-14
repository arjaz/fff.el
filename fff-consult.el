;;; fff-consult.el --- Consult UI for fff -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Eugene Rossokha
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (consult "2.0") (fff "0.1"))
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

;; Consult integration for fff.el

;;; Code:

(require 'fff)
(require 'consult)

(defvar embark-exporters-alist)
(declare-function embark-consult-export-grep "embark-consult" (lines))

(defun fff-consult--highlight-literal (term str offset &optional smart)
  "Highlight TERM in STR."
  (fff--highlight-literal-with-face term str offset 'consult-highlight-match smart))

(defun fff-consult--format-match (raw terms &optional mode smart)
  "Format RAW as a `consult-grep' candidate."
  (pcase-let ((`(,file ,line ,content ,col ,ranges) (fff--parse-raw-match raw)))
    (let* ((str (concat file ":" line ":" col ":" content))
           (file-len (length file))
           (prefix-len (+ 3 file-len (length line) (length col))))
      (add-text-properties 0 file-len
                           `(face consult-file consult--prefix-group ,file) str)
      (put-text-property (1+ file-len) (+ 2 file-len (length line) (length col))
                         'face 'consult-line-number str)
      (fff--highlight-match terms ranges content prefix-len str 'consult-highlight-match smart mode)
      str)))

(defun fff-consult--grep-format (input &optional mode smart)
  "Formatter for INPUT for `consult--async-transform-by-input'."
  (let ((terms (fff--literal-terms input)))
    (lambda (cands)
      (mapcar (lambda (raw) (fff-consult--format-match raw terms mode smart)) cands))))

;;; Tags force a refetch: appended to push input, stripped before query.

(defconst fff-consult--mode-tag-sep "\x1f"
  "Separates query text from a refetch tag.")

(defun fff-consult--tag-input (input mode)
  "INPUT plus MODE tag."
  (concat input fff-consult--mode-tag-sep (symbol-name mode)))

(defun fff-consult--strip-refresh-tag (input)
  "INPUT without a trailing mode/case tag."
  (if (string-match "\x1f\\(literal\\|regex\\|fuzzy\\|smart\\|case\\)\\'" input)
      (substring input 0 (match-beginning 0))
    input))

(defconst fff-consult--more-tag-sep "\x1fmore"
  "Separates query text from a paging tag.")

(defun fff-consult--more-tag-input (input n)
  "INPUT plus paging counter N."
  (concat input fff-consult--more-tag-sep (number-to-string n)))

(defun fff-consult--more-tag-n (input)
  "Paging counter in INPUT, else nil."
  (when (string-match "\x1fmore\\([0-9]+\\)\\'" input)
    (string-to-number (match-string 1 input))))

(defun fff-consult--smart-tag-input (input smart)
  "INPUT plus smart/case tag for SMART."
  (concat input fff-consult--mode-tag-sep (if smart "smart" "case")))

(defun fff-consult--strip-query-tags (input)
  "INPUT without trailing tags."
  (let ((prev nil)
        (cur input))
    (while (not (equal prev cur))
      (setq prev cur)
      (setq cur (fff-consult--strip-refresh-tag
                 (if (string-match "\x1fmore[0-9]+\\'" cur)
                     (substring cur 0 (match-beginning 0))
                   cur))))
    cur))

(defun fff-consult--async-input (str)
  "Query part of STR."
  (let* ((style (or (alist-get consult-async-split-style
                              consult-async-split-styles-alist)
                    (user-error "Splitting style `%s' not found"
                                consult-async-split-style)))
         (fun (plist-get style :function)))
    (car (funcall fun str style))))

(defun fff-consult--set-cell-mode (cell mode)
  "Set CELL to MODE. t if changed."
  (fff--normalize-grep-mode mode)
  (unless (eq (car cell) mode)
    (setcar cell mode)
    t))

(defun fff-consult--set-cell-smart (cell smart)
  "Set CELL to SMART. t if changed."
  (let ((want (if smart t nil)))
    (unless (eq (car cell) want)
      (setcar cell want)
      t)))

(defun fff-consult--cell-smart-arg (cell)
  "1/0 for CELL, or global when nil."
  (if (null cell)
      (fff--smart-case-arg)
    (if (car cell) 1 0)))

(defconst fff-consult--st-query 0)
(defconst fff-consult--st-cands 1)
(defconst fff-consult--st-offset 2)
(defconst fff-consult--st-done 3)
(defconst fff-consult--st-loading 4)
(defconst fff-consult--st-more 5)

(defun fff-consult--fetch (id input cell state &optional smart-cell)
  "Paged grep fetch for INPUT. Returns accumulated matches."
  (let* ((n (fff-consult--more-tag-n input))
         (query (fff-consult--strip-query-tags input))
         (mode (car cell))
         (smart (fff-consult--cell-smart-arg smart-cell)))
    (aset state fff-consult--st-loading nil)
    (if (and n (> (length query) 0) (equal query (aref state fff-consult--st-query)))
        (if (aref state fff-consult--st-done)
            (aref state fff-consult--st-cands)
          (let ((page (fff--grep-fetch-page id query mode
                                            (fff--grep-page-size)
                                            (aref state fff-consult--st-offset)
                                            nil smart)))
            (when page
              (aset state fff-consult--st-cands (append (aref state fff-consult--st-cands) (cdr page)))
              (aset state fff-consult--st-offset (car page))
              (when (= (car page) 0)
                (aset state fff-consult--st-done t)))
            (aref state fff-consult--st-cands)))
      (aset state fff-consult--st-query query)
      (aset state fff-consult--st-more 0)
      (let ((page (fff--grep-fetch-page id query mode
                                        (fff--grep-page-size) 0
                                        nil smart)))
        (if (null page)
            (progn
              (aset state fff-consult--st-cands nil)
              (aset state fff-consult--st-offset 0)
              (aset state fff-consult--st-done nil)
              nil)
          (aset state fff-consult--st-cands (cdr page))
          (aset state fff-consult--st-offset (car page))
          (aset state fff-consult--st-done (= (car page) 0))
          (aref state fff-consult--st-cands))))))

(defun fff-consult--make-paging-state ()
  "Empty paging state: [query cands offset done loading more]."
  (vector nil nil 0 nil nil 0))

(defvar-local fff-consult--mode-cell nil)
(defvar-local fff-consult--smart-cell nil)
(defvar-local fff-consult--paging-state nil)
(defvar-local fff-consult--session-id nil)
(defvar-local fff-consult--push nil)
(defvar-local fff-consult--mode-overlay nil)

(defun fff-consult--show-mode (mode smart)
  "Badge MODE and SMART in the prompt."
  (when (overlayp fff-consult--mode-overlay)
    (delete-overlay fff-consult--mode-overlay))
  (setq fff-consult--mode-overlay
        (consult--make-overlay
         (1- (minibuffer-prompt-end)) (minibuffer-prompt-end)
         'before-string
         (format #(" (%s, %s)" 0 5 (face consult-narrow-indicator))
                 mode (if smart "smart" "case")))))

(defun fff-consult--session-smart ()
  "Session smart-case, or global."
  (if (and (boundp 'fff-consult--smart-cell) fff-consult--smart-cell)
      (car fff-consult--smart-cell)
    fff-grep-smart-case))

(defun fff-consult--switch-mode (mode)
  "Switch session to MODE."
  (unless (minibufferp)
    (user-error "fff: mode keys work only during a Consult grep session"))
  (unless (and fff-consult--mode-cell fff-consult--push)
    (user-error "fff: no Consult grep session"))
  (when (fff-consult--set-cell-mode fff-consult--mode-cell mode)
    (fff-consult--show-mode mode (fff-consult--session-smart))
    (funcall fff-consult--push
             (fff-consult--tag-input
              (fff-consult--async-input (minibuffer-contents-no-properties))
              mode))
    (message "fff grep mode: %s (session only)" mode)))

(defun fff-consult--switch-smart-case ()
  "Flip session smart-case."
  (unless (minibufferp)
    (user-error "fff: case key works only during a Consult grep session"))
  (unless (and fff-consult--smart-cell fff-consult--push
               fff-consult--mode-cell)
    (user-error "fff: no Consult grep session"))
  (let ((smart (not (car fff-consult--smart-cell))))
    (setcar fff-consult--smart-cell smart)
    (fff-consult--show-mode (car fff-consult--mode-cell) smart)
    (funcall fff-consult--push
             (fff-consult--smart-tag-input
              (fff-consult--async-input (minibuffer-contents-no-properties))
              smart))
    (message "fff grep smart-case: %s (session only)"
             (if smart "on" "off"))))

(defun fff-consult-grep-cycle-mode ()
  "Cycle session grep mode."
  (declare (completion ignore))
  (interactive)
  (unless (minibufferp)
    (user-error "fff: mode keys work only during a Consult grep session"))
  (unless (and fff-consult--mode-cell fff-consult--push)
    (user-error "fff: no Consult grep session"))
  (fff-consult--switch-mode
   (fff--next-grep-mode (car fff-consult--mode-cell))))

(defun fff-consult-grep-smart-case ()
  "Flip session smart-case."
  (declare (completion ignore))
  (interactive)
  (fff-consult--switch-smart-case))

(defun fff-consult--load-more ()
  "Fetch next page. t if started."
  (let ((st fff-consult--paging-state))
    (when (and st fff-consult--push
               (aref st fff-consult--st-query)
               (not (aref st fff-consult--st-done))
               (not (aref st fff-consult--st-loading)))
      (let ((n (1+ (aref st fff-consult--st-more))))
        (aset st fff-consult--st-more n)
        (aset st fff-consult--st-loading t)
        (funcall fff-consult--push
                 (fff-consult--more-tag-input (aref st fff-consult--st-query) n))
        t))))

(defun fff-consult-grep-more ()
  "Fetch next page now."
  (declare (completion ignore))
  (interactive)
  (unless (minibufferp)
    (user-error "fff: more key works only during a Consult grep session"))
  (unless (and fff-consult--push fff-consult--paging-state)
    (user-error "fff: no Consult grep session"))
  (or (fff-consult--load-more)
      (message "fff: no more matches")))

(defun fff-consult--load-more-idle (buf query)
  "Fetch next page for BUF if QUERY is current."
  (when (and (buffer-live-p buf)
             (eq buf (window-buffer (active-minibuffer-window))))
    (with-current-buffer buf
      (when fff-consult--paging-state
        (aset fff-consult--paging-state fff-consult--st-loading nil))
      (when (and fff-consult--push fff-consult--paging-state
                 (equal query (aref fff-consult--paging-state fff-consult--st-query)))
        (fff-consult--load-more)))))

(defun fff-consult--scroll-load-more (win _start)
  "Fetch next page when WIN hits the end."
  (when (window-live-p win)
    (when-let* ((mb (active-minibuffer-window))
                (buf (window-buffer mb)))
      (when (buffer-live-p buf)
        (let ((st (buffer-local-value 'fff-consult--paging-state buf)))
          (when (and st (aref st fff-consult--st-query)
                     (not (aref st fff-consult--st-done))
                     (not (aref st fff-consult--st-loading))
                     (memq (window-buffer win)
                           (list buf (get-buffer "*Completions*")))
                     (with-current-buffer (window-buffer win)
                       (= (window-end win) (point-max))))
            (with-current-buffer buf
              (aset fff-consult--paging-state fff-consult--st-loading t)
              (run-with-idle-timer
               0 nil #'fff-consult--load-more-idle buf (aref st fff-consult--st-query)))))))))

(defvar-keymap fff-consult-grep-map
  :doc "Keys for `fff-consult-grep'."
  "M-s r" #'fff-consult-grep-cycle-mode
  "M-s c" #'fff-consult-grep-smart-case
  "M-s m" #'fff-consult-grep-more)

(defun fff-consult--export-query ()
  "Current grep query for export."
  (or (and (boundp 'fff-consult--paging-state)
           fff-consult--paging-state
           (aref fff-consult--paging-state fff-consult--st-query))
      (and (minibufferp)
           (condition-case nil
               (fff-consult--strip-query-tags
                (fff-consult--async-input (minibuffer-contents-no-properties)))
             (error nil)))))

(defun fff-consult--export-insert (cands)
  "Write CANDS to a grep-mode buffer."
  (if (fboundp 'embark-consult-export-grep)
      (embark-consult-export-grep cands)
    (let ((buf (generate-new-buffer "*Embark Export Grep*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (dolist (line cands)
            (insert line "\n")))
        (setq-local default-directory default-directory)
        (grep-mode))
      (pop-to-buffer buf))))

(defun fff-consult--export-grep (cands)
  "Export CANDS, re-fetching unlimited. Falls back to CANDS."
  (let* ((id (and (boundp 'fff-consult--session-id)
                  fff-consult--session-id))
         (cell (and (boundp 'fff-consult--mode-cell)
                    fff-consult--mode-cell))
         (mode (if (and (consp cell) (car cell))
                   (car cell)
                 fff-grep-mode))
         (smart (fff-consult--cell-smart-arg
                 (and (boundp 'fff-consult--smart-cell)
                      fff-consult--smart-cell)))
         (query (fff-consult--export-query))
         (raw (and (integerp id) (stringp query)
                   (> (length query) 0)
                   (fff--grep-fetch id query mode t smart))))
    (if (consp raw)
        (let* ((terms (fff--literal-terms query))
               (formatted (mapcar (lambda (m)
                                    (fff-consult--format-match m terms mode smart))
                                  raw)))
          (fff-consult--export-insert formatted))
      (fff-consult--export-insert cands))))

(defun fff-consult--capture-push (pipeline mode-cell smart-cell state id)
  "Save PIPELINE push fn and session state buffer-locally."
  (lambda (sink)
    (let ((down (funcall pipeline sink)))
      (lambda (action)
        (cond ((eq action 'setup)
               (setq-local fff-consult--push down)
               (setq-local fff-consult--mode-cell mode-cell)
               (setq-local fff-consult--smart-cell smart-cell)
               (setq-local fff-consult--paging-state state)
               (setq-local fff-consult--session-id id)
               (setq-local embark-exporters-alist
                           (cons '(consult-grep . fff-consult--export-grep)
                                 (bound-and-true-p embark-exporters-alist)))
               (add-hook 'window-scroll-functions
                         #'fff-consult--scroll-load-more nil t)
               (fff-consult--show-mode (car mode-cell) (car smart-cell)))
              ((eq action 'destroy)
               (remove-hook 'window-scroll-functions
                            #'fff-consult--scroll-load-more t)))
        (funcall down action)))))

(defun fff-consult--pick (prompt dir kind initial)
  "Consult pick. KIND `dir' for dirs. Returns (ID ROOT SEL QUERY)."
  (pcase-let ((`(,p ,paths ,edir)
               (consult--directory-prompt prompt (or dir (fff-project-root t)))))
    (let* ((root (fff--single-root prompt paths edir))
           (id (fff--session root))
           (default-directory root)
           (fetch (if (eq kind 'dir) #'fff--dir-fetch #'fff--file-fetch))
           (query nil)
           (sel (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'minibuffer-exit-hook
                                (lambda () (setq query (minibuffer-contents-no-properties)))
                                nil t))
                  (consult--read
                   (consult--dynamic-collection
                    (lambda (input) (funcall fetch id input))
                    :min-input 0
                    :throttle fff-throttle :debounce fff-debounce)
                   :prompt p :sort nil :require-match t
                   :state (consult--file-state)
                   :initial initial
                   :add-history (thing-at-point 'filename)
                   :category 'file
                   :history '(:input consult--find-history)))))
      (list id root sel query))))

;;;###autoload
(defun fff-consult-find-file (&optional dir initial)
  "Find a file with Consult + preview."
  (interactive "P")
  (fff--load)
  (pcase-let ((`(,id ,root ,sel ,query) (fff-consult--pick "Find" dir 'file initial)))
    (let ((abs (or (fff--selectable-file sel root)
                   (user-error "fff: no such file: %s" sel))))
      (fff--track-completion id query abs)
      (find-file abs))))

;;;###autoload
(defun fff-consult-find-dir (&optional dir initial)
  "Find a directory with Consult, open in dired."
  (interactive "P")
  (fff--load)
  (pcase-let ((`(,_id ,root ,sel ,_q) (fff-consult--pick "Find dir" dir 'dir initial)))
    (let ((abs (or (fff--selectable-dir sel root)
                   (user-error "fff: no such directory: %s" sel))))
      (dired abs))))

;;;###autoload
(defun fff-consult-grep (&optional dir initial)
  "FFF grep with consult and previews"
  (interactive "P")
  (fff--load)
  (pcase-let ((`(,c-prompt ,paths ,edir)
               (consult--directory-prompt "fff grep" (or dir (fff-project-root t)))))
    (let* ((root (fff--single-root "grep" paths edir))
           (id (fff--session root))
           (mode-cell (list (fff--normalize-grep-mode fff-grep-mode)))
           (smart-cell (list (if fff-grep-smart-case t nil)))
           (paging (fff-consult--make-paging-state))
           (prompt c-prompt)
           (default-directory root)
           (query nil)
           (sel (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'minibuffer-exit-hook
                                (lambda ()
                                  (setq query
                                        (minibuffer-contents-no-properties)))
                                nil t))
                    (consult--read
                     (consult--dynamic-collection
                      (lambda (input)
                        (fff-consult--fetch id input mode-cell paging smart-cell))
                      :min-input fff-min-grep-input
                      :throttle fff-throttle :debounce fff-debounce
                      :transform (consult--async-transform-by-input
                                  (lambda (input)
                                    (fff-consult--grep-format
                                     (fff-consult--strip-query-tags input)
                                     (car mode-cell)
                                     (fff-consult--cell-smart-arg smart-cell)))))
                     :prompt prompt
                     :lookup #'consult--lookup-member
                     :state (consult--grep-state)
                     :keymap fff-consult-grep-map
                     :async-wrap (lambda (pipeline)
                                   (consult--async-pipeline
                                    (consult--async-split)
                                    (fff-consult--capture-push pipeline mode-cell smart-cell paging id)
                                    (consult--async-indicator)
                                    (consult--async-refresh)))
                   :initial (or initial
                                (and (use-region-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))))
                   :add-history (thing-at-point 'symbol)
                   :require-match t
                   :category 'consult-grep
                   :group #'consult--prefix-group
                   :history '(:input consult--grep-history)
                   :sort nil))))
      (when (stringp sel)
        (let* ((file-end (next-single-property-change 0 'face sel))
               (file (substring-no-properties sel 0 file-end)))
          (fff--track-completion
           id (and (stringp query)
                    (fff-consult--strip-query-tags query))
           (expand-file-name file root))))
      sel)))

(provide 'fff-consult)
;;; fff-consult.el ends here
