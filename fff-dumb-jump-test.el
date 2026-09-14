;;; fff-dumb-jump-test.el --- Behavior tests for the dumb-jump shim -*- lexical-binding: t; -*-
(require 'cl-lib)
(add-to-list 'load-path default-directory)
(setq fff-core-file (expand-file-name "fff-core.so" default-directory))
(require 'fff)
(require 'fff-dumb-jump)

(if (not (require 'dumb-jump nil t))
    (message "SKIP dumb-jump tests (dumb-jump not loadable)")

(defvar fff-dj-test--root
  (expand-file-name "dumb-jump-fixture" temporary-file-directory))

(let ((root fff-dj-test--root))
  (make-directory root t)
  (with-temp-file (expand-file-name "hello.el" root)
    (insert ";;; hello.el\n(defun hello-greet ()\n  (message \"hello world\"))\n"))
  (make-directory (expand-file-name "excluded" root) t)
  (with-temp-file (expand-file-name "excluded/dup.el" root)
    (insert ";;; dup.el\n(defun hello-greet ()\n  (message \"dup\"))\n"))
  (with-temp-file (expand-file-name "notes.py" root)
    (insert "# notes.py\n# (defun hello-greet ()\n"))
  (let ((default-directory (file-name-as-directory root))
        (cur (expand-file-name "other.el" root)))
    (with-temp-file cur (insert ";;; other.el\n"))
    (fff-dumb-jump-setup)
    (let ((id (fff--session root)))
      (unless (fff--wait-for-scan id 10000)
        (error "scan timed out"))
      ;; PCRE2-only patterns go to shell.
      (dolist (p '("(?=foo)" "a\\1" "^[[:space:]]*([][[:alnum:]]+)+foo" "x[^=]*?({y}"))
        (unless (fff-dumb-jump--pcre2-only-p p)
          (error "pcre2 missed: %S" p)))
      (when (fff-dumb-jump--pcre2-only-p "\\((defun\\s-+hello\\j")
        (error "false positive on elisp rule"))
      ;; Project gate + format column.
      (unless (fff-dumb-jump--for-project-p root)
        (error "project gate rejected root"))
      (let ((formatted (fff-dumb-jump--format
                        root (concat "hello.el" "\x1f" "2" "\x1f" "7"
                                      "\x1f" "(defun hello-greet ()"))))
        (unless (string= formatted
                         (concat (expand-file-name "hello.el" root) ":2:8:(defun hello-greet ()"))
          (error "format dropped column: %S" formatted)))
      ;; Query uses raw-ex with knobs; case follows language.
      (let ((captured nil)
            (fff-max-grep-results 7)
            (fff-max-matches-per-file 11)
            (fff-max-file-size 12345)
            (fff-time-budget-ms 99)
            (fff-enforce-time-budget t)
            (fff-grep-smart-case t))
        (cl-letf (((symbol-function 'fff--grep-raw-ex)
                   (lambda (sid pattern mode max-matches budget per-file size enforce smart)
                     (setq captured (list sid pattern mode max-matches budget per-file size enforce smart))
                     '("mocked"))))
          (unless (equal (fff-dumb-jump--query id "hello" "elisp") '("mocked"))
            (error "query did not return raw-ex result"))
          (unless (equal captured (list id "hello" "regex" 7 99 11 12345 1 0))
            (error "query missed knobs: %S" captured))))
      (unless (= (fff-dumb-jump--smart-arg "elisp" "hello") 0)
        (error "elisp must stay sensitive"))
      (unless (= (fff-dumb-jump--smart-arg "commonlisp" "hello") 1)
        (error "commonlisp lowercase must go smart"))
      (unless (fff-dumb-jump--shell-only-p '("x(?!y)") "elisp")
        (error "pcre2 must need shell"))
      (when (fff-dumb-jump--shell-only-p '("hello") "elisp")
        (error "plain elisp must use the index"))
      ;; End-to-end: finds defun, drops excluded dir + wrong language.
      (let ((results (dumb-jump-run-command
                      "hello-greet" root '("\\((defun\\s-+JJJ\\j") "elisp"
                      nil cur 1 #'dumb-jump-parse-rg-response #'dumb-jump-generate-rg-command)))
        (unless (seq-some (lambda (it) (string-suffix-p "hello.el" (plist-get it :path))) results)
          (error "shim missed hello.el: %S" results))
        (let ((filtered (dumb-jump-run-command
                         "hello-greet" root '("\\((defun\\s-+JJJ\\j") "elisp"
                         (list (expand-file-name "excluded" root)) cur 1
                         #'dumb-jump-parse-rg-response #'dumb-jump-generate-rg-command)))
          (when (seq-some (lambda (it) (string-match-p "excluded/" (plist-get it :path))) filtered)
            (error "excluded dir leaked: %S" filtered))
          (when (seq-some (lambda (it) (string-suffix-p ".py" (plist-get it :path))) filtered)
            (error "wrong language leaked: %S" filtered))))
      ;; PCRE2 fallback returns a list, errors when disabled.
      (let ((fff-dumb-jump-fallback-to-shell t))
        (unless (listp (dumb-jump-run-command
                        "hello-greet" root '("x(?!y)JJK") "elisp" nil cur 1
                        #'dumb-jump-parse-rg-response #'dumb-jump-generate-rg-command))
          (error "shell fallback did not return a list")))
      (let ((fff-dumb-jump-fallback-to-shell nil) (failed nil))
        (condition-case err
            (dumb-jump-run-command
             "hello-greet" root '("x(?!y)JJK") "elisp" nil cur 1
             #'dumb-jump-parse-rg-response #'dumb-jump-generate-rg-command)
          (user-error (setq failed t)))
        (unless failed (error "disabled fallback did not signal"))))
    (fff-dumb-jump-teardown)
    (message "SMOKE dj OK"))))
;;; fff-dumb-jump-test.el ends here
