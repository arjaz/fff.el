# fff.el - fast fuzzy files and grep for Emacs

Emacs frontend for [fff](https://github.com/dmtrKovalenko/fff).
Keeps one in-memory index per project and queries it directly.
Provides the following project-aware commads:
- `fff-find-file`
- `fff-find-dir`
- `fff-grep`

You can additionally use fff-consult with the following:
- `fff-consult-find-file`
- `fff-consult-find-dir`
- `fff-consult-grep`

And fff-dumb-jump integration:
- `fff-dumb-jump-setup`

## Requirements

- Emacs 29.1+ with dynamic modules (`(featurep 'modules)` must be `t`).
- Rust stable toolchain to build.
- Optional: Consult 2.0+ for `fff-consult.el`.
- Optional: `embark` + `embark-consult` for collect/export.
- Optional: `dumb-jump`

## Installation

Build:
```sh
git clone https://github.com/arjaz/fff.el.git && cd fff.el
make
```

Configure:

```elisp
(use-package fff
  :vc (:url "https://github.com/arjaz/fff.el")
  :bind
  ([remap project-find-file] . fff-find-file)
  ([remap project-find-dir] . fff-find-dir)
  ([remap project-find-regexp] . fff-find-grep)
  :custom
  (fff-grep-mode 'fuzzy))

(use-package fff-consult
  :vc (:url "https://github.com/arjaz/fff.el")
  :bind
  ;; also optionally bind fff-consult-find-file and fff-consult-find-dir
  ("M-s r" . fff-consult-grep))

(use-package fff-dumb-jump
  :vc (:url "https://github.com/arjaz/fff.el")
  :after dumb-jump
  :config
  (fff-dumb-jump-setup))
```

## Consult
Consult commands support previews and Embark exporting.
`fff-consult-grep` allows changing matching between literal/regex/fuzzy with `M-s r` and cycling smart-case with `M-s c`.
