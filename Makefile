MODULE = fff-core.so
TARGET = target/release/libfff_core.so

# Elisp compile/test dependencies. Explicit *_DIR vars win; otherwise
# checkouts under .el-deps are used (fetched by `make deps`, which
# `check` and `test` run first).
DEPS_DIR ?= .el-deps
COMPAT_URL = https://github.com/emacs-compat/compat.git
CONSULT_URL = https://github.com/minad/consult.git
EMBARK_URL = https://github.com/oantolin/embark.git
DUMB_JUMP_URL = https://github.com/jacktasia/dumb-jump.git
DASH_URL = https://github.com/magnars/dash.el.git

COMPAT_DIR ?= $(DEPS_DIR)/compat
CONSULT_DIR ?= $(DEPS_DIR)/consult
EMBARK_DIR ?= $(DEPS_DIR)/embark
DUMB_JUMP_DIR ?= $(DEPS_DIR)/dumb-jump
DASH_DIR ?= $(DEPS_DIR)/dash
LOAD_PATH = -L . $(if $(COMPAT_DIR),-L $(COMPAT_DIR)) $(if $(CONSULT_DIR),-L $(CONSULT_DIR)) $(if $(EMBARK_DIR),-L $(EMBARK_DIR)) $(if $(DUMB_JUMP_DIR),-L $(DUMB_JUMP_DIR)) $(if $(DASH_DIR),-L $(DASH_DIR))

all: $(MODULE)

$(MODULE): crates/fff-emacs/src/lib.rs crates/fff-emacs/Cargo.toml Cargo.toml
	cargo build --release
	cp $(TARGET) $(MODULE)

clean:
	cargo clean
	rm -f $(MODULE) fff.elc fff-consult.elc fff-dumb-jump.elc

deps: $(COMPAT_DIR) $(CONSULT_DIR) $(EMBARK_DIR) $(DUMB_JUMP_DIR) $(DASH_DIR)

$(COMPAT_DIR):
	git clone --depth 1 $(COMPAT_URL) $@
$(CONSULT_DIR):
	git clone --depth 1 $(CONSULT_URL) $@
$(EMBARK_DIR):
	git clone --depth 1 $(EMBARK_URL) $@
$(DUMB_JUMP_DIR):
	git clone --depth 1 $(DUMB_JUMP_URL) $@
$(DASH_DIR):
	git clone --depth 1 $(DASH_URL) $@

check: deps
	emacs --batch -L . -f batch-byte-compile fff.el
	emacs --batch $(LOAD_PATH) -f batch-byte-compile fff-consult.el
	emacs --batch $(LOAD_PATH) -f batch-byte-compile fff-dumb-jump.el

test: all check
	emacs --batch $(LOAD_PATH) -l fff-test.el
	emacs --batch $(LOAD_PATH) -l fff-consult-test.el
	emacs --batch $(LOAD_PATH) -l fff-dumb-jump-test.el

.PHONY: all clean check test deps
