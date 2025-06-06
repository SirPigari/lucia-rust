# Detect OS
IS_WINDOWS := $(shell echo $(ComSpec))
IS_WINDOWS_CMD := $(findstring cmd.exe,$(IS_WINDOWS))

LUCIA_DIR := .
TARGET_DIR := $(LUCIA_DIR)/src/env/bin
TARGET_EXE := lucia$(if $(IS_WINDOWS_CMD),.exe,)
TARGET := $(TARGET_DIR)/$(TARGET_EXE)

ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	CARGO_ENV := set RUSTFLAGS=-Awarnings && cargo
	MKDIR := if not exist "$(subst /,\,$(TARGET_DIR))" mkdir "$(subst /,\,$(TARGET_DIR))"
	MOVE := move /Y
	RUN := $(TARGET_EXE)
	SHELL := cmd
	.SHELLFLAGS := /C
	TEST_LOOP := for %%f in (src\env\Docs\examples\tests\*.lc) do (
	TEST_FILE := %%f
else
	CARGO_ENV := RUSTFLAGS=-Awarnings cargo
	MKDIR := mkdir -p $(TARGET_DIR)
	MOVE := mv -f
	RUN := ./$(TARGET_EXE)
	TEST_LOOP := for f in src/env/Docs/examples/tests/*.lc; do
	TEST_FILE := $$f
endif

.PHONY: all deps build release run activate test test-all clean

all: deps build run

deps:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
else
	@which cargo >/dev/null || curl https://sh.rustup.rs -sSf | sh -s -- -y
	@sudo apt-get update && sudo apt-get install -y build-essential pkg-config libssl-dev
endif

build:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build
	@$(MKDIR)
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"

release:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release
	@$(MKDIR)
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"

run: $(TARGET)
	@cd $(subst /,\,$(TARGET_DIR)) && $(RUN)

activate:
	@cd $(subst /,\,$(TARGET_DIR)) && $(RUN) --activate

test:
	@cd $(subst /,\,$(TARGET_DIR)) && $(RUN) ../../tests/test.lucia

test-all:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@for %%f in (src\env\Docs\examples\tests\*.lc) do ( \
		echo Running: "%%f" && \
		"$(subst /,\,$(TARGET))" "%%f" -q \
	)
else
	@for f in src/env/Docs/examples/tests/*.lc; do \
		echo Running: "$$f"; \
		$(TARGET) "$$f" -q; \
	done
endif


clean:
	@cd $(LUCIA_DIR) && cargo clean
