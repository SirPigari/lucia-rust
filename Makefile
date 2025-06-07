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
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --bin lucia
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/debug/$(TARGET_EXE)" "$(TARGET)"
endif


release:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release --bin lucia
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/release/$(TARGET_EXE)" "$(TARGET)"
endif


run: $(TARGET)
	@$(MKDIR)
	@cd $(subst /,\,$(TARGET_DIR)) && $(RUN)

activate:
	@$(MKDIR)
ifeq ($(OS),Windows_NT)
	@cd $(subst /,\\,$(TARGET_DIR)) && $(RUN) --activate -e
else
	@cd $(subst \,/,$(TARGET_DIR)) && $(RUN) --activate -e
endif

test:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MKDIR)
	@if exist .\tests\run_tests.exe ( \
		.\tests\run_tests.exe $(filter-out $@,$(MAKECMDGOALS)) \
	) else if exist .\tests\run_tests ( \
		.\tests\run_tests $(filter-out $@,$(MAKECMDGOALS)) \
	) else ( \
		echo Error: run_tests executable not found in .\tests && exit /b 1 \
	)
else
	@$(MKDIR)
	@if [ -x ./tests/run_tests ]; then \
		./tests/run_tests $(filter-out $@,$(MAKECMDGOALS)); \
	elif [ -x ./tests/run_tests.exe ]; then \
		./tests/run_tests.exe $(filter-out $@,$(MAKECMDGOALS)); \
	else \
		echo "Error: run_tests executable not found in ./tests" && exit 1; \
	fi
endif

benchmark:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MKDIR)
	@if exist .\tests\run_benchmarks.exe ( \
		.\tests\run_benchmarks.exe $(filter-out $@,$(MAKECMDGOALS)) \
	) else if exist .\tests\run_benchmarks ( \
		.\tests\run_benchmarks $(filter-out $@,$(MAKECMDGOALS)) \
	) else ( \
		echo Error: run_benchmarks executable not found in .\tests && exit /b 1 \
	)
else
	@$(MKDIR)
	@if [ -x ./tests/run_benchmarks ]; then \
		./tests/run_benchmarks $(filter-out $@,$(MAKECMDGOALS)); \
	elif [ -x ./tests/run_benchmarks.exe ]; then \
		./tests/run_benchmarks.exe $(filter-out $@,$(MAKECMDGOALS)); \
	else \
		echo "Error: run_benchmarks executable not found in ./tests" && exit 1; \
	fi
endif

benchmark-save:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MKDIR)
	@if exist .\tests\run_benchmarks.exe ( \
		.\tests\run_benchmarks.exe $(filter-out $@,$(MAKECMDGOALS)) --save-results \
	) else if exist .\tests\run_benchmarks ( \
		.\tests\run_benchmarks $(filter-out $@,$(MAKECMDGOALS)) --save-results \
	) else ( \
		echo Error: run_benchmarks executable not found in .\tests && exit /b 1 \
	)
else
	@$(MKDIR)
	@if [ -x ./tests/run_benchmarks ]; then \
		./tests/run_benchmarks $(filter-out $@,$(MAKECMDGOALS)) --save-results; \
	elif [ -x ./tests/run_benchmarks.exe ]; then \
		./tests/run_benchmarks.exe $(filter-out $@,$(MAKECMDGOALS)) --save-results; \
	else \
		echo "Error: run_benchmarks executable not found in ./tests" && exit 1; \
	fi
endif

%:
	@:

build-test:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release --bin run_tests && $(CARGO_ENV) build --release --bin run_benchmarks
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@if not exist tests mkdir tests
	@$(MOVE) "$(LUCIA_DIR)\target\release\run_tests.exe" "tests\run_tests.exe"
	@$(MOVE) "$(LUCIA_DIR)\target\release\run_benchmarks.exe" "tests\run_benchmarks.exe"
else
	@mkdir -p tests
	@$(MOVE) "$(LUCIA_DIR)/target/release/run_tests" "tests/run_tests"
	@$(MOVE) "$(LUCIA_DIR)/target/release/run_benchmarks" "tests/run_benchmarks"
endif

clean:
	@cd $(LUCIA_DIR) && cargo clean
