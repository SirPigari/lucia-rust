IS_WINDOWS := $(shell echo $(ComSpec))
IS_WINDOWS_CMD := $(findstring cmd.exe,$(IS_WINDOWS))

LUCIA_DIR := .
TARGET_DIR := $(LUCIA_DIR)/src/env/bin
TARGET_EXE := lucia$(if $(IS_WINDOWS_CMD),.exe,)
TARGET := $(TARGET_DIR)/$(TARGET_EXE)
TARGET_STANDALONE := $(TARGET_DIR)/lucia-standalone$(if $(IS_WINDOWS_CMD),.exe,)

ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	CARGO_ENV := cargo
	MKDIR := if not exist "$(subst /,\,$(TARGET_DIR))" mkdir "$(subst /,\,$(TARGET_DIR))"
	MOVE := move /Y
	RM := del /F /Q
	RUN := $(TARGET_EXE)
	RUN_FULL := $(subst /,\,$(TARGET_DIR))\$(TARGET_EXE)
	SHELL := cmd
	.SHELLFLAGS := /C
	TEST_LOOP := for %%f in (src\env\Docs\examples\tests\*.lc) do (
	TEST_FILE := %%f
else
	CARGO_ENV := cargo
	MKDIR := mkdir -p $(TARGET_DIR)
	MOVE := - mv -f
	RM := - rm -f
	RUN := ./$(TARGET_EXE)
	RUN_FULL := $(TARGET_DIR)/$(TARGET_EXE)
	TEST_LOOP := for f in src/env/Docs/examples/tests/*.lc; do
	TEST_FILE := $$f
endif

ifeq ($(OS),Windows_NT)
    TARGET_LIB_STATIC  := lucia.lib
    TARGET_LIB_DYNAMIC := lucia.dll
else ifeq ($(shell uname -s),Darwin)
    TARGET_LIB_STATIC  := liblucia.a
    TARGET_LIB_DYNAMIC := liblucia.dylib
else
    TARGET_LIB_STATIC  := liblucia.a
    TARGET_LIB_DYNAMIC := liblucia.so
endif

.PHONY: all build release run activate test benchmark benchmark-save build-tests clean deps help lucia

all: deps build run

ifeq ($(IS_WINDOWS_CMD),cmd.exe)
else
	@which cargo >/dev/null || curl https://sh.rustup.rs -sSf | sh -s -- -y

	@for pkg in base-devel pkgconf openssl; do \
		if ! pacman -Qi $$pkg >/dev/null 2>&1; then \
			echo "Installing $$pkg..."; \
			sudo pacman -Syu --noconfirm $$pkg; \
		fi \
	done
endif

build:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --bin lucia
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/debug/$(TARGET_EXE)" "$(TARGET)"
endif

build-lib:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --lib
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_LIB_STATIC)" $(TARGET_DIR)/"$(subst /,\,$(TARGET_LIB_STATIC))"
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_LIB_DYNAMIC)" $(TARGET_DIR)/"$(subst /,\,$(TARGET_LIB_DYNAMIC))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/debug/$(TARGET_LIB_STATIC)" $(TARGET_DIR)/"$(TARGET_LIB_STATIC)"
	@$(MOVE) "$(LUCIA_DIR)/target/debug/$(TARGET_LIB_DYNAMIC)" $(TARGET_DIR)/"$(TARGET_LIB_DYNAMIC)"
endif

build-single:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --bin lucia --features single_executable
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\debug\$(TARGET_EXE)" "$(subst /,\,$(TARGET_STANDALONE))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/debug/$(TARGET_EXE)" "$(TARGET_STANDALONE)"
endif

build-wasm:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@cd $(LUCIA_DIR) && if exist build.rs rename build.rs _build.rs
	@cd $(LUCIA_DIR) && if exist src\env\runtime\wasm.rs copy src\env\runtime\wasm.rs src\main_wasm.rs
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --target wasm32-unknown-unknown --bin lucia_wasm --release --features preprocessor_include_std --config 'build.rustflags=["--cfg","getrandom_backend=\"wasm_js\""]' || echo Build failed
	@cd $(LUCIA_DIR) && if exist _build.rs rename _build.rs build.rs
	@cd $(LUCIA_DIR) && if exist src\main_wasm.rs del src\main_wasm.rs
else
	@cd $(LUCIA_DIR) && [ -f build.rs ] && mv build.rs _build.rs || true
	@cd $(LUCIA_DIR) && [ -f src/env/runtime/wasm.rs ] && cp src/env/runtime/wasm.rs src/main_wasm.rs || true
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --target wasm32-unknown-unknown --bin lucia_wasm --release --features preprocessor_include_std --config 'build.rustflags=["--cfg","getrandom_backend=\"wasm_js\""]' || true
	@cd $(LUCIA_DIR) && [ -f _build.rs ] && mv _build.rs build.rs || true
	@cd $(LUCIA_DIR) && [ -f src/main_wasm.rs ] && rm src/main_wasm.rs || true
endif

wasm: build-wasm
single: build-single
standalone: build-single
lib: build-lib

run-wasm: build-wasm
	@wasmtime $(LUCIA_DIR)/target/wasm32-unknown-unknown/debug/lucia_wasm.wasm

release:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release --bin lucia
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_EXE)" "$(subst /,\,$(TARGET))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/release/$(TARGET_EXE)" "$(TARGET)"
endif

release-lib:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release --lib
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_LIB_STATIC)" $(TARGET_DIR)/"$(subst /,\,$(TARGET_LIB_STATIC))"
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_LIB_DYNAMIC)" $(TARGET_DIR)/"$(subst /,\,$(TARGET_LIB_DYNAMIC))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/release/$(TARGET_LIB_STATIC)" $(TARGET_DIR)/"$(TARGET_LIB_STATIC)"
	@$(MOVE) "$(LUCIA_DIR)/target/release/$(TARGET_LIB_DYNAMIC)" $(TARGET_DIR)/"$(TARGET_LIB_DYNAMIC)"
endif

release-single:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) build --release --bin lucia --features single_executable
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MOVE) "$(LUCIA_DIR)\target\release\$(TARGET_EXE)" "$(subst /,\,$(TARGET_STANDALONE))"
else
	@$(MOVE) "$(LUCIA_DIR)/target/release/$(TARGET_EXE)" "$(TARGET_STANDALONE)"
endif

full: release release-lib release-single wasm installer

run: $(TARGET)
	@$(MKDIR)
	@$(RUN_FULL) $(filter-out $@,$(MAKECMDGOALS))

activate:
	@$(MKDIR)
ifeq ($(OS),Windows_NT)
	@cd $(subst /,\\,$(TARGET_DIR)) && $(RUN) --activate -e
else
	@cd $(subst \,/,$(TARGET_DIR)) && $(RUN) --activate -e
endif

test-stdout:
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(MKDIR)
	@if not exist .\tests\stdout mkdir .\tests\stdout
	@if exist .\tests\run_tests.exe ( \
		.\tests\run_tests.exe $(filter-out $@,$(MAKECMDGOALS)) --stdout=.\tests\stdout\ \
	) else if exist .\tests\run_tests ( \
		.\tests\run_tests $(filter-out $@,$(MAKECMDGOALS)) --stdout=.\tests\stdout\ \
	) else ( \
		echo Error: run_tests executable not found in .\tests && exit /b 1 \
	)
else
	@$(MKDIR)
	@mkdir -p ./tests/stdout
	@if [ -x ./tests/run_tests ]; then \
		./tests/run_tests $(filter-out $@,$(MAKECMDGOALS)) --stdout=./tests/stdout/ ; \
	elif [ -x ./tests/run_tests.exe ]; then \
		./tests/run_tests.exe $(filter-out $@,$(MAKECMDGOALS)) --stdout=./tests/stdout/ ; \
	else \
		echo "Error: run_tests executable not found in ./tests" && exit 1; \
	fi
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

installer-dbg:
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(RM) "$(LUCIA_DIR)\src\env\assets\installer\LuciaInstaller.exe"
	@cd $(LUCIA_DIR) && cd src\env\assets\installer && "C:\Program Files (x86)\NSIS\makensis.exe" LuciaInstaller.nsi
else
	@$(RM) "$(LUCIA_DIR)/src/env/assets/installer/LuciaInstaller.exe"
	@cd $(LUCIA_DIR) && cd src/env/assets/installer && makensis LuciaInstaller.nsi
endif

installer:
	@$(MKDIR)
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@$(RM) "$(LUCIA_DIR)\src\env\assets\installer\LuciaInstaller.exe"
	@cd $(LUCIA_DIR) && cd src\env\assets\installer && "C:\Program Files (x86)\NSIS\makensis.exe" "/DRELEASE_BUILD" LuciaInstaller.nsi
else
	@$(RM) "$(LUCIA_DIR)/src/env/assets/installer/LuciaInstaller.exe"
	@cd $(LUCIA_DIR) && cd src/env/assets/installer && makensis "-DRELEASE_BUILD" LuciaInstaller.nsi
endif

build-test: build-tests
build-tests:
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
ifeq ($(IS_WINDOWS_CMD),cmd.exe)
	@cmd /c "if exist benchmark-results rmdir /s /q benchmark-results"
	@cmd /c "if exist tests\stdout rmdir /s /q tests\stdout"
	@cmd /c "if exist temp rmdir /s /q temp"
else
	@rm -rf "$(LUCIA_DIR)/tests/stdout" "$(LUCIA_DIR)/temp" "$(LUCIA_DIR)/benchmark-results"
endif

01 02 03 04 05 06 07 08 09 \
10 11 12 13 14 15 16 17 18 19 \
20 21 22 23 24 25 26 27 28 29 \
30 31 32 33 34 35 36 37 38 39 \
40 41 42 43 44 45 46 47 48 49 \
50 51 52 53 54 55 56 57 58 59 \
60 61 62 63 64 65 66 67 68 69 \
70 71 72 73 74 75 76 77 78 79 \
80 81 82 83 84 85 86 87 88 89 \
90 91 92 93 94 95 96 97 98 99:
	@:


check:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) check --bin lucia

install:
	@cd $(LUCIA_DIR) && $(CARGO_ENV) install --path . --bin lucia
	@echo Lucia installed system-wide. You can now run 'lucia' from anywhere.

help:
	@echo Available targets:
	@echo all             - deps + build + run
	@echo activate        - Activate the environment
	@echo build           - Build debug binary
	@echo release         - Build release binary
	@echo run             - Run the program
	@echo test            - Run tests
	@echo benchmark       - Run benchmarks
	@echo benchmark-save  - Run benchmarks ^& save results
	@echo build-tests     - Build test/benchmark binaries
	@echo clean           - Clean cargo artifacts
	@echo deps            - Install dependencies (Unix only)
	@echo help            - Show this help message
	@echo lucia           - Run Lucia with specified flags
	@echo check           - Check the project
	@echo install         - Install Lucia system-wide
