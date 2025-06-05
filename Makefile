LUCIA_DIR := .
TARGET := $(LUCIA_DIR)/src/env/bin
RUSTFLAGS := -Awarnings

all: build run

build:
	@cmd /C "cd $(LUCIA_DIR) && set RUSTFLAGS=$(RUSTFLAGS) && cargo build"
	@cmd /C "move $(LUCIA_DIR)\target\debug\lucia.exe $(TARGET)\lucia.exe"
	@echo Build complete. Executable is located at $(TARGET)\lucia.exe

release:
	@cmd /C "cd $(LUCIA_DIR) && set RUSTFLAGS=$(RUSTFLAGS) && cargo build --release"
	@cmd /C "move $(LUCIA_DIR)\target\release\lucia.exe $(TARGET)\lucia.exe"
	@echo Release build complete. Executable is located at $(TARGET)\lucia.exe

run: $(TARGET)
	@cd $(TARGET) && lucia

activate:
	@cd $(TARGET) && lucia --activate

$(TARGET):
	@$(MAKE) build
	@$(MAKE) run

test-all:
	@echo Running all test files in src/env/Docs/examples/tests...
	@for %%f in (src/env/Docs/examples/tests/*.lc) do ( \
		echo. && \
		echo Executing file: "%%f" && \
		"$(TARGET)\\lucia.exe" "src/env/Docs/examples/tests/%%f" -q \
	)

test:
	@cd $(TARGET) && lucia ../../tests/test.lucia

clean:
	@cd $(LUCIA_DIR) && cargo clean
