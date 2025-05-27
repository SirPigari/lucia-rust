LUCIA_DIR := .
TARGET := $(LUCIA_DIR)/src/env/bin
RUSTFLAGS := -Awarnings

all: build run

build:
	@cmd /C "cd $(LUCIA_DIR) && set RUSTFLAGS=$(RUSTFLAGS) && cargo build"
	@cmd /C "move $(LUCIA_DIR)\target\debug\lucia.exe $(TARGET)\lucia.exe"
	@echo Build complete. Executable is located at $(TARGET)\lucia.exe


run: $(TARGET)
	@cd $(TARGET) && lucia

activate:
	@cd $(TARGET) && lucia --activate

$(TARGET):
	@$(MAKE) build
	@$(MAKE) run

test:
	@cd $(TARGET) && lucia ../tests/test.lucia

clean:
	@cd $(LUCIA_DIR) && cargo clean
