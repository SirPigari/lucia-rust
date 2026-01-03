# Installation Guide

Welcome to the installation guide for Lucia! You can install Lucia using one of the following methods:

- Using [**Git Clone**](#method-1-git-clone) method  
- Using the [**Installer from GitHub**](#method-2-installer-from-github) method

---

## Method 1: Git Clone

To install Lucia by cloning the repository and building from source, follow these steps:

1. Open a terminal window.

2. Clone the Lucia repository:

    ```console
    git clone https://github.com/SirPigari/lucia-rust.git lucia
    ```

3. Navigate into the cloned directory:

    ```console
    cd lucia
    ```

4. Build Lucia using Cargo (requires Rust toolchain and Makefile installed):

    ```console
    make release
    ```

5. Activate the Lucia environment:

    ```console
    make activate
    ```

6. (Optional) To install the binary system-wide:

    ```console
    make install
    ```

7. Verify the installation by running:

    ```console
    lucia --version
    ```

---

## Method 2: Installer from GitHub

Alternatively, you can download and run the pre-built installer:

1. Go to the [Lucia releases](https://github.com/SirPigari/lucia-rust/releases/latest).

2. Download the installer (only on Windows).

3. Run the installer and follow the on-screen instructions.

4. After installation, open a terminal and verify:

```console
lucia --version
```

---

### Post-Installation Setup

Make sure Lucia is added to your system's PATH. If it isn't, add the directory containing the `lucia` executable manually.

---

### Next Steps

Start writing and running Lucia programs! For a quick start, see the [Getting Started](./getting-started.md) guide.

If you run into any issues, check the [Lucia GitHub Issues](https://github.com/SirPigari/lucia-rust/issues) page for help or to report problems.
