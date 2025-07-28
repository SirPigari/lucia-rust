use std::collections::HashMap;
use once_cell::sync::Lazy;

// This module defines the standard libraries available in the runtime environment.
// Each library is represented by a name and a description, along with its version.
// The libraries are stored in a static HashMap for easy access.

// Lucia Version 2.0.0
// This file is part of the Lucia programming language runtime.

#[derive(Debug, Clone)]
pub struct LibInfo {
    pub description: &'static str,
    pub version: &'static str,
    pub expected_lucia_version: &'static str,
}

pub static STD_LIBS: Lazy<HashMap<&'static str, LibInfo>> = Lazy::new(|| {
    let mut m = HashMap::new();

    m.insert("math", LibInfo {
        description: "Provides mathematical functions and constants.",
        version: "1.0.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("os", LibInfo {
        description: "Interfaces with the operating system.",
        version: "1.0.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("time", LibInfo {
        description: "Handles time and date functionality.",
        version: "0.3.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("json", LibInfo {
        description: "JSON parsing and serialization.",
        version: "1.0.82",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("config", LibInfo {
        description: "Lucia configuration management.",
        version: "0.2.6",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("regex", LibInfo {
        description: "Regular expressions for pattern matching.",
        version: "0.9.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("collections", LibInfo {
        description: "Collection of utilities.",
        version: "1.0.0",
        expected_lucia_version: "^2.0.0",
    });
    
    m.insert("random", LibInfo {
        description: "Random number generation utilities.",
        version: "0.7.42",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("fs", LibInfo {
        description: "File system operations and utilities.",
        version: "0.4.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("clib", LibInfo {
        description: "C standard library bindings for Lucia.",
        version: "0.1.69",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("lasm", LibInfo {
        description: "Cross-platform, lightweight assembly-inspired utilities for low-level programming and direct hardware control.",
        version: "1.0.3",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("nest", LibInfo {
        description: "HTTP client and server utilities.",
        version: "1.1.0",
        expected_lucia_version: "^2.0.0",
    });

    m.insert("libload", LibInfo {
        description: "Dynamic library loading and function invocation.",
        version: "1.0.0",
        expected_lucia_version: "^2.0.0",
    });

    m
});


// ------- Macros -------

#[macro_export]
macro_rules! insert_native_fn {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr) => {{
        let native_fn = NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
        );

        let func = Function::Native(Arc::new(native_fn));

        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                Value::Function(func),
                "function".to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}

#[macro_export]
macro_rules! insert_native_fn_state {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $state:expr) => {{
        let native_fn = NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            Some($state),
        );

        let func = Function::Native(Arc::new(native_fn));

        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                Value::Function(func),
                "function".to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}

#[macro_export]
macro_rules! insert_native_var {
    ($map:expr, $name:expr, $value:expr, $typ:expr) => {{
        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                $value,
                $typ.to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}
