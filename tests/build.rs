extern crate walkdir;

use std::fmt::Write;
use walkdir::WalkDir;

pub fn main() {
    let mut output = String::new();

    let mut dir_depth = 0;
    let mut ignore = false;
    for entry in WalkDir::new(".").min_depth(1).into_iter() {
        let entry = entry.expect("failed to open entry");

        let went_down = entry.depth() > dir_depth;

        if ignore && went_down {
            continue;
        }

        ignore = false;

        if !went_down {
            for _ in 0..(1 + dir_depth - entry.depth()) {
                write!(output, "}}\n").expect("");
            }

            dir_depth = entry.depth() - 1;
        }

        if entry.file_type().is_dir() {
            let ignore_file = entry.path().join("TEST_IGNORE.txt");
            if ignore_file.exists() {
                // Skip directory
                println!("cargo:rerun-if-changed={}", ignore_file.to_string_lossy());
                ignore = true;
                continue;
            }

            dir_depth = entry.depth();
            println!("cargo:rerun-if-changed={}", entry.path().to_string_lossy());

            write!(output, "mod {} {{\n", entry.file_name().to_string_lossy()).expect("");

            continue;
        }

        let f_name = String::from(entry.file_name().to_string_lossy());

        if !f_name.ends_with(".c") {
            continue;
        }

        let canonical = entry.path().canonicalize().expect("failed to canonicalize");
        println!("cargo:rerun-if-changed={}", entry.path().to_string_lossy());

        write!(
            output,
            "#[test]
fn test_{}() {{
    compiler::run_compiler_test_case(include_str!({:?}));
}}\n
",
            f_name.replace(|c: char| !c.is_alphanumeric(), "_"),
            canonical
        )
        .expect("failed to write to output");
    }

    for _ in 0..dir_depth {
        write!(output, "}}\n").expect("");
    }

    std::fs::write("./src/lib.rs", output).expect("failed to generate code");
}
