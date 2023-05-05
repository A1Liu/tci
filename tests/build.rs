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

        if ignore {
            if went_down {
                println!("cargo:warning=ignore {:?}", entry.path());
                continue;
            }

            ignore = false;
            dir_depth -= 1;
        }

        if !went_down {
            for d in 0..(1 + dir_depth - entry.depth()) {
                write!(output, "{}}}\n", "    ".repeat(dir_depth - d - 1)).expect("");
            }

            dir_depth = entry.depth() - 1;
        }

        if entry.file_type().is_dir() {
            let ignore_file = entry.path().join("TEST_IGNORE.txt");
            let prev_dir_depth = dir_depth;
            dir_depth = entry.depth();

            if ignore_file.exists() {
                // Skip directory
                println!("cargo:rerun-if-changed={}", ignore_file.to_string_lossy());
                ignore = true;
                continue;
            }

            println!("cargo:rerun-if-changed={}", entry.path().to_string_lossy());

            write!(
                output,
                "\n{}mod {} {{",
                "    ".repeat(prev_dir_depth),
                entry.file_name().to_string_lossy()
            )
            .expect("");

            continue;
        }

        let f_name = String::from(entry.file_name().to_string_lossy());

        if !f_name.ends_with(".c") {
            continue;
        }

        let canonical = entry.path().canonicalize().expect("failed to canonicalize");
        println!("cargo:rerun-if-changed={}", entry.path().to_string_lossy());

        let indent = "    ".repeat(dir_depth);
        output += &format!(
            "
{}#[test]
{}fn test_{}() {{
{}    compiler::run_compiler_test_case(include_str!({:?}));
{}}}
",
            indent,
            indent,
            f_name.replace(|c: char| !c.is_alphanumeric(), "_"),
            indent,
            canonical,
            indent,
        );
    }

    if ignore {
        dir_depth -= 1;
    }

    for _ in 0..dir_depth {
        write!(output, "}}\n").expect("");
    }

    std::fs::write("./src/lib.rs", output).expect("failed to generate code");
}
