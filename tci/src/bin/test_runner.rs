extern crate tci;
use clap::Parser;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    test_case: std::path::PathBuf,

    #[clap(short, long)]
    write: bool,

    #[clap(short, long)]
    out_file: Option<std::path::PathBuf>,
}

fn main() {
    let args = Cli::parse();

    let test_case =
        std::fs::read_to_string(&args.test_case).expect("file should exist and be a valid string");

    let result = tci::api::run_test_code(&*test_case);

    let text = result.test_case();

    if let Some(out) = &args.out_file {
        std::fs::write(out, text).expect("failed to write file");
    } else if args.write {
        std::fs::write(&args.test_case, text).expect("failed to write file");
    } else {
        print!("{}", text);
    }
}
