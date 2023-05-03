use clap::Parser;

use compiler::StageOutput;

#[derive(clap::ValueEnum, Clone, Copy)]
enum Stage {
    Lex,
    Macro,
    Parse,
}

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    test_case: std::path::PathBuf,

    #[clap(short, long)]
    #[arg(value_enum)]
    ignore: Vec<Stage>,

    #[clap(short, long)]
    write: bool,

    #[clap(short, long)]
    out_file: Option<std::path::PathBuf>,
}

fn main() {
    let args = Cli::parse();

    let test_case =
        std::fs::read_to_string(&args.test_case).expect("file should exist and be a valid string");

    let (source, mut result) = compiler::api::run_test_code(&*test_case);

    for stage in args.ignore {
        match stage {
            Stage::Lex => result.lexer = StageOutput::Ignore,
            Stage::Macro => result.macro_expansion = StageOutput::Ignore,
            Stage::Parse => result.parsed_ast = StageOutput::Ignore,
        }
    }

    let text = result.test_case(source);

    if let Some(out) = &args.out_file {
        std::fs::write(out, text).expect("failed to write file");
    } else if args.write {
        std::fs::write(&args.test_case, text).expect("failed to write file");
    } else {
        print!("{}", text);
    }
}
