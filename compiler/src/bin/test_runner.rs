use clap::Parser;
use codespan_reporting::term::termcolor::*;
use codespan_reporting::term::*;
use compiler::{parse_test_case, StageOutput};

#[derive(clap::ValueEnum, Clone, Copy)]
enum Stage {
    Lex,
    Macro,
    Parse,
}

/// Run
#[derive(Parser)]
#[clap(author = "Albert Liu", about = "Test runner for TCI.")]
struct Cli {
    #[clap(help = "a path to a test case")]
    test_case: std::path::PathBuf,

    #[clap(
        short,
        long,
        value_delimiter = ',',
        help = "a stage to ignore",
        long_help = r#"A stage to ignore. This can be repeated, or you can pass
the stage names as a comma-separated list.

Examples:
"lex,macro" skips the lexing and macro expansion stages."#
    )]
    #[arg(value_enum)]
    ignore: Vec<Stage>,

    #[clap(
        short,
        long,
        help = "output the result to OUT_FILE. Overrides `--write`"
    )]
    out_file: Option<std::path::PathBuf>,

    #[clap(short, long, help = "write to the input file in-place")]
    write: bool,
}

fn main() {
    let args = Cli::parse();

    let test_case =
        std::fs::read_to_string(&args.test_case).expect("file should exist and be a valid string");

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    let print_err: compiler::PrintFunc = &|files, tu, err| {
        let diagnostic = tu.diagnostic(err);
        codespan_reporting::term::emit(&mut writer.lock(), &config, files, &diagnostic)
            .expect("wtf");

        if let Some(b) = &err.backtrace {
            println!("{}", b);
        }
    };

    let (source, expected) = parse_test_case(&test_case);

    let print_err = if args.out_file.is_some() || args.write {
        None
    } else {
        Some(print_err)
    };

    let mut result = compiler::run_compiler_for_testing(source.to_string(), print_err);
    assert_eq!(result, expected);

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
