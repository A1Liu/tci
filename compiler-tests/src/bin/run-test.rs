extern crate clap;
extern crate codespan_reporting;
extern crate compiler;

use clap::Parser;
use codespan_reporting::term::termcolor::*;
use codespan_reporting::term::*;
use compiler::{api::display_tree, parse_test_case, single_file_db, StageOutput};

#[derive(clap::ValueEnum, Clone, Copy, PartialEq)]
enum Stage {
    Lex,
    Macro,
    Parse,
    Validate,
}

const STAGES: &[Stage] = &[Stage::Lex, Stage::Macro, Stage::Parse, Stage::Validate];

/// Run
#[derive(Parser)]
#[clap(author = "Albert Liu", about = "Test runner for TCI.")]
struct Cli {
    #[clap(help = "a path to a test case")]
    test_case: std::path::PathBuf,

    #[clap(long, help = "parallelism for the compiler")]
    parallel: Option<u8>,

    #[clap(long, help = "print a nested version of the AST")]
    print_ast: bool,

    #[clap(
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

    #[clap(long, help = "run the compiler, but ignore all stage outputs")]
    ignore_all: bool,

    #[clap(long, help = "the only stage that should run")]
    #[arg(value_enum)]
    only: Option<Stage>,

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
    let main_begin = std::time::Instant::now();

    // Rust backtraces are useful and it seems dumb to disable them by default, especially in debug mode.
    #[cfg(debug_assertions)]
    std::env::set_var("RUST_BACKTRACE", "1");

    let mut args = Cli::parse();

    if let Some(threads) = args.parallel {
        rayon::ThreadPoolBuilder::new()
            .num_threads(threads as usize)
            .build_global()
            .unwrap();
    }

    let test_case =
        std::fs::read_to_string(&args.test_case).expect("file should exist and be a valid string");

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let (source, expected) = parse_test_case(&test_case);

    let begin = std::time::Instant::now();

    let (db, file_id) = single_file_db(source.to_string());

    let mut result = compiler::run_compiler_for_testing(&db, file_id);

    let elapsed = begin.elapsed();

    args.ignore = if args.ignore_all {
        STAGES.iter().map(|s| *s).collect()
    } else if let Some(only) = args.only {
        STAGES.iter().map(|s| *s).filter(|s| *s != only).collect()
    } else {
        args.ignore
    };

    for stage in args.ignore {
        match stage {
            Stage::Lex => result.lexer = StageOutput::Ignore,
            Stage::Macro => result.macro_expansion = StageOutput::Ignore,
            Stage::Parse => result.parsed_ast = StageOutput::Ignore,
            Stage::Validate => result.ast_validation = StageOutput::Ignore,
        }
    }

    if !args.out_file.is_some() && !args.write {
        for err in result.errors() {
            let diagnostic = result.translation_unit.diagnostic(err);
            codespan_reporting::term::emit(&mut writer.lock(), &config, &db, &diagnostic)
                .expect("wtf");

            if let Some(b) = &err.backtrace {
                eprintln!("{}", b);
            }
        }
    }

    if let (StageOutput::Ok(ast), true) = (&result.ast_validation, args.print_ast) {
        eprintln!("{}", display_tree(ast, Some(&result.ty_db)));
    } else if let (StageOutput::Ok(ast), true) = (&result.parsed_ast, args.print_ast) {
        eprintln!("{}", display_tree(ast, None));
    }

    assert_eq!(result, expected);

    let text = result.test_case(source);

    if let Some(out) = &args.out_file {
        std::fs::write(out, text).expect("failed to write file");
    } else if args.write {
        std::fs::write(&args.test_case, text).expect("failed to write file");
    } else {
        print!("{}", text);
    }

    eprintln!("");
    eprintln!("Compiler Time: {:.3?}", elapsed);
    eprintln!("Total Time: {:.3?}", main_begin.elapsed());
}
