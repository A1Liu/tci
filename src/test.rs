use crate::buckets::BucketList;
use crate::run_on_file;
use crate::util::{StringWriter, Void};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

fn test_file_should_succeed(filename: &str) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let buckets = BucketList::new();
    let mut files = SimpleFiles::new();
    let mut output = StringWriter::new();

    match run_on_file(&mut output, Void::new(), buckets, &mut files, filename) {
        Err(diagnostic) => {
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .expect("why did this fail?");
            panic!();
        }
        _ => {}
    }

    // let filename = String::from(filename);
    // assert!(output.to_string() == read_to_string(filename + ".out").expect("why did this fail?"));
}

fn test_file_should_fail(filename: &str) {
    let writer = StandardStream::stdout(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let buckets = BucketList::new();
    let mut files = SimpleFiles::new();

    match run_on_file(Void::new(), Void::new(), buckets, &mut files, filename) {
        Err(diagnostic) => {
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .expect("why did this fail?");
        }
        _ => panic!("should have failed"),
    }
}

#[test]
fn test_expr() {
    test_file_should_succeed("test/hello_world.c");
}

#[test]
fn test_recursive_struct() {
    test_file_should_fail("test/recursive_struct.c");
}

#[test]
fn test_variable_redefinition() {
    test_file_should_fail("test/var_redef.c");
}
