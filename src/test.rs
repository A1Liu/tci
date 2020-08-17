use crate::buckets::BucketList;
use crate::run_on_file;
use crate::util::{StringWriter, Void};
use codespan_reporting::files::SimpleFiles;

fn test_file_should_succeed(filename: &str) {
    let mut writer = StringWriter::new();
    let config = codespan_reporting::term::Config::default();

    let buckets = BucketList::new();
    let mut files = SimpleFiles::new();
    let mut output = StringWriter::new();

    match run_on_file(&mut output, Void::new(), buckets, &mut files, filename) {
        Err(diagnostic) => {
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                .expect("why did this fail?");
            println!("{}", writer.to_string());
            panic!();
        }
        _ => {}
    }

    // let filename = String::from(filename);
    // assert!(output.to_string() == read_to_string(filename + ".out").expect("why did this fail?"));
}

fn test_file_should_fail(filename: &str) {
    let mut writer = StringWriter::new();
    let config = codespan_reporting::term::Config::default();

    let buckets = BucketList::new();
    let mut files = SimpleFiles::new();

    match run_on_file(Void::new(), Void::new(), buckets, &mut files, filename) {
        Err(diagnostic) => {
            codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                .expect("why did this fail?");
            println!("{}", writer.to_string());
        }
        _ => panic!("should have failed"),
    }
}

#[test]
fn test_hello_world() {
    test_file_should_succeed("test/hello_world.c");
}
