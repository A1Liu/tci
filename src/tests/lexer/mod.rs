#[test]
fn simple() {
    crate::run_test_code(include_str!("simple.c"));
}

#[test]
fn inclue() {
    crate::run_test_code(include_str!("include.c"));
}
