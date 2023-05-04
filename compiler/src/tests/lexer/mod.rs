#[test]
fn simple() {
    crate::run_compiler_test_case(include_str!("simple.c"), None);
}

#[test]
fn dotdot() {
    crate::run_compiler_test_case(include_str!("dotdot.c"), None);
}

// #[test]
// fn include() {
//     crate::run_test_code(include_str!("include.c"));
// }
