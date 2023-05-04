#[test]
fn simple() {
    crate::run_compiler_test_case(include_str!("simple.c"));
}

#[test]
fn dotdot() {
    crate::run_compiler_test_case(include_str!("dotdot.c"));
}

// #[test]
// fn include() {
//     crate::run_test_code(include_str!("include.c"));
// }
