// THIS FILE ISN'T USED IN THE CODEBASE RIGHT NOW; IT'S PURELY FOR REFERENCE WHEN
// IMPLEMENTING FUTURE STUFF

pub fn truth_value_of_struct(value: &Expr, value_type: &TCType) -> Result<(), Error> {
    if let TCType {
        kind: TCTypeKind::Struct { .. },
        ..
    } = value_type
    {
        return Err(Error::new(
            "tried to check truth value of struct",
            vec![(value.range.clone(), "value is a struct type".to_string())],
        ));
    }

    return Ok(());
}

pub fn variable_is_void(decl_range: Range) -> Error {
    return Error::new(
        "variable has type void",
        vec![(decl_range, "variable is here".to_string())],
    );
}

pub fn assignment_convert_incompatible(target_range: Range, value_range: Range) -> Error {
    return Error::new(
        "value cannot be converted to target type",
        vec![
            (target_range, "target type is here".to_string()),
            (value_range, "value is here".to_string()),
        ],
    );
}

pub fn unexpected_return_value(value_range: Range) -> Error {
    return Error::new(
        "didn't expect value in return statement (return type is void)",
        vec![(value_range, "value is here".to_string())],
    );
}

pub fn expected_return_value(target_range: Range, return_range: Range) -> Error {
    return Error::new(
        "expected value in return statement (return type is not void)",
        vec![
            (target_range, "target type is here".to_string()),
            (return_range, "return statement is here".to_string()),
        ],
    );
}

pub fn return_type_convert(
    target_type: &TCType,
    target_range: Range,
    value_type: Option<&TCType>,
    value_range: Range,
) -> Result<(), Error> {
    let value_type = match value_type {
        Some(t) => t,
        None => {
            if target_type.kind == TCTypeKind::Void && target_type.pointer_count == 0 {
                return Ok(());
            } else {
                return Err(Error::expected_return_value(target_range, value_range));
            }
        }
    };

    if target_type.kind == TCTypeKind::Void && target_type.pointer_count == 0 {
        return Err(Error::unexpected_return_value(value_range));
    }

    return Error::assignment_convert(target_type, target_range, value_type, value_range);
}

pub fn assignment_convert(
    target_type: &TCType,
    target_range: Range,
    value_type: &TCType,
    value_range: Range,
) -> Result<(), Error> {
    match target_type.kind {
        TCTypeKind::Void => {
            if target_type.pointer_count == 0 {
                return Err(Error::variable_is_void(target_range));
            }

            if target_type == value_type {
                return Ok(());
            }

            if target_type.pointer_count == 1 && value_type.pointer_count > 0 {
                return Ok(());
            }

            return Err(Error::assignment_convert_incompatible(
                target_range,
                value_range,
            ));
        }
        TCTypeKind::U64 | TCTypeKind::I32 | TCTypeKind::Char => {
            if target_type.pointer_count != value_type.pointer_count {
                return Err(Error::assignment_convert_incompatible(
                    target_range,
                    value_range,
                ));
            }

            if target_type.pointer_count == 0 {
                match value_type.kind {
                    TCTypeKind::I32 | TCTypeKind::Char => return Ok(()),
                    _ => {
                        return Err(Error::assignment_convert_incompatible(
                            target_range,
                            value_range,
                        ))
                    }
                }
            } else if target_type.kind == value_type.kind {
                return Ok(());
            } else {
                return Err(Error::assignment_convert_incompatible(
                    target_range,
                    value_range,
                ));
            }
        }
        TCTypeKind::Struct { .. } => {
            if target_type == value_type {
                return Ok(());
            }

            return Err(Error::assignment_convert_incompatible(
                target_range,
                value_range,
            ));
        }
    }
}
