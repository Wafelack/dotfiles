match $expr {
    Ok(v) => v,
    Err(e) => panic!("{}", e),
}
