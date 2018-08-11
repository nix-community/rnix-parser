#![feature(rust_2018_preview)]

macro_rules! m {
    () => {}
}

mod m {
    use crate::nonexistent_module::m;

    fn f() {
        m!();
    }
}
