// Copyright 2018 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate rustc_version;

use rustc_version::{version_meta, Version};

fn main() {
    // `i128`/`u128` support was stabilized in Rust 1.26, so automatically
    // enable it if we detect that the compiler is recent enough (enable it if
    // we fail to detect the compiler version as well, assuming that future
    // Rust versions might break compatibility with the `rustc_version`
    // crate).
    if version_meta()
        .map(|meta| meta.semver >= Version::new(1, 26, 0))
        .unwrap_or(true)
    {
        println!("cargo:rustc-cfg=stable128");
    }
}
