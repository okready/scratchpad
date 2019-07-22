// Copyright 2018-2019 Theodore Cipicchio
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate rustc_version;

use rustc_version::{version_meta, Version};

fn main() {
    // Automatically enable various features if we detect the compiler is
    // recent enough. If we're unable to detect the Rust compiler version,
    // enable all such features, assuming that future Rust versions might
    // break compatibility with the `rustc_version` crate.
    let enable_stable_128;
    let enable_stable_maybe_uninit;
    if let Ok(meta) = version_meta() {
        let semver = meta.semver;

        // `i128`/`u128` support was stabilized in Rust 1.26.
        enable_stable_128 = semver >= Version::new(1, 26, 0);

        // `MaybeUninit` was stabilized in Rust 1.36.
        enable_stable_maybe_uninit = semver >= Version::new(1, 36, 0);
    } else {
        enable_stable_128 = false;
        enable_stable_maybe_uninit = false;
    }

    if enable_stable_128 {
        // TODO: Change this flag to `stable_128` for consistency. I'm not
        //       sure if it's safe to alter config flag strings without
        //       incrementing the major version number (can external crates
        //       check our own config flags at all?).
        println!("cargo:rustc-cfg=stable128");
    }

    if enable_stable_maybe_uninit {
        println!("cargo:rustc-cfg=stable_maybe_uninit");
    }
}
