# Secret Service

Secret Service Rust library.

Interfaces with the Linux Secret Service API through dbus.

This library is feature complete, has stabilized its API, and has removed extraneous dependencies, so I've made it 1.0.

### Documentation

[Get Docs!](https://docs.rs/secret-service/1.0.0/secret_service/)

### Basic Usage

Requires dbus library.

On ubuntu, this is libdbus-1-dev when building, and libdbus-1-3 when running.

In Cargo.toml:

```
[dependencies]
secret-service = "1.0.0"
```

If you have `cargo-extras` installed, can replace above step with the command at the prompt in your project directory:

```
$ cargo add secret-service
```

In source code (below example is for --bin, not --lib)

```rust
extern crate secret_service;
use secret_service::SecretService;
use secret_service::EncryptionType;
use std::error::Error;

fn main() -> Result<(), Box<Error>> {

    // initialize secret service (dbus connection and encryption session)
    let ss = SecretService::new(EncryptionType::Dh)?;

    // get default collection
    let collection = ss.get_default_collection()?;

    //create new item
    collection.create_item(
        "test_label", // label
        vec![("test", "test_value")], // properties
        b"test_secret", //secret
        false, // replace item with same attributes
        "text/plain" // secret content type
    )?;

    // search items by properties
    let search_items = ss.search_items(
        vec![("test", "test_value")]
    )?;

    let item = search_items.get(0)?;

    // retrieve secret from item
    let secret = item.get_secret()?;
    assert_eq!(secret, b"test_secret");

    // delete item (deletes the dbus object, not the struct instance)
    item.delete()?;
}
```

### Functionality

- SecretService: initialize dbus, create plain/encrypted session.
- Collections: create, delete, search.
- Items: create, delete, search, get/set secret.


### Changelog
_0.1.0_
- dependency on gmp is removed.
- rust-crypto replaced by RustCrypto.
- as a result of above, error on encrypting and decrypting blank input is fixed.

_0.4.0_
- gmp is now optional dependency.
- gmp upgraded to 0.3 to fix "private-in-public" warnings which will be hard errors soon.


## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
  
