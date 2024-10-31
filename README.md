marc-record
===========

A Rust library for parsing MARC records, specifically using the MARC21 format, with either
UTF-8 or MARC-8 encoding. This library has been tested on a bunch of records from a single provider and various samples found in the wild.
Since MARC is an open standard with many variations, we may not support all the files. In particular, we do not
support MARCXML at the moment.

Getting started
===============

Add the crate to your rust library:

```sh
cargo install marc-record
```

Load, parse and inspect a record:
```rust
let mut contents = Vec::new();
File::open(path_to_my_file)?.read_to_end(&mut contents)?;
let records = marc_record::parse_records(&contents)?;
println!("File contains {} records", records.len());
```

License
=======

`marc-record` is distributed under the terms of the MIT license.