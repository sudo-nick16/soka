## Soka [WIP]

Experimental compiler for a custom grammar written in Rust.
Compiles the source code to x86-64 assembly and then uses GNU assembler to generate static executable.

### Supported Datatypes

- [x] bool
- [x] unsigned int (8 bytes)
- [x] signed int
- [x] array
- [x] char
- [ ] float

### Usage

```
cargo run <filename>
```

### Examples

- [Tcp Server](https://github.com/sudo-nick16/soka/blob/master/examples/server.n)
- [Conway's game of life](https://github.com/sudo-nick16/soka/blob/master/examples/gol.n)

> Written for educational purpose.
