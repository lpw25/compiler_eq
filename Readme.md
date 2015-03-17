## Compiler_eq

A tool to test the differences between two versions of OCaml.

### Install

Compiler_eq should be installed using the version of OCaml on which the
two compilers being tested are based:

```
opam switch 4.02.1
opam pin add compiler_eq git://github.com/lpw25/compiler_eq
```

### Initialize the compilers to be tested

```
compiler_eq init 4.02+trunk 4.02+doc
```

### Install some OPAM packages

```
compiler_eq install lwt core
```

### Compare the compilers

```
compiler_eq check
```

### Inspect differences on specific files

```
compiler_eq diff lwt _buils/foo/bar.cmt
```
