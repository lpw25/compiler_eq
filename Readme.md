## Compiler_eq

A tool to test the differences between two versions of OCaml.

### Install

Compiler_eq can only compare compilers which use the same AST format,
and compiler_eq must be install using a compiler which uses this AST
format.

```
$opam switch 4.02.1
$opam pin add compiler_eq git://github.com/lpw25/compiler_eq
```

### Initialize the compilers to be tested

Initialise the two compilers being tested using the `init` command:

```
$compiler_eq init 4.02.2+trunk 4.02.2+new-doc
```

### Install some OPAM packages

Install packges for use in the comparison using the `install` command:

```
$compiler_eq install lwt core
```

### Compare the compilers

Compare the compilers on the installed packages using the `check` command:

```
$compiler_eq check
```

which will produce a table for each installed package:

```
┌───────────────────────────────────────────────────┐
│ lwt                                               │
├───────────────────────────────────────────────┬───┤
│ ./setup.cmt                                   │ ✓ │
│ ./_build/src/unix/lwt_unix_jobs_generated.cmt │ ✓ │
│ ./_build/src/unix/lwt_unix.cmti               │ ✓ │
│ ./_build/src/unix/lwt_unix.cmt                │ ✓ │
│ ./_build/src/unix/lwt_timeout.cmti            │ ✓ │
│ ./_build/src/unix/lwt_timeout.cmt             │ ✓ │
│ ./_build/src/unix/lwt_throttle.cmti           │ ✓ │
│ ./_build/src/unix/lwt_throttle.cmt            │ ✓ │
│ ./_build/src/unix/lwt_sys.cmti                │ ✓ │
│ ./_build/src/unix/lwt_sys.cmt                 │ ✓ │
│ ./_build/src/unix/lwt_process.cmti            │ ✓ │
│ ./_build/src/unix/lwt_process.cmt             │ ✓ │
│ ./_build/src/unix/lwt_main.cmti               │ ✓ │
│ ./_build/src/unix/lwt_main.cmt                │ ✓ │
│ ./_build/src/unix/lwt_log.cmti                │ ✓ │
│ ./_build/src/unix/lwt_log.cmt                 │ ✓ │
│ ./_build/src/unix/lwt_io.cmti                 │ ✓ │
│ ./_build/src/unix/lwt_io.cmt                  │ ✓ │
│ ./_build/src/unix/lwt_gc.cmti                 │ ✓ │
│ ./_build/src/unix/lwt_gc.cmt                  │ ✓ │
│ ./_build/src/unix/lwt_engine.cmti             │ ✓ │
│ ./_build/src/unix/lwt_engine.cmt              │ ✓ │
│ ./_build/src/unix/lwt_daemon.cmti             │ ✓ │
│ ./_build/src/unix/lwt_daemon.cmt              │ ✓ │
│ ./_build/src/unix/lwt_config.cmt              │ ✓ │
│ ./_build/src/unix/lwt_chan.cmti               │ ✓ │
│ ./_build/src/unix/lwt_chan.cmt                │ ✓ │
│ ./_build/src/unix/lwt_bytes.cmti              │ ✓ │
│ ./_build/src/unix/lwt_bytes.cmt               │ ✓ │
│ ./_build/src/simple_top/lwt_simple_top.cmt    │ ✓ │
│ ./_build/src/preemptive/lwt_preemptive.cmti   │ ✓ │
│ ./_build/src/preemptive/lwt_preemptive.cmt    │ ✓ │
│ ./_build/src/logger/lwt_log_rules.cmti        │ ✓ │
│ ./_build/src/logger/lwt_log_rules.cmt         │ ✓ │
│ ./_build/src/logger/lwt_log_core.cmti         │ ✓ │
│ ./_build/src/logger/lwt_log_core.cmt          │ ✓ │
│ ./_build/src/core/lwt_switch.cmti             │ ✓ │
│ ./_build/src/core/lwt_switch.cmt              │ ✓ │
│ ./_build/src/core/lwt_stream.cmti             │ ✓ │
│ ./_build/src/core/lwt_stream.cmt              │ ✓ │
│ ./_build/src/core/lwt_sequence.cmti           │ ✓ │
│ ./_build/src/core/lwt_sequence.cmt            │ ✓ │
│ ./_build/src/core/lwt_pqueue.cmti             │ ✓ │
│ ./_build/src/core/lwt_pqueue.cmt              │ ✓ │
│ ./_build/src/core/lwt_pool.cmti               │ ✓ │
│ ./_build/src/core/lwt_pool.cmt                │ ✓ │
│ ./_build/src/core/lwt_mvar.cmti               │ ✓ │
│ ./_build/src/core/lwt_mvar.cmt                │ ✓ │
│ ./_build/src/core/lwt_mutex.cmti              │ ✓ │
│ ./_build/src/core/lwt_mutex.cmt               │ ✓ │
│ ./_build/src/core/lwt_list.cmti               │ ✓ │
│ ./_build/src/core/lwt_list.cmt                │ ✓ │
│ ./_build/src/core/lwt_condition.cmti          │ ✓ │
│ ./_build/src/core/lwt_condition.cmt           │ ✓ │
│ ./_build/src/core/lwt.cmti                    │ ✓ │
│ ./_build/src/core/lwt.cmt                     │ ✓ │
│ ./_build/ppx/ppx_lwt_ex.cmt                   │ ✗ │
│ ./_build/ppx/ppx_lwt.cmti                     │ ✓ │
│ ./_build/ppx/ppx_lwt.cmt                      │ ✓ │
│ ./_build/myocamlbuild.cmt                     │ ✓ │
└───────────────────────────────────────────────┴───┘
```

The comparison can be restricted to specific packages:

```
compiler_eq check lwt
```

### Inspect differences on specific files

The differences between compilers for a specific file in a specific
package can be inspected using the `diff` command:

```
compiler_eq diff lwt ./_build/ppx/ppx_lwt_ex.cmt
```

### Ignoring attributes and locations

Attributes and locations can be ignored for `check` and `diff` by using
the `-ignore-attributes` and `-ignore-locations` options.
