# supercons

A Common Lisp port of Meta's [`superconsole`](https://github.com/facebookincubator/superconsole)
Rust crate: a terminal renderer that maintains a re-rendered canvas at the
bottom of the screen while letting log lines scroll above it.

The port aims to track the upstream library closely — public names, component
semantics, and module layout all mirror the Rust originals — while presenting
an idiomatic Common Lisp API.

## Status

Early port. Functional, with a test suite (`rove`) and the upstream
examples translated under `examples/`.

Terminal I/O currently uses SBCL-specific facilities (`sb-ext:posix-getenv`,
`sb-sys:fd-stream-fd`); only SBCL is supported at this time.

## Installation

Clone into a directory ASDF can find (e.g. `~/common-lisp/` or `~/quicklisp/local-projects/`):

```sh
git clone <repo-url> ~/common-lisp/supercons
```

Then from a Lisp REPL:

```lisp
(asdf:load-system :supercons)
```

Runtime dependency: [`bordeaux-threads`](https://github.com/sionescu/bordeaux-threads).
Tests additionally require [`rove`](https://github.com/fukamachi/rove).

## Quick start

```lisp
(defpackage #:demo (:use #:cl) (:local-nicknames (#:sc #:supercons)))
(in-package #:demo)

(defclass hello-world (sc:component) ())

(defmethod sc:draw-unchecked ((c hello-world) dimensions mode)
  (declare (ignore dimensions mode))
  (sc:make-lines (list (sc:line-from-strings '("Hello world!")))))

(let* ((console   (or (sc:make-superconsole)
                      (sc:make-superconsole-forced (sc:make-dimensions 80 24))))
       (component (sc:make-bordered (make-instance 'hello-world))))
  (sc:superconsole-render   console component)
  (sc:superconsole-finalize console component))
```

`sc:make-superconsole` returns `NIL` when stderr is not a compatible TTY;
`sc:make-superconsole-forced` always returns a console, using the supplied
fallback dimensions.

## Concepts

- **`component`** — base class. Subclass it and specialize
  `sc:draw-unchecked (component dimensions mode)`, returning a `lines`
  object. `mode` is `:normal` for live frames and `:final` for the last
  frame drawn by `superconsole-finalize`.
- **`span` / `line` / `lines`** — styled text primitives. Spans carry text,
  a `content-style`, and optional hyperlink; lines are sequences of spans;
  `lines` is a sequence of lines.
- **Canvas vs. emit** — `superconsole-render` redraws the live canvas in
  place; `superconsole-emit` queues lines that scroll permanently above it.

### Built-in components

| Component       | Constructor              | Purpose                                  |
|-----------------|--------------------------|------------------------------------------|
| `blank`         | `make-blank`             | Draws nothing                            |
| `echo`          | `make-echo`              | Emits a fixed `lines` value              |
| `bounded`       | `make-bounded`           | Constrains a child to a maximum size     |
| `aligned`       | `make-aligned`           | Positions a child within the box         |
| `padded`        | `make-padded`            | Pads a child on each side                |
| `bordered`      | `make-bordered`          | Draws a border around a child            |
| `split`         | `make-split`             | Splits space among children              |
| `spinner`       | `make-spinner`           | Animated spinner with a message          |

`draw-vertical` and `draw-horizontal` are stateful builders (not components)
for composing layouts that consume the remaining height or width as they go.

### Styling

```lisp
(sc:make-span-styled (sc:bold (sc:red "error")))
(sc:make-span-styled (sc:on-black (sc:green "info")))
```

Helpers like `red`, `on-blue`, `bold`, `italic`, `underlined` etc. wrap a
string (or styled value) into a `styled-content` carrying a `content-style`.

## Running the examples

Each example mirrors its upstream Rust counterpart and runs under SBCL:

```sh
sbcl --script examples/readme.lisp
sbcl --script examples/hello-world.lisp
sbcl --script examples/stylized.lisp
sbcl --script examples/cargo.lisp
sbcl --script examples/finalization.lisp
```

When stderr is not a TTY (e.g. piped), the examples fall back to a forced
80×24 console so the output is still observable.

## Tests

```lisp
(asdf:test-system :supercons)
```

or, from the shell, `make test`.

The test suite is written with `rove`: each check lives in a `deftest` in the
`supercons/tests` package. It exercises the content primitives, dimensions,
every component, output backends, and the rendering engine.

## Relation to upstream

File and symbol names follow the Rust source where reasonable; each Lisp
source file carries a header noting which Rust module it ports. Behavioral
differences from upstream are noted in the relevant file headers.

## License

Dual-licensed under MIT OR Apache-2.0, matching upstream `superconsole`. See
[`LICENSE-MIT`](LICENSE-MIT) and [`LICENSE-APACHE`](LICENSE-APACHE); upstream
copyright (Meta Platforms, Inc. and affiliates) is preserved in both.
