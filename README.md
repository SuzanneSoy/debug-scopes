[![Build Status,](https://img.shields.io/travis/jsmaniac/debug-scopes/main.svg)](https://travis-ci.org/jsmaniac/debug-scopes)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/debug-scopes/main.svg)](https://codecov.io/gh/jsmaniac/debug-scopes)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/debug-scopes)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/debug-scopes/)
[![Maintained as of 2018,](https://img.shields.io/maintenance/yes/2018.svg)](https://github.com/jsmaniac/debug-scopes/issues)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

debug-scopes
============

Some utilities which help debugging scope-related issues with Racket
unhygienic macros (hygienic macros in Racket will likely not need these
utilities).

First, run:

    raco pkg install debug-scopes

Then, require one or several of the following modules:

    (require debug-scopes)

* `(+scopes stx)` returns a string with a succinct representation of which
  scopes are present on each identifier.
* `(print-full-scopes)` prints the long version of the scope information for
  the scopes used in preceeding calls to `+scopes`.

```racket
(require debug-scopes/named-scopes)
```

* (make-named-scope string-or-symbol) creates a scope similarly to
  `make-syntax-introducer`, but annotates it with a name. This is a hack which
  creates a module with that name, and extracts the module scope (since these
  scopes have names attached to them in Racket). As a result, with the current
  implementation, this returns a module scope instead of a macro scope like
  `make-syntax-introducer`.

```racket
(require debug-scopes/named-scopes/override)
```

* Overrides `define-syntax` and `syntax-local-introduce`. The overridden
  `syntax-local-introduce` works with the overridden `define-syntax` to flip a
  named scope instead of the usual macro scope. The use-site scope which may
  be flipped by `syntax-local-introduce` is left unchanged.
