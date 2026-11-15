# Changelog

All notable changes to Ouro are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and Ouro's
versions follow the [Haskell Package Versioning Policy](https://pvp.haskell.org/): while the
version is 0.x, a new minor version may break programs, and each change that does is marked
**Breaking**. Each entry links the commit that made it.

## [Unreleased]

## [0.6.0.0] - 2026-11-15

### Changed
- **Breaking:** `define/procedure`, `define/property` and `define/constant` replace `defn`,
  `defn/property` and `defn/const`. A `/` always means a variant of what comes before it. The
  old words, and a bare `define`, are errors that name their replacement. <!-- okwrrvyr -->
- **Breaking:** every form takes its subject first, so `->` threads through all of them:
  `(map xs f)`, `(filter xs f)`, `(forall xs f)`, `(exists xs f)`, `(omits xs f)`,
  `(insert coll i x)`, `(replace-at coll i x)` and `(get-or record default key ...)`. <!-- usyozyln -->
- **Breaking:** `satisfies` asks its question value first, `(satisfies x #p)`, so it never looks
  like the assertion `#p x`. As the head of a `case` branch, `(satisfies #p)` is an ordinary
  guard, with the target put first. <!-- usyozyln -->
- **Breaking:** every built-in takes a fixed number of arguments, and many values are an Array.
  Arithmetic and comparisons take two operands; `(+ 1 2 3)` and `(< a b c)` are arity errors
  whose hint is the fold or `all`. `and`, `or`, `union`, `intersect` and `setminus` take two. <!-- knpqkwuw -->
- **Breaking:** `min` and `max` take one Array of Numbers. <!-- ywmtrptr -->
- A call of a built-in with the wrong number of arguments is reported where it is written,
  even in a function nothing calls, as a call of a defined function already was. <!-- lrpqytku -->
- **Breaking:** Ouro has no null. The word is an error where it is written, and a context
  cannot clear another or set anything to null. ([5a22957])
- A function of no parameters is evaluated once per graph. ([0b7120c])
- A property tag is any name, qualified or not; until properties are checked, one is an
  unknown property. ([e53a1ad])
- Recursion that goes down into a value is allowed, checked by size-change termination; the
  evaluator runs a call the analysis accepted. ([c67f5ea], [e36ea9e])
- The static analysis checks names, the arity of calls of known functions, and recursion
  wherever they are written, called or not. ([bf1c90d], [7207d0b], [0b3d932])
- Special forms are bindings, as in Scheme: a binding of a form's name shadows the form, and
  Canon's own forms use heads no source can write. `define` and `defun` are ordinary names.
  ([3d956b9], [2c00794])

### Added
- `fold`, a left fold: `(fold (1 2 3) 0 +)` is `6`. <!-- knpqkwuw -->
- `all` and `any`, the n-ary `and` and `or` over an Array of Booleans, also written `⋀` and
  `⋁`. Over an Array written in place they stop at the element that decides. <!-- knpqkwuw -->
- String built-ins: `to-string`, `split`, `join`, `upper-case`, `lower-case`, `trim`,
  `starts-with`, `ends-with`, `slice`, `pad-start` and `replace-all`, each taking its String
  first. <!-- szvtrnxu -->
- Ranges, `(1..5)` and `(2 4 .. 10)`, as in Haskell, and `zip`, which pairs two Arrays of one
  length. <!-- yvyknlsp -->
- The README shows how to build a String of many pieces with `->`. <!-- tplxpxky -->

### Fixed
- A whole number is written whole, and any other in plain decimal, as JavaScript writes
  numbers, not in scientific notation (gap 48). <!-- szvtrnxu -->
- A `define/constant` or `define/property` written in an expression is reported as a local
  definition, as a `define/procedure` is (gap 49). <!-- mrxmsmxu -->
- A name that only begins with `true`, `false` or `null` is read as a name (gap 47). ([176a723])
- A failed module is reported once, however many modules import it (gap 40). ([6adb240])
- The names of removed forms are free as keys and bindings (gap 42). ([b09d13c])

## [0.5.0.0] - 2026-11-07

### Added
- Functions: `defn`, `fn` (also `λ`) and `let`, with typed parameters and return types, `doc`
  strings, destructuring parameters, and placeholders, `(+ ? 1)`.
  ([fe5b5b7], [5b4e959], [6c28a7a], [4e794d6], [cfba031])
- `map`, `with`, `->` and `adjoin`, the dual of `with`. ([e950f0c], [fdcbd53], [963372f])
- Quantifiers and filters: `exists`, `forall`, `omits`, `filter`, `contains`, `distinct`,
  `has`, `get-or` and `not`. ([0dabd87], [f3a5911])
- Sets: `union`, `intersect`, `setminus`, `subset`, `unions` and `intersections`, and the
  symbols of logic and sets, with `=` and `/=`. ([31f4612])
- `case` conditions: `when`, comparison guards and `satisfies` (`⊨`). ([53ffd3c], [a9e990e])
- Paths: `image` reads a path as a set, and indexes are written in a path. ([4fa290e], [35c85c2])
- `len`. ([03f3479])
- Imports and exports, with modules read from disk and each error shown in its own file.
  ([b3a0631], [9590273])
- `std`, Linked Art's patterns as bundled modules: `std/base`, `std/vocab`, `std/time`,
  `std/object`, `std/actor`, `std/provenance`, `std/exhibition` and `std/digital`.
  ([3e5cabc], [dd79750], [331a6b3])
- `aat`, Linked Art's recommended terms, generated from its list. ([a92e8e5])
- `ouro build`: a folder is a project, with an optional `package.ouro`, vendored libraries in
  `vendor/`, and `-g` or `:graphs` to choose the graphs. ([5c06dd6], [54a6a6e], [dbfbe0d],
  [f12a5f1])
- Warnings for a function that replaces a built-in, and for a name not in kebab-case.
  ([fc10b69], [6682173])
- Every error code's message is pinned by a golden file. ([104ffff])

### Changed
- **Breaking:** scoping is lexical, as in Scheme, with memoised promises; quoting is removed.
  ([f72bc72], [712b962])
- **Breaking:** `defun` is `defn`, and functions are called without a `!`. ([fe5b5b7], [fe27307])
- **Breaking:** a local binding is written `$name` in a `let`. ([63279df])
- **Breaking:** an attribute pair is written `:key value`, and `attr` and `nth` are removed: a
  key is never computed, and an index is written in `get`'s path. ([7511251], [0f55e2b])
- **Breaking:** the connectives and quantifiers are `and`, `or`, `exists` and `forall`.
  ([f3a5911])
- **Breaking:** built-in properties are written in full: `#string`, `#number`, `#boolean`,
  `#record`, `#array`, `#function`, `#record-empty` and `#array-empty`. ([983fcfd])
- **Breaking:** `ouro compile` is removed; `ouro build` builds a project. ([f12a5f1])
- **Breaking:** `inlay` overrides attributes. ([24f5ea4])
- A function of no parameters, named, is its value. ([5941eec])
- A key always has a value, and a compact IRI is one key. ([4e263a2])
- The compiler's errors are collected with `ValidateT`. ([d85706c])

### Fixed
- Source and messages are read and written as UTF-8 on Windows. ([173e95e])
- Strings are escaped in the output. ([94cc827])
- Shifting by months clips to the end of a shorter month. ([2a3dc94])
- A variadic operation under a built-in property is desugared. ([8e9c445])
- A binding never sees itself. ([e8a90cb])
- `@context` is written as JSON-LD writes it. ([facb4df])

## [0.0.0.1] - 2026-05-27

### Added
- The first Ouro: a Lisp that compiles to JSON-LD, with primitives, typed URIs and dates,
  records and arrays, serialisation to JSON, and a command line. ([45edabc], [9e10f0e],
  [0fabbf6], [4ef6457], [4db16ac])

[Unreleased]: https://github.com/protocol-oberon/ouro/compare/v0.6.0.0...HEAD
[0.6.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.5.0.0...v0.6.0.0
[0.5.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.0.0.1...v0.5.0.0
[0.0.0.1]: https://github.com/protocol-oberon/ouro/releases/tag/v0.0.0.1

[5a22957]: https://github.com/protocol-oberon/ouro/commit/5a22957
[0b7120c]: https://github.com/protocol-oberon/ouro/commit/0b7120c
[e53a1ad]: https://github.com/protocol-oberon/ouro/commit/e53a1ad
[c67f5ea]: https://github.com/protocol-oberon/ouro/commit/c67f5ea
[e36ea9e]: https://github.com/protocol-oberon/ouro/commit/e36ea9e
[bf1c90d]: https://github.com/protocol-oberon/ouro/commit/bf1c90d
[7207d0b]: https://github.com/protocol-oberon/ouro/commit/7207d0b
[0b3d932]: https://github.com/protocol-oberon/ouro/commit/0b3d932
[3d956b9]: https://github.com/protocol-oberon/ouro/commit/3d956b9
[2c00794]: https://github.com/protocol-oberon/ouro/commit/2c00794
[176a723]: https://github.com/protocol-oberon/ouro/commit/176a723
[6adb240]: https://github.com/protocol-oberon/ouro/commit/6adb240
[b09d13c]: https://github.com/protocol-oberon/ouro/commit/b09d13c
[fe5b5b7]: https://github.com/protocol-oberon/ouro/commit/fe5b5b7
[5b4e959]: https://github.com/protocol-oberon/ouro/commit/5b4e959
[6c28a7a]: https://github.com/protocol-oberon/ouro/commit/6c28a7a
[4e794d6]: https://github.com/protocol-oberon/ouro/commit/4e794d6
[cfba031]: https://github.com/protocol-oberon/ouro/commit/cfba031
[e950f0c]: https://github.com/protocol-oberon/ouro/commit/e950f0c
[fdcbd53]: https://github.com/protocol-oberon/ouro/commit/fdcbd53
[963372f]: https://github.com/protocol-oberon/ouro/commit/963372f
[0dabd87]: https://github.com/protocol-oberon/ouro/commit/0dabd87
[f3a5911]: https://github.com/protocol-oberon/ouro/commit/f3a5911
[31f4612]: https://github.com/protocol-oberon/ouro/commit/31f4612
[53ffd3c]: https://github.com/protocol-oberon/ouro/commit/53ffd3c
[a9e990e]: https://github.com/protocol-oberon/ouro/commit/a9e990e
[4fa290e]: https://github.com/protocol-oberon/ouro/commit/4fa290e
[35c85c2]: https://github.com/protocol-oberon/ouro/commit/35c85c2
[03f3479]: https://github.com/protocol-oberon/ouro/commit/03f3479
[b3a0631]: https://github.com/protocol-oberon/ouro/commit/b3a0631
[9590273]: https://github.com/protocol-oberon/ouro/commit/9590273
[3e5cabc]: https://github.com/protocol-oberon/ouro/commit/3e5cabc
[dd79750]: https://github.com/protocol-oberon/ouro/commit/dd79750
[331a6b3]: https://github.com/protocol-oberon/ouro/commit/331a6b3
[a92e8e5]: https://github.com/protocol-oberon/ouro/commit/a92e8e5
[5c06dd6]: https://github.com/protocol-oberon/ouro/commit/5c06dd6
[54a6a6e]: https://github.com/protocol-oberon/ouro/commit/54a6a6e
[dbfbe0d]: https://github.com/protocol-oberon/ouro/commit/dbfbe0d
[f12a5f1]: https://github.com/protocol-oberon/ouro/commit/f12a5f1
[fc10b69]: https://github.com/protocol-oberon/ouro/commit/fc10b69
[6682173]: https://github.com/protocol-oberon/ouro/commit/6682173
[104ffff]: https://github.com/protocol-oberon/ouro/commit/104ffff
[f72bc72]: https://github.com/protocol-oberon/ouro/commit/f72bc72
[712b962]: https://github.com/protocol-oberon/ouro/commit/712b962
[fe27307]: https://github.com/protocol-oberon/ouro/commit/fe27307
[63279df]: https://github.com/protocol-oberon/ouro/commit/63279df
[7511251]: https://github.com/protocol-oberon/ouro/commit/7511251
[0f55e2b]: https://github.com/protocol-oberon/ouro/commit/0f55e2b
[983fcfd]: https://github.com/protocol-oberon/ouro/commit/983fcfd
[24f5ea4]: https://github.com/protocol-oberon/ouro/commit/24f5ea4
[5941eec]: https://github.com/protocol-oberon/ouro/commit/5941eec
[4e263a2]: https://github.com/protocol-oberon/ouro/commit/4e263a2
[d85706c]: https://github.com/protocol-oberon/ouro/commit/d85706c
[173e95e]: https://github.com/protocol-oberon/ouro/commit/173e95e
[94cc827]: https://github.com/protocol-oberon/ouro/commit/94cc827
[2a3dc94]: https://github.com/protocol-oberon/ouro/commit/2a3dc94
[8e9c445]: https://github.com/protocol-oberon/ouro/commit/8e9c445
[e8a90cb]: https://github.com/protocol-oberon/ouro/commit/e8a90cb
[facb4df]: https://github.com/protocol-oberon/ouro/commit/facb4df
[45edabc]: https://github.com/protocol-oberon/ouro/commit/45edabc
[9e10f0e]: https://github.com/protocol-oberon/ouro/commit/9e10f0e
[0fabbf6]: https://github.com/protocol-oberon/ouro/commit/0fabbf6
[4ef6457]: https://github.com/protocol-oberon/ouro/commit/4ef6457
[4db16ac]: https://github.com/protocol-oberon/ouro/commit/4db16ac
