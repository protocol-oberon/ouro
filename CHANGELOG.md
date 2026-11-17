# Changelog

All notable changes to Ouro are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and Ouro's
versions follow the [Haskell Package Versioning Policy](https://pvp.haskell.org/): while the
version is 0.x, a new minor version may break programs, and each change that does is marked
**Breaking**. Each entry links the commit that made it.

## [Unreleased]

### Added
- `(array a b)` builds an Array, as `(list a b)` did: the value is an Array everywhere else,
  in `#array`, in messages and in JSON (gap 55). `std` builds its arrays with it. <!-- zkvmtnrm -->

### Changed
- A graph compiles in about a fifth less time, allocation and memory: a hoard of 2000 coins
  takes 0.59 s where it took 0.76 s, measured together, and its peak memory is 100 MB where
  it was 125 MB. The JSON writer no longer holds its whole output as work left to do, and
  writes a string a run of characters at a time. <!-- tswvxqso --> <!-- kzlzuvxt -->
  <!-- uruvosts -->

### Deprecated
- `(list a b)` still builds an Array, with a warning that names `array`, `'list' is now called
  'array'` (`WRN0001`). It will be an error in 0.7 (gap 55). <!-- zkvmtnrm -->

### Fixed
- Set operations (`union`, `intersect`, `setminus`, `subset`, `distinct`, `unions`,
  `intersections`, `image` and `adjoin`) take time near linear in their arrays' length, not
  near its square: the union of 2000 records with themselves takes 0.52 s, where it took
  4.4 s (gap 56). <!-- tmxpswqz -->
- `ouro validate` checks a TimeSpan's dates: `begin_of_the_begin`, `end_of_the_begin`,
  `begin_of_the_end` and `end_of_the_end` must each be a date, where a value that was not one
  was written back as a string, and the middle two were never read as dates (gap 54).
  <!-- sxvuovpn -->

## [0.6.1.0] - 2026-11-17

### Added
- A `define/procedure` of no parameters warns (`WRN0006`) and suggests `define/constant`: a
  function of no parameters is a constant, worked out once per graph. ([60c1d69])
- `aat/language/iso` and `aat/currency/iso`: each language and currency with its ISO code as
  its notation, ISO 639 and ISO 4217, as Linked Art's examples mostly write them.
  `(import aat/language/iso)` gives `iso/english`, written `en`. Both are qualified `iso`, so
  a module using both imports one under another name: `(import aat/currency/iso as
  currency-iso)`. ([34eb2df], [170007b])
- Ten `aat` modules of the terms Linked Art's examples use: `activity-type`, `gender`,
  `part-type`, `price-type`, `provenance-type`, `right-type`, `role-type`, `style`,
  `technique` and `visual-type`. ([e1637aa], [170007b], [8681536], [f8e9c5b], [ca4ae1d],
  [80a3525], [28b1562])
- The terms Linked Art's examples use beyond its recommended list, in the modules that already
  existed: object types such as `exhibition-catalog` and `page`, and terms of the dimension,
  group, identifier, name, place, set and statement types, units, currencies and materials.
  Each says so in its documentation: "From Linked Art's examples, not its recommended list."
  ([e1637aa])
- `base/core` is exported: the constructor of the `id`, `type` and `_label` that every node
  with an id begins with. ([d737e5c])
- `std/object` has the units `day-unit`, `pixels` and `kilobytes`. ([e1637aa])

### Fixed
- `aat`'s material and unit labels match Linked Art's recommended list, which `aat` is
  generated from: they were capitalised, and are written as the list and Linked Art's examples
  write them, lower-case. `material/oil-paint` is labelled `"oil"`, where it was `"Oil"`, and
  `unit/centimeters` `"centimeters"`. A graph that uses them writes the corrected labels.
  ([d70655d])
- `ouro validate` without `-o` writes the JSON to stdout, and nothing else, so it can be piped;
  it no longer writes over its input (gap 13). ([6f09853])
- A `[` or `]` is an error that says an array is written in parentheses, `(1 2 3)`. The bracket
  syntax parsed, but meant nothing (gap 13). ([6f09853])
- A graph of many records compiles in time near linear in their number, not near its square:
  a hoard of 2000 coins takes 0.76 s, where it took 7.7 s (gap 52). ([194bf69])
- `insert` and `replace-at` reject a fractional index, as `get` does, where they rounded it
  down without a word: `(insert (1 2) 1.5 "x")` is an error (gap 39). ([69ced62])
- A form a file's top level cannot hold is named as it is written, `'defun' cannot begin a
  form at the top level of a file`, with what a top level may hold, where the message named
  Ouro's own Haskell, `Invalid top-level keyword: Symbol "defun"` (gap 46). ([037edac])
- A `#uri` is written as it was written: `"https://a.org/"` keeps its slash, and
  `"https://a.org/x//y"` its empty segment, in the output and in `ouro validate`'s
  (gaps 17, 22). ([0e55fc3])
- A `#date`'s year has at least four digits, as ISO 8601 and `xsd:dateTime` require: a year
  before 1000 is written with its leading zeros, `0079-08-24T12:00:00Z`, and a shorter year,
  `"81-01-01T00:00:00Z"`, is not a date (gaps 24, 53). ([0e55fc3])
- A graph built must have a name that is a file name on every system (`BLD0019`), and two graphs
  of a module cannot be named apart only by case (`BLD0020`), since either would be lost or
  written over on Windows or macOS; `ex:g` was written to a hidden stream (gap 34). ([773a992])

## [0.6.0.0] - 2026-11-15

### Changed
- **Breaking:** `define/procedure`, `define/property` and `define/constant` replace `defn`,
  `defn/property` and `defn/const`. A `/` always means a variant of what comes before it. The
  old words, and a bare `define`, are errors that name their replacement. ([03d7e99])
- **Breaking:** every form takes its subject first, so `->` threads through all of them:
  `(map xs f)`, `(filter xs f)`, `(forall xs f)`, `(exists xs f)`, `(omits xs f)`,
  `(insert coll i x)`, `(replace-at coll i x)` and `(get-or record default key ...)`. ([8cd535e])
- **Breaking:** `satisfies` asks its question value first, `(satisfies x #p)`, so it never looks
  like the assertion `#p x`. As the head of a `case` branch, `(satisfies #p)` is an ordinary
  guard, with the target put first. ([8cd535e])
- **Breaking:** every built-in takes a fixed number of arguments, and many values are an Array.
  Arithmetic and comparisons take two operands; `(+ 1 2 3)` and `(< a b c)` are arity errors
  whose hint is the fold or `all`. `and`, `or`, `union`, `intersect` and `setminus` take two. ([bea690a])
- **Breaking:** `min` and `max` take one Array of Numbers. ([aaeb9bd])
- A call of a built-in with the wrong number of arguments is reported where it is written,
  even in a function nothing calls, as a call of a defined function already was. ([db079b8])
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
- `fold`, a left fold: `(fold (1 2 3) 0 +)` is `6`. ([bea690a])
- `all` and `any`, the n-ary `and` and `or` over an Array of Booleans, also written `⋀` and
  `⋁`. Over an Array written in place they stop at the element that decides. ([bea690a])
- String built-ins: `to-string`, `split`, `join`, `upper-case`, `lower-case`, `trim`,
  `starts-with`, `ends-with`, `slice`, `pad-start` and `replace-all`, each taking its String
  first. ([7d06946])
- Ranges, `(1..5)` and `(2 4 .. 10)`, as in Haskell, and `zip`, which pairs two Arrays of one
  length. ([bc6a009])
- The README shows how to build a String of many pieces with `->`. ([834261a])

### Fixed
- A whole number is written whole, and any other in plain decimal, as JavaScript writes
  numbers, not in scientific notation (gap 48). ([7d06946])
- A `define/constant` or `define/property` written in an expression is reported as a local
  definition, as a `define/procedure` is (gap 49). ([abd6f0b])
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

[Unreleased]: https://github.com/protocol-oberon/ouro/compare/v0.6.1.0...HEAD
[0.6.1.0]: https://github.com/protocol-oberon/ouro/compare/v0.6.0.0...v0.6.1.0
[0.6.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.5.0.0...v0.6.0.0
[0.5.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.0.0.1...v0.5.0.0
[0.0.0.1]: https://github.com/protocol-oberon/ouro/releases/tag/v0.0.0.1

[60c1d69]: https://github.com/protocol-oberon/ouro/commit/60c1d69
[34eb2df]: https://github.com/protocol-oberon/ouro/commit/34eb2df
[170007b]: https://github.com/protocol-oberon/ouro/commit/170007b
[e1637aa]: https://github.com/protocol-oberon/ouro/commit/e1637aa
[8681536]: https://github.com/protocol-oberon/ouro/commit/8681536
[f8e9c5b]: https://github.com/protocol-oberon/ouro/commit/f8e9c5b
[ca4ae1d]: https://github.com/protocol-oberon/ouro/commit/ca4ae1d
[80a3525]: https://github.com/protocol-oberon/ouro/commit/80a3525
[28b1562]: https://github.com/protocol-oberon/ouro/commit/28b1562
[d737e5c]: https://github.com/protocol-oberon/ouro/commit/d737e5c
[d70655d]: https://github.com/protocol-oberon/ouro/commit/d70655d
[6f09853]: https://github.com/protocol-oberon/ouro/commit/6f09853
[194bf69]: https://github.com/protocol-oberon/ouro/commit/194bf69
[69ced62]: https://github.com/protocol-oberon/ouro/commit/69ced62
[037edac]: https://github.com/protocol-oberon/ouro/commit/037edac
[0e55fc3]: https://github.com/protocol-oberon/ouro/commit/0e55fc3
[773a992]: https://github.com/protocol-oberon/ouro/commit/773a992

[03d7e99]: https://github.com/protocol-oberon/ouro/commit/03d7e99
[8cd535e]: https://github.com/protocol-oberon/ouro/commit/8cd535e
[bea690a]: https://github.com/protocol-oberon/ouro/commit/bea690a
[aaeb9bd]: https://github.com/protocol-oberon/ouro/commit/aaeb9bd
[db079b8]: https://github.com/protocol-oberon/ouro/commit/db079b8
[7d06946]: https://github.com/protocol-oberon/ouro/commit/7d06946
[bc6a009]: https://github.com/protocol-oberon/ouro/commit/bc6a009
[834261a]: https://github.com/protocol-oberon/ouro/commit/834261a
[abd6f0b]: https://github.com/protocol-oberon/ouro/commit/abd6f0b
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
