# Changelog

All notable changes to Ouro are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and Ouro's
versions follow the [Haskell Package Versioning Policy](https://pvp.haskell.org/): while the
version is 0.x, a new minor version may break programs, and each change that does is marked
**Breaking**. Each entry links the commit that made it.

## [Unreleased]

### Changed
- The tree of built-in refinements a message draws opens only the branches that lead to what was
  required and what was given; any other branch is one row, with `…` after its name.
  <!-- rlwxpvku -->
- A function is named by its kind in messages and warnings: "a procedure", "a refinement" or
  "a built-in procedure", where it was "an unexecuted function" or "a Built-in function", and a
  Refinement Failed's `given:` agrees with the tree's mark. `WRN0003` and `WRN0004` say "the
  procedure 'len'". <!-- pnootmro -->
- A refinement whose body only asks a type, `<r ∈ (record :label string)>`, is checked against
  that type, and a failure keeps its trail through it, `└─ labelled asks: (record :label string)`,
  `└─ key label: string`, where it named the refinement alone. <!-- mnpywwom -->
- A failure's trail ends with what was found there, `└─ element 2: number, given string`.
  <!-- mnpywwom -->

### Fixed
- A function where data is needed, `(:f +)`, expects `data` and draws it in the tree, where the
  message listed eight built-in refinements by hand, from before `value`, `data`, `procedure`,
  `refinement` and `duration`, and called a function "not a value" (gap 60). <!-- rlwxpvku -->
- An unknown refinement's suggestion is drawn from every built-in refinement, so `duraton`
  suggests `duration`, where it chose from the first eight only. <!-- pnootmro -->

## [0.8.0.0] - 2026-11-25

### Added
- `value`, `data`, `procedure` and `refinement` are built-in refinements beside `function`, so
  every level of the type tree can be written: `value` every value, `data` what can be written
  to JSON-LD, `procedure` and `refinement` the two kinds of function, decided on the value,
  a built-in counting as a procedure. ([b6b8350], [d42fa2c])
- A value outside a built-in refinement is shown where it sits in the tree of built-in
  refinements, the refinement required and the value's own kind marked; left out where the two
  are kinds of data side by side, a string required and a number given. ([d42fa2c])
- `WRN0010`, a refinement declared over `value`, `(<x ⊢ value>)`, which says nothing about its
  subject: allowed, for a refinement that decides across kinds, but warned of, a narrower domain
  saying more. ([d42fa2c])
- `duration`, a built-in refinement of `string`: the strings in `xsd:duration` form that are
  magnitudes, `"P19D"`, `"PT3H"`. ([6520853])
- The four shifts of a date, `shift/forward`, `shift/backward`, `shift/forward-through` and
  `shift/backward-through`, the last two to the end, or the start, of the duration's largest
  unit; `duration/add`, which adds durations part by part; and `date/before` and `date/after`,
  which compare dates. ([221bc7f])

### Changed
- A duration is a string, `(days 19)` writing `"P19D"`, so it is data and may be written, where
  it was a value of its own with no JSON form. `(+ (days 1) (days 2))` joins them, `"P1DP2D"`:
  durations are added with `duration/add`. A duration is now in `string`, so
  `<(days 1) ∈ string>` is `true`. ([a79bc97])
- **Breaking**: a unit's amount is a whole number but for seconds, `(days 1.5)` being an error
  where it was floored to a day. ([a79bc97])
- **Breaking**: `thru` is read as a shift through, `(+ d (thru (days 1)))` as
  `(shift/forward-through d (days 1))`: a date not at midnight now goes on to the end of the
  day, where 23:59:59 was added to it, and `(- d (thru (days 1)))` goes back to the start of the
  day before, where it ended a second after midnight two days back. `(- d (thru (months 1)))`
  goes back to the start of the month before, where it went to that month's end, and a negative
  amount in a `thru`, `(thru (months -1))`, is refused. ([a79bc97])
- A value outside a signature's requirement or an assertion is `TYP0002`, Refinement Failed,
  which names what was required and what was given on lines of their own, the role the value
  was required in, `the 1st argument of describe`, who is blamed, the caller for an argument and
  the function for its result, and the trail down to the part that failed, `element 2`, then
  `key _label`. A built-in's own operand check stays `TYP0001`. ([73d5779])

- `∈` answers `false` for a value outside a defined refinement's domain,
  `<"a" ∈ titled>` where `titled` takes a record, which was an error. Such a value asserted,
  `<"a" ⊢ titled>`, is reported at the value, where it was at the refinement's name.
  ([44b6293])

### Deprecated
- `+` and `-` of a date and a duration, `(+ d (days 1))`; the two-argument `shift`, whose
  duration's sign said its direction; and `thru`: each still works, warned of with what
  replaces it (`WRN0001`), until they are errors. ([a79bc97])

### Fixed
- A value failing a refinement is named with its article, `an array`, where it was `a array`
  (gap 59). ([73d5779])
- A refinement that breaks inside its body, an assertion there failing, is that error under `∈`
  and inside a union, where it was read as the value outside the refinement (gap 50).
  ([44b6293])
- `(∈ p)` written anywhere but a case branch's head says so, where it was an unbound `%in`
  (gap 57). ([44b6293])

## [0.7.0.1] - 2026-11-23

### Fixed
- A diagnostic in no file, such as one about the argument `-t ouro-mode`, a vendored folder or
  an unreadable file, names where it is, `│-t ouro-mode│`, where it was framed as line 1 of an
  empty file with a caret under nothing (gap 58). ([ec69b48])

## [0.7.0.0] - 2026-11-23

Types are written as sets. A refinement is a set of values, built in or defined in Ouro, and is
written by its name: `⊢` asserts that a value is in one, and `∈` asks. The old spellings still
work, each with a warning that names what replaces it, until 0.8.0.0.

### Added
- `⊢` (also `assert` or `|-`) asserts that a value is in a refinement, passing it on unchanged
  or stopping with a type error; `∈` (also `in`) asks, answering `true` or `false`. Each is
  written prefix, `(⊢ x string)`, or infix, `<x ⊢ string>`, and a `case` branch's head
  `(∈ p)` asks it of the case's target. ([c6d1213], [69e2c2e])
- Any form can be written infix between angle brackets, `<a op b>` being `(op a b)`, and a
  chain threads, `<x ⊢ record ⊢ titled>`. ([3529b6b])
- Signatures are written with binders, `(<id ⊢ uri> <label ⊢ string>)`, a pattern's too,
  `<(?a ?b) ⊢ array>`, and a return type after the name, `(define/procedure f ⊢ record (...)
  ...)`. A signature is kept with its function, and checked when it is called.
  ([764c698], [9a72541])
- Refinements defined in Ouro, with `define/refinement`: one typed parameter, a body that
  answers `true` or `false`, and a `doc` that a failure reports as the rule it broke,
  `titled: Has a label.` A refinement is asserted by its name, `<r ⊢ titled>`, in a signature
  as anywhere, and may go down into its value, so a tree is checked at every depth. One
  without a doc warns (`WRN0007`). ([cbbe699], [4f77773], [305b528], [ab52bac])
- Types combine as sets: `(∪ a b)`, `(∩ a b)`, `(∖ a b)`, `(array p)` for an array whose every
  element is in `p`, and `(record :k p ...)` for a record with at least these keys, each value
  in its type. A failure names the part that failed, `an array whose element 3 is string`, `a
  record without label`. `(record)`, with no keys, warns that it is every record (`WRN0008`).
  ([96dd471], [eba4e3e], [b518516], [386ce4a])
- `(make record ...)` and `(make array ...)` build a record or an array whatever the shape of
  what follows, and `(make record)` and `(make array)` are the empty ones: a record of only
  inlays can now be written (gap 14). ([4642487])
- `(shift date duration)` moves a date, back with a negative duration. ([70a95b1])
- `prose` writes long text as bar lines, each raw to the end of its line, joined into one
  string, a bar alone being a paragraph break. A triple-quoted string warns that it is three
  strings, and points to `prose` (`WRN0009`). ([9e4e75f], [0f86ded])
- `(document name expr)` declares a document, the output file, in place of `graph`.
  ([4636b37])

### Changed
- **Breaking:** comparisons are words: `<`, `>`, `<=` and `>=` are errors that name `lt`,
  `gt`, `leq` and `geq`, since `<` and `>` now write infix forms. ([de25bf6])
- **Breaking:** `make`, `assert` and `in` are read by their syntax, so a function cannot take
  their names (`SYN0008`). ([4642487], [c6d1213])
- `uri` and `date` are refinements of strings: an asserted value passes on as it was written,
  never converted, and a date is read where it is used, by `shift`. ([70a95b1])
- Messages name refinements as they are written, `Expected data to have the refinement
  number, got string instead.`, and their examples are in the current syntax.
  ([01996c7], [9a809e7])
- `std` and the fixtures are written in the current syntax. ([d77d37a])
- A build of one document says `1 document`. ([86c0c71])

### Deprecated
- The old spellings with a hash mark, a refinement before a value or a parameter or after a
  definition's name, the empty record and array, and `satisfies` before such a refinement,
  still work, each with a warning that names its replacement where it is written, `<x ⊢
  string>`, `⊢ string`, `(make record)`, `<x ∈ string>` (`WRN0001`). They are errors in
  0.8.0.0. ([50021e0], [c419ea6])
- `graph` still declares a document, with a warning that names `document` (`WRN0001`).
  ([4636b37])
- `ouro build -g` still chooses documents, left out of the help, with a warning that names
  `-t`, `--target`, which replaces it. ([4636b37], [c419ea6])
- The build setting `:graphs` still chooses documents, read when `:documents` is not given,
  with a warning that names `:documents` (`WRN0001`). ([4636b37], [60a3e94])

### Removed
- **Breaking:** `(list a b)` is an error that names `array`, as 0.6.2.0 warned (gap 55).
  ([6e7bba8])

## [0.6.2.0] - 2026-11-17

### Added
- `(array a b)` builds an Array, as `(list a b)` did: the value is an Array everywhere else,
  in `#array`, in messages and in JSON (gap 55). `std` builds its arrays with it. ([74c4e09])

### Changed
- A graph compiles in about a fifth less time, allocation and memory: a hoard of 2000 coins
  takes 0.59 s where it took 0.76 s, measured together, and its peak memory is 100 MB where
  it was 125 MB. The JSON writer no longer holds its whole output as work left to do, and
  writes a string a run of characters at a time. ([e59379f], [e5502f9], [fb96aed])

### Deprecated
- `(list a b)` still builds an Array, with a warning that names `array`, `'list' is now called
  'array'` (`WRN0001`). It will be an error in 0.7 (gap 55). ([74c4e09])

### Removed
- The warning `WRN0000`, that a string after `#record-empty` or `#array-empty` would be
  discarded: it never was. In an array the string is an element, and in a record a loose value
  is an error already. Its code is not reused (gap 41). ([c197912])

### Fixed
- Set operations (`union`, `intersect`, `setminus`, `subset`, `distinct`, `unions`,
  `intersections`, `image` and `adjoin`) take time near linear in their arrays' length, not
  near its square: the union of 2000 records with themselves takes 0.52 s, where it took
  4.4 s (gap 56). ([8b2a239])
- `ouro validate` checks a TimeSpan's dates: `begin_of_the_begin`, `end_of_the_begin`,
  `begin_of_the_end` and `end_of_the_end` must each be a date, where a value that was not one
  was written back as a string, and the middle two were never read as dates (gap 54).
  ([fee0e02])
- Diagnostics are coloured only when stderr is a terminal and `NO_COLOR` is not set, so a log,
  a pipe or a CI report holds plain text, without escape codes (gap 37). ([1867050])
- `ouro validate` reports invalid JSON on stderr, as `build` reports its diagnostics, so
  `ouro validate in.json > out.json` never writes an error as the JSON (gap 38). ([18551ae])
- A string that is never closed is reported at its opening quote, `This string is never
  closed`, where the error was at the end of the file, far from its cause (gap 23).
  ([5d05989])
- `ouro validate` reads JSON's escapes in strings: `\u00e9` is `é`, a surrogate pair one
  character, and `\/` a slash. Haskell's escapes, such as `\x41`, a lone surrogate and a raw
  control character are errors, as JSON has them (gap 21). ([cb6ce34])
- `ouro build` writes all or nothing: every graph is staged first, and moved into place only
  when all are, so a graph that cannot be written, such as one whose folder is a file, leaves
  none written and ends in `Build Failed`, where the graphs before it were written and the
  build ended in a Haskell exception (gap 35). ([784d109])

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

[Unreleased]: https://github.com/protocol-oberon/ouro/compare/v0.8.0.0...HEAD
[0.8.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.7.0.1...v0.8.0.0
[0.7.0.1]: https://github.com/protocol-oberon/ouro/compare/v0.7.0.0...v0.7.0.1
[0.7.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.6.2.0...v0.7.0.0
[0.6.2.0]: https://github.com/protocol-oberon/ouro/compare/v0.6.1.0...v0.6.2.0
[0.6.1.0]: https://github.com/protocol-oberon/ouro/compare/v0.6.0.0...v0.6.1.0
[0.6.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.5.0.0...v0.6.0.0
[0.5.0.0]: https://github.com/protocol-oberon/ouro/compare/v0.0.0.1...v0.5.0.0
[0.0.0.1]: https://github.com/protocol-oberon/ouro/releases/tag/v0.0.0.1
[b6b8350]: https://github.com/protocol-oberon/ouro/commit/b6b8350
[d42fa2c]: https://github.com/protocol-oberon/ouro/commit/d42fa2c
[6520853]: https://github.com/protocol-oberon/ouro/commit/6520853
[221bc7f]: https://github.com/protocol-oberon/ouro/commit/221bc7f
[a79bc97]: https://github.com/protocol-oberon/ouro/commit/a79bc97
[73d5779]: https://github.com/protocol-oberon/ouro/commit/73d5779
[44b6293]: https://github.com/protocol-oberon/ouro/commit/44b6293
[ec69b48]: https://github.com/protocol-oberon/ouro/commit/ec69b48
[c6d1213]: https://github.com/protocol-oberon/ouro/commit/c6d1213
[69e2c2e]: https://github.com/protocol-oberon/ouro/commit/69e2c2e
[3529b6b]: https://github.com/protocol-oberon/ouro/commit/3529b6b
[764c698]: https://github.com/protocol-oberon/ouro/commit/764c698
[9a72541]: https://github.com/protocol-oberon/ouro/commit/9a72541
[cbbe699]: https://github.com/protocol-oberon/ouro/commit/cbbe699
[4f77773]: https://github.com/protocol-oberon/ouro/commit/4f77773
[305b528]: https://github.com/protocol-oberon/ouro/commit/305b528
[ab52bac]: https://github.com/protocol-oberon/ouro/commit/ab52bac
[96dd471]: https://github.com/protocol-oberon/ouro/commit/96dd471
[eba4e3e]: https://github.com/protocol-oberon/ouro/commit/eba4e3e
[b518516]: https://github.com/protocol-oberon/ouro/commit/b518516
[386ce4a]: https://github.com/protocol-oberon/ouro/commit/386ce4a
[4642487]: https://github.com/protocol-oberon/ouro/commit/4642487
[70a95b1]: https://github.com/protocol-oberon/ouro/commit/70a95b1
[9e4e75f]: https://github.com/protocol-oberon/ouro/commit/9e4e75f
[0f86ded]: https://github.com/protocol-oberon/ouro/commit/0f86ded
[4636b37]: https://github.com/protocol-oberon/ouro/commit/4636b37
[de25bf6]: https://github.com/protocol-oberon/ouro/commit/de25bf6
[01996c7]: https://github.com/protocol-oberon/ouro/commit/01996c7
[9a809e7]: https://github.com/protocol-oberon/ouro/commit/9a809e7
[d77d37a]: https://github.com/protocol-oberon/ouro/commit/d77d37a
[86c0c71]: https://github.com/protocol-oberon/ouro/commit/86c0c71
[50021e0]: https://github.com/protocol-oberon/ouro/commit/50021e0
[c419ea6]: https://github.com/protocol-oberon/ouro/commit/c419ea6
[60a3e94]: https://github.com/protocol-oberon/ouro/commit/60a3e94
[6e7bba8]: https://github.com/protocol-oberon/ouro/commit/6e7bba8

[74c4e09]: https://github.com/protocol-oberon/ouro/commit/74c4e09
[e59379f]: https://github.com/protocol-oberon/ouro/commit/e59379f
[e5502f9]: https://github.com/protocol-oberon/ouro/commit/e5502f9
[fb96aed]: https://github.com/protocol-oberon/ouro/commit/fb96aed
[c197912]: https://github.com/protocol-oberon/ouro/commit/c197912
[8b2a239]: https://github.com/protocol-oberon/ouro/commit/8b2a239
[fee0e02]: https://github.com/protocol-oberon/ouro/commit/fee0e02
[1867050]: https://github.com/protocol-oberon/ouro/commit/1867050
[18551ae]: https://github.com/protocol-oberon/ouro/commit/18551ae
[5d05989]: https://github.com/protocol-oberon/ouro/commit/5d05989
[cb6ce34]: https://github.com/protocol-oberon/ouro/commit/cb6ce34
[784d109]: https://github.com/protocol-oberon/ouro/commit/784d109

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
