# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax

Ouro like any other lisp is anchored around the opening parentheses of an expression `()`. Its compiler uses a lookahead to determine the target JSON-LD structure of incoming Ouro source code, whether it maps to a JSON-LD Array, Record or is an Ouro specific function application. Ouro also makes use of lisp key words to match the classic JSON *key* : *value* syntax (these are referred to as **Attributes**). This allows for an Ouro file's source code to roughly match the topology of JSON that it compiles to. 

A Ouro file consists of a collection of top-level, _higher expressions_. There are three types of higher expressions:

1. `(import path)` and `(export name ...)`: the modules a file uses and the functions it offers, written first in the file (see [Modules](#modules)).
2. `(graph name expr)`: Expressions that encode JSON-LD graphs. A graph is one expression, usually a record.
3. `(defn name (args) expr)`: Ouro supports _higher order functions_,  `(defn)` is a named function. `(fn (x y) expr)` is a lambda function. 

The target structure of Ouro code is determined by a few simple rules, which only look at an expression's direct elements, never at their types:

1. An expression whose first element is a symbol `(sym expr expr)` is a function application, where the symbol is the function and the rest of the expression are its arguments.
2. Otherwise, an expression that contains an attribute pair `(:key value)` will compile to a JSON-LD **Object**.
3. Anything else, such as `(1 2 3)` or `()`, will compile to a JSON-LD **Array**.

Through composition of these rules it is possible to ergonomically express any JSON-LD structure in Ouro with just parentheses. For example to make an nested list of records the following syntax would be used `((:key val) (:key val))`.

- Arrays in Ouro, like JSON arrays, may contain values of mixed types, `(1 "a" (:k 1))`. `(insert)` can put any value into an Array, but not a `:key value` attribute pair.

Ouro is 100% declarative, and lazily evaluated. This allows for the values of records to not only be referenced within the local scope of the expression, but also out of declaration order. To reference the value of an attribute pair, simply call the key without the colon, so `:lookup value` becomes `lookup`, which returns the `value`.

Each key can appear once in a record: writing a key twice is an error. An attribute pair never sees itself, so in `(:label label)` the `label` being looked up is the one from the enclosing scope. This is what lets a template simply write `(defn node (id type) (:id id :type type))`.

Source files are UTF-8 on every platform, so strings, names and keys can hold any text: `(:_label "Édouard Manet")` compiles to `"_label": "Édouard Manet"` on Windows as well as Linux. A file saved in another encoding is reported, not garbled.

## Built-in properties

In JSON-LD, there are many datatypes which can look similar to a compiler. To ensure that the compiled JSON from Ouro code is always well formed, we can give an expression a property, which tells the compiler how to treat it. A property is written before the expression, as in `#str (expr)`.

There are 8 built-in properties:

| Property     | Description                                                                              |
|:-------------|:-----------------------------------------------------------------------------------------|
| `#uri`       | Ensures that any expression that evaluates to a URI is properly formed.                  |
| `#date`      | Ensures valid ISO 8601 date-time formats and enables temporal desugaring (e.g., `thru`). |
| `#num`       | Asserts that the evaluated result is a numeric primitive.                                |
| `#str`       | Asserts that the evaluated result is a string.                                           |
| `#bool`      | Asserts that the evaluated result is a boolean primitive.                                |
| `#rec`       | Asserts that the evaluated result is a Record.                                           |
| `#arr`       | Asserts that the evaluated result is an Array.                                           |
| `#fn`        | Asserts that the evaluated result is a function (a `fn`, a `defn` name or a built-in).   |

A value without the property it is given is an error, such as `Expected data to have the property #num, got #str instead.` The same properties type a function's parameters and result (see [Functions](#functions)).

Two further `#` forms are values rather than properties, written in place of an empty collection:

| Value        | Description                                                                              |
|:-------------|:-----------------------------------------------------------------------------------------|
| `#arr-empty` | An empty Array, `[]` (the same as `()`).                                                 |
| `#rec-empty` | An empty Record, `{}`.                                                                   |

## Special forms

Ouro contains special forms designed for increased ergonomics for creating JSON-LD graphs. These forms interact with the JSON-LD graph in different ways.

### Let

In Ouro there are two ways to bind a name, with difference in serialisation.

| Form                           | Binds               | Output?                   | Scope                         |
|:-------------------------------|:--------------------|:--------------------------|:------------------------------|
| `(:name expr ...)` (a record)  | its attribute pairs | yes, every attribute pair | the record's own expressions  |
| `(let ($name expr ...) body)`  | its bindings        | no, only `body`           | the bindings and `body`       |

Both are recursive (`letrec`), the bindings can refer to each other in any order, and a cycle is an error. An inner binding shadows an outer one, but a binding is always evaluated in the scope where it was written:

```clojure
(let ($artist     "Manet"
      $painted-by artist)                 ; written here, so "Manet"
  (:provenance (:artist     "Proust"
                :owner      artist        ; the inner 'artist': "Proust"
                :painted-by painted-by))) ; still "Manet"
```

To summarise, bindings with `:` are serialised and ones with `$` can only be declared inside of `(let (bindings) (body))` and are not serialised. `(let)` bindings are also important in defining local functions.

``` clojure
(let ($local (fn (n) (+ 1 n)))
  (:res (local 1)))
```

Here `res` will evaluate to 2. 

### Context

A record's JSON-LD context is written `:context value`. The value is an ordinary expression, written as JSON-LD writes a context: the URL of a remote context, a local context record, `null`, or an array of them, applied in order. Most Linked Art documents need only the remote context:

```clojure
(:context "https://linked.art/ns/v1/linked-art.json" ...)
```

A project that extends Linked Art adds its own terms after it, as an array:

```clojure
(graph extended
  (:context ("https://linked.art/ns/v1/linked-art.json"
             (:ex        "https://example.org/ns/"
              :condition (:@id "ex:condition" :@type "@id")))
   :type      "HumanMadeObject"
   :condition "https://example.org/condition/good"))
```

...compiles to:

``` json
{
  "@context": [
    "https://linked.art/ns/v1/linked-art.json",
    {
      "ex": "https://example.org/ns/",
      "condition": {
        "@id": "ex:condition",
        "@type": "@id"
      }
    }
  ],
  "type": "HumanMadeObject",
  "condition": "https://example.org/condition/good"
}
```

- A key starting with `@` is a JSON-LD keyword: `(:@vocab "https://example.org/" :@language "en")` is a local context. Any other key defines a term, which maps to an IRI, to `null`, or to an expanded definition record (`:@id`, `:@type`, `:@container`, ...).
- A context is a value like any other, so a function can build one: `(defn linked-art () "https://linked.art/ns/v1/linked-art.json")`, then `:context ((linked-art) (:ex "https://example.org/ns/"))`.
- A malformed context is reported where it is written: a remote context that is not an absolute URL, an unknown `@` keyword, or a term mapped to a Number, for example.
- `:context null` clears the context, and `:@context` means the same as `:context`.

### Get


The `(get)` special form evaluates its target and walks the resulting records, so it works through function results, such as `(inlay)`, `(case)` and `(let)`. Only a record's attribute pairs can be reached, `(let)` bindings are private. Path components are bare symbols (`(get p q r)`, not `(get p :q)`) or whole numbers, which index an Array from 0: `(get p classified_as 0 _label)` is the label of the first classification, and a negative index counts from the end, so `(get xs -1)` is the last element. An index only indexes Arrays, since a record's attribute pairs have no order in JSON-LD and are reached by key. A hole can stand for the target, `(get ? 0)`, but never for a key or an index: a path is always written. 

``` clojure
(:nested_manifest (:id       #uri (+ api-root "dataset")
                   :created  #date "2026-05-28T12:00:00Z"
                   ;; Deep structural nesting
                   :meta     (:version       "v2.1.0"
                              :release_code  905
                              :maintainer    (:name    "Dev Team"
                                              :is_valid is-active)))
;; Lookup nested values with 'get'
:status          (get nested_manifest meta version)
:runtime_check   (get nested_manifest meta maintainer is_valid))
```

`(has record key ...)` is `true` when the path reaches a value, and `(get-or default record key ...)` is the value at the path, or `default` when a key on the way is missing or an index is past the end:

``` clojure
(let ($p (:classified_as ((:_label "Painting") (:_label "Oil")) :xs (1 2 3)))
  (:first (get p classified_as 0 _label)                  ; "Painting"
   :last  (get p xs -1)                                   ; 3
   :has   (has p classified_as 5)                         ; false
   :or    (get-or "none" p classified_as 5 _label)))      ; "none"
```

### Image

`(image record key ...)` reads a path the way JSON-LD reads properties: as a set of values. It goes through Arrays, so it follows a key from every element it reaches, a record without the key adds nothing, and the result is always an Array with no repeats, kept in the order they are first reached:

``` clojure
(graph kinds
  (let ($book (:part ((:classified_as ((:id "aat:leaf-marker" :_label "Leaf marker")))
                      (:classified_as ((:id "aat:leaf-marker" :_label "Leaf markers")))
                      (:_label "An unclassified part"))))
    (:kinds (image book part classified_as id)
     :none  (image book tp46))))
```

...compiles to:

``` json
{
  "kinds": ["aat:leaf-marker"],
  "none": []
}
```

Following `id` compares by identity: the same type written with two labels is one element. Use `(get)` where the data has one value by construction, and `(image)` for anything JSON-LD reads as a set. Because a missing key gives `()` rather than an error, a misspelt key gives an empty set. `image` takes no indexes, since a set has no positions.

### Case

The `(case)` special form serves as the Ouro's main mode of control flow, it expects a target expr `(case <trgt>)` followed by a list of tuples, (pattern value).

``` clojure
(:trgt   99
 :result (case trgt
               ((eq 100)  "fails")
               ((> 100)   "fails")
               (otherwise "fails")))
```

`case` statements must always contain a default branch `otherwise`, to ensure totality. When a case statement is triggered, the target expr is eagerly evaluated, and passed into the first element of branch tuple of the branch as its first argument `((eq trgt 100) ret1)`. If the resulting expression evaluates to `True`, then the second element of the branches tuple (the return value) is evaluated and returned.

`case` can also destructure Arrays and Records. A branch whose pattern is a Record, or contains a hole (`?`), matches the *value* of the target against that shape:

- `?` matches anything, and `?name` binds the matched value to `name` inside the branch.
- `?..name` binds everything that is left: the remaining elements of an Array, or the attribute pairs of a Record that the pattern doesn't mention.
- Anything else in a pattern (a literal, a variable, an expression) is evaluated and compared by value.

Because Ouro has no side effects, an expression and its value are interchangeable, so patterns only ever see values. `((+ 2 4) 3)` and `(6 3)` match exactly the same patterns:

```clojure
(:trgt   ((+ 2 4) 3)
 :result (case trgt
               ((6 ?x)    x)
               (otherwise 0)))
```

...which compiles to:

```clojure
(:trgt   (6 3)
 :result 3)
```

A case statement in Ouro also supports multiple clauses in a branches pattern allowing for a more condensed syntax for when multiple branches return the same value. This done by putting the conditions for the branch in a list.

``` clojure
(case name
      (("Manet" "Proust") linked-art-person)
      ("Spring"           linked-art-object)
      (otherwise          getty)))
```

Is de-sugared to:

``` clojure
(case name
      ("Manet"   linked-art-person)
      ("Proust"  linked-art-person)
      ("Spring"  linked-art-object)
      (otherwise getty))
```

A list of literals without holes, like `("Manet" "Proust")` above, is always this alternatives sugar rather than a pattern.

A branch can also test a condition instead of a shape. `(when cond)` takes the branch when `cond` is `true`, and tries the next branch when it is `false`; a condition that isn't a Boolean is an error. `(satisfies #p)`, also written `(⊨ #p)`, takes the branch when the target has the property `#p`:

``` clojure
(map (fn (v) (case v
                   ((when (eq v 0))  "zero")
                   ((satisfies #num) "a number")
                   ((⊨ #str)         "a string")
                   (otherwise        "other")))
     (0 7 "x" true))
```

...is `["zero", "a number", "a string", "other"]`. Outside a branch, `(satisfies #p x)` is a question with a Boolean answer, while `#p x` is an assertion that fails.

Array patterns match by position. The following:

 ``` clojure
 (graph case-statements
   (:trgt (1 2 3)
    :res  (case trgt
                ((?head ?..rest) rest)
                (otherwise       trgt))))
 ```
 
... binds the first element to `head` and the remaining elements to `rest`, which is returned. A `?..rest` hole must come last in an Array pattern. This graph compiles to:

``` json
{
  "trgt": [
    1,
    2,
    3
  ],
  "res": [
    2,
    3
  ]
}
```

Record patterns match by key, in any order, and attribute pairs the pattern doesn't mention are allowed, so `((:type "Person") ...)` matches any Person record. The following Ouro graph matches the `name` attribute pair without binding it, and returns the other attribute pairs:

``` clojure
 (graph case-statements
   (:trgt (:name "Manet"
           :id   "https://linked.art/example/person/manet"
           :type "Person")
    :res  (case trgt
                ((:name ? ?..rest) rest)
                (otherwise         trgt))))
```

...compiles to:

``` json
{
  "trgt": {
    "name": "Manet",
    "id": "https://linked.art/example/person/manet",
    "type": "Person"
  },
  "res": {
    "id": "https://linked.art/example/person/manet",
    "type": "Person"
  }
}
```

### List

The compiler decides what an expression is from its shape alone, so there are cases where the developer intent must be made clear to the compiler. An example of this is the following expression `(id name type)`. Because its first element is a symbol, the compiler reads this as an expression where a function, `id` takes two arguments `name` and `type`.  However if the developer intends for this to be a list of values where the variables `id`, `name` and `type` are looked up, then they will have to use the `(list)` special form. The same goes for an Array of one bound value: `(manet)` would call `manet`, while `(list manet)` is an Array holding its value.

```clojure
(list id name type)
```

This form simply tells the compiler to evaluation every expression after a `list` and store them in-place. So the following Ouro code:

``` clojure
(let ($name "Manet"
      $id   "https://linked.art/example/person/manet"
      $type "Person")
  (:_list (list name id type)))
```

...complies to the following JSON-LD:

``` json
{
  "_list" : [ "Manet", "https://linked.art/example/person/manet", "Person"]
}
```

### Map

`(map f xs)` calls the function `f` on each element of the Array `xs`, and returns an Array of the results, in order. `f` can be a `defn`, a `fn`, a builtin or a [placeholder](#placeholders), so most uses need no `fn` at all:

``` clojure
(defn aat-type (id label)
  (:id #uri (+ "http://vocab.getty.edu/aat/" id) :type "Type" :_label label))

(graph mapped
  (:classified_as (map (aat-type ? "Painting") ("300033618" "300177435"))
   :counts        (map (+ ? 1) (1 2 3))))
```

...compiles to:

``` json
{
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300033618",
      "type": "Type",
      "_label": "Painting"
    },
    {
      "id": "http://vocab.getty.edu/aat/300177435",
      "type": "Type",
      "_label": "Painting"
    }
  ],
  "counts": [2, 3, 4]
}
```

Each element is passed to `f` as an ordinary call, so `f` must take exactly one argument, and its signature checks every element: a mismatch is reported at the Array. Mapping over an empty Array gives an empty Array, and mapping over anything other than an Array is an error. `map` cannot be the name of a `defn`, but `:map` is an ordinary key and `$map` an ordinary binding.

When the elements are themselves Arrays or Records, a [pattern parameter](#destructuring-parameters) takes each one apart: `(map (fn ((?slug ?label)) (person slug label)) pairs)`.

### Inlay

`(inlay)` is a special form used to nest the contents of a record into the
current record. For example the following Ouro code:

``` clojure
(:context "https://linked.art/ns/v1/linked-art.json"
 :id      #uri "https://linked.art/example/provenance/manet_proust/1"
 :type    "Activity"
 :_label  "Purchase of Spring by Proust"
 (inlay (:begin_of_the_begin #date "1881-01-01T00:00:00Z"
         :end_of_the_end     #date (+ begin_of_the_begin (thru (years 2))))))
```

...is complied to the following JSON.

``` json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://linked.art/example/provenance/manet_proust/1",
  "type": "Activity",
  "_label": "Purchase of Spring by Proust",
  "begin_of_the_begin": "1881-01-01T00:00:00Z",
  "end_of_the_end": "1883-12-31T23:59:59Z"
}
```

Inlays are layers. Since each key can appear once in a record, when an inlaid record has a key that is already written in the record, the written attribute pair wins, wherever the inlay is placed. When two inlays share a key, the later inlay wins. This makes inlaying a template and overriding some of its attribute pairs straightforward:

``` clojure
(defn base () (:type "Type" :_label "Painting"))

(graph overridden
  (:_label "Spring"
   (inlay (base))))
```

...compiles to:

``` json
{
  "_label": "Spring",
  "type": "Type"
}
```

Inlay is useful for systematically merging records, but it can also be used to merge lists. The following Ouro code:

``` clojure
(:test_list (1 2 3 (inlay to_merge))
 :to_merge  (4 5 6))
```

...is compiled to the following JSON.

``` json
{
  "test_list": [
    1,
    2,
    3,
    4,
    5,
    6
  ],
  "to_merge": [
    4,
    5,
    6
  ]
}
```

An inlaid record's `@context` is layered in too. The record ends up with the inlays' contexts, in order, then its own, so its own context, applied last, overrides a term both define. The same context twice in a row, such as Linked Art's from both an inlaid node and the record, is written once.

### With and ->

`(with base :key value ...)` is `base` with the attribute pairs written over it, exactly as `((inlay base) :key value ...)`: a written attribute pair wins over the base's own. `(-> x step ...)` threads a value through steps, each receiving the value before it as its first argument. Together they build a record step by step:

```clojure
(defn core (id type label)
  (:id #uri id :type type :_label label))

(defn aat (id label)
  (core (+ "http://vocab.getty.edu/aat/" id) "Type" label))

(defn classified (r t) (with r :classified_as (list t)))
(defn named      (r n) (with r :identified_by (list (:type "Name" :content n))))

(graph amsterdam
  (-> (core "https://linked.art/example/place/amsterdam/1" "Place" "Amsterdam")
      (with :context "https://linked.art/ns/v1/linked-art.json")
      (classified (aat "300008389" "City"))
      (named "Amsterdam")))
```

...compiles to:

``` json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://linked.art/example/place/amsterdam/1",
  "type": "Place",
  "_label": "Amsterdam",
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300008389",
      "type": "Type",
      "_label": "City"
    }
  ],
  "identified_by": [
    {
      "type": "Name",
      "content": "Amsterdam"
    }
  ]
}
```

- A step's value goes in as its first argument, or where a hole marks it: `(-> 1 (- 10))` is `-9`, and `(-> 1 (- 10 ?))` is `9`. A bare name is called with the value, and so is a `fn`.
- `(-> ? step ...)` is a function of the value, so a pipeline can be mapped: `(map (-> ? (with :seen true)) records)`.
- `with` extends an Array too: `(with (1 2) 3 4)` is `(1 2 3 4)`.

### Insert

`(insert)` serves as the dual to `(inlay)`, while `(inlay)` is used to bring things into the current scope, `(insert)` takes either a Record, or an Array, and merges its contents into the current Record/Array, `(insert)` does the same from an outside perspective. It takes an **insert position**, the payload to be inserted and a **Record**/**Array** to insert into. As Ouro is a functional language, `(insert)` returns a new instance of the original record, with the values inserted rather than mutating in-place. In this sense, `(insert)` is not to dissimilar to *lenses* in Haskell.    

**Example**

The following Ouro code:

``` clojure
(:inserted_into (:id   "https://linked.art/example/person/proust"
             :type "Person")

 :record    (insert 2 :_label "Proust" inserted_into))
```

...compiles to the following JSON:

``` json
{
  "inserted_into": {
    "id": "https://linked.art/example/person/proust",
    "type": "Person"
  },
  "record": {
    "id": "https://linked.art/example/person/proust",
    "type": "Person",
    "_label": "Proust"
  }
}
```

A record can't end up with a key twice, so inserting an attribute pair whose key already exists is an error. To change an existing attribute pair or element, use `(replace-at)`, which takes the same arguments and replaces whatever is at the index. Negative indexes count from the end in `(insert)` and `(replace-at)`, as they do in a path.

### And, Or and Quantifiers

`(and a b ...)` is `true` when every operand is, and `(or a b ...)` when at least one is. They stop at the operand that decides the answer, so later operands are never evaluated:

``` clojure
(graph tags
  (let ($tags ())
    (:featured (and (> (len tags) 0) (eq (get tags 0) "featured"))    ; false: get is never reached
     :listed   (or  (eq (len tags) 0) (eq (get tags 0) "listed")))))  ; true
```

- Each operand that is reached must be a Boolean: `(and true 1)` is an error, `Expected data to have the property #bool, got #num instead.` Ouro has no truthiness.
- `(and)` is `true`, and `(or)` is `false`.
- They take holes like a call, `(map (or ? false) (true false))`, and are threading steps, `(-> true (and false))`.

The quantifiers ask the same of every element of an Array. `(forall f xs)` is `true` when `f` answers `true` for every element, `(exists f xs)` when it does for at least one, and `(omits f xs)` when it does for none. Each stops at the element that decides. `(filter f xs)` keeps the elements `f` answers `true` for:

``` clojure
(let ($present ("leaf marker" "clasp")
      $absent  ("endband"))
  (:consistent (omits (contains absent ?) present)        ; true: nothing is both present and absent
   :any-clasp  (exists (= ? "clasp") present)             ; true
   :long       (filter (fn (s) (> (len s) 6)) present)))  ; ("leaf marker")
```

`omits` is named for what the data omits, not for what does not exist: a record's silence is not an observation, so `omits` says only that the Array has no such element.

### Symbols

The connectives, the quantifiers and the set operations can be written with the symbols of logic and sets. Each is read as the name it stands for, so `(∧ a b)` is `(and a b)`, and messages use the name:

| Symbol | Name           | Symbol | Name            |
|:-------|:---------------|:-------|:----------------|
| `¬`    | `not`          | `∪`    | `union`         |
| `∧`    | `and`          | `∩`    | `intersect`     |
| `∨`    | `or`           | `∖`    | `setminus`      |
| `∀`    | `forall`       | `⊆`    | `subset`        |
| `∃`    | `exists`       | `⋃`    | `unions`        |
| `∄`    | `omits`        | `⋂`    | `intersections` |
| `⊨`    | `satisfies`    | `=`    | `eq`            |
| `λ`    | `fn`           | `/=`   | `neq`           |

``` clojure
(defn consistent #bool (#rec book)
  (∄ (contains (absent book) ?) (present book)))
```

## Builtin Functions

Ouro has the following built in functions:

**Variadic Arithmetic & Temporal Shifting:** The arithmetic functions in Ouro are fully variadic and handle both standard mathematical computations and strict #date temporal adjustments.

| Function | Syntax Example                         | Behaviour / Domain Rules                                          |
|:---------|:---------------------------------------|:------------------------------------------------------------------|
| **`+`**  | `(+ 2 2 6)` -> `10`                    | Computes the sum of all numerical arguments.                      |
|          | `(+ #date "2026-06-14..." (years 1))`  | When provided a `#date` head, applies time windows forward.       |
|          | `(+ "Type: " label)`                   | Joins strings.                                                    |
|          | `(+ (1 2) (3))` -> `(1 2 3)`           | Concatenates arrays.                                              |
| **`-`**  | `(- 20 5 2)` -> `13`                   | Sequentially subtracts values from the first argument.            |
|          | `(- #date "2026-06-14..." (months 2))` | When provided a `#date` head, rolls time windows backward.        |
| **`*`**  | `(* 2 3 4)` -> `24`                    | Computes the product of all numerical arguments.                  |
| **`/`**  | `(/ 100 2 5)` -> `10`                  | Sequentially divides the first argument by the subsequent values. |

Time windows are made with `(years n)`, `(months n)`, `(days n)`, `(hours n)`, `(minutes n)` and `(seconds n)`. Shifting by months or years clips to the end of a shorter month instead of rolling over into the next one, so 31 March minus one month is the last day of February.

**Relational Comparisons:** Relational operators evaluate numbers. They support variadic chaining, verifying that the comparison holds true sequentially across every element in the expression.

| Function  | Syntax Example | Evaluation Matrix                                 |
|:----------|:---------------|:--------------------------------------------------|
| **`>`**   | `(> 10 5 2)`   | Evaluates to `true` (Strictly decreasing stream). |
| **`>=`**  | `(>= 10 10 5)` | Evaluates to `true` (Decreasing or equal stream). |
| **`<`**   | `(< 2 5 10)`   | Evaluates to `true` (Strictly increasing stream). |
| **`<=`**  | `(<= 5 5 10)`  | Evaluates to `true` (Increasing or equal stream). |
| **`eq`**  | `(eq 1 1 1)`   | Evaluates to `true` (All values are equal).       |
| **`neq`** | `(neq 1 2)`    | Evaluates to `true` (The values differ).          |

`eq` compares records and arrays by value, and `=` and `/=` are `eq` and `neq`. Using an operator with the wrong number of operands is an arity error.

**Logic:** `not` negates a Boolean: `(not false)` is `true`. `and` and `or` are special forms, described above.

**Length:** `len` counts an Array's elements, whatever their types, or a String's characters.

| Function  | Syntax Example           | Behaviour / Domain Rules              |
|:----------|:-------------------------|:--------------------------------------|
| **`len`** | `(len (1 (2 3)))` -> `2` | Counts elements; `()` has length `0`. |
|           | `(len "Málaga")` -> `6`  | Counts characters, not bytes.         |

Any other value, such as a record or a `#uri`, is a type error.

**Collections and sets:** JSON-LD reads a property's values as a set, and so do these. Values are compared as `eq` compares them, and a result keeps the first of each value, in the order it is first written, so the output is always the same.

| Function            | Syntax Example                           | Behaviour / Domain Rules                        |
|:--------------------|:-----------------------------------------|:------------------------------------------------|
| **`contains`**      | `(contains (1 2 3) 2)` -> `true`         | An Array has the value; a String the substring. |
| **`distinct`**      | `(distinct (1 2 1 3))` -> `(1 2 3)`      | The first of each value.                        |
| **`union`**         | `(union (1 2) (2 3))` -> `(1 2 3)`       | Every value in any of the Arrays.               |
| **`intersect`**     | `(intersect (1 2 3) (2 3 4))` -> `(2 3)` | The values in all of them.                      |
| **`setminus`**      | `(setminus (1 2 3) (2))` -> `(1 3)`      | The values of the first in none of the rest.    |
| **`subset`**        | `(subset (1 2) (1 2 3))` -> `true`       | Every value of the first is in the second.      |
| **`unions`**        | `(unions ((1) (1 2) (3)))` -> `(1 2 3)`  | The union of an Array of Arrays.                |
| **`intersections`** | `(intersections ((1 2) (2 3)))` -> `(2)` | The intersection of an Array of Arrays.         |

## Functions

Like any good Lisp, Ouro supports powerful first class functions. A function's body is one expression, written exactly as it would be anywhere else, and its value is the result: a Record, an Array or a Primitive.

```clojure
(defn add-one (n)
  (+ 1 n))
```

| Form                     | Meaning                                                             |
|:-------------------------|:--------------------------------------------------------------------|
| `(defn name (x y) expr)` | a named function; top level only; visible throughout the file       |
| `(fn (x y) expr)`        | an anonymous function value, closed over the scope it is written in |
| `(λ (x y) expr)`         | the same as `fn`                                                    |
| `(f a b)`                | a call: `f` is any expression that evaluates to a function          |

A local function is a `let`-bound `fn`:

``` clojure
(let ($attribute (fn (label) (+ label " (attributed)")))
  (:attributed (attribute (attribute "Manet"))))
```

A function binds its own private values with `let`, just as a graph does:

``` clojure
(defn type-node (aat-id label)
  (let ($prefix "Type: ")
    (:id     #uri (+ "http://vocab.getty.edu/aat/" aat-id)
     :type   "Type"
     :_label (+ prefix label))))
```

Because parentheses around values make an Array, `(fn () ("hello"))` returns `["hello"]`, not `"hello"`. The compiler warns about this (`WRN0002`) when a result is a single plain value in parentheses.

Semantics of a call:

- *Arity* is checked: a call with the wrong number of arguments is an error. Functions are not curried.

- *Arguments are call-by-need:* each argument becomes a promise over the caller's scope. It is evaluated at most once, when the function first uses it, and always means what it meant at the call site, so a function's own bindings can never capture it. An argument the function never uses is still checked for errors.

- *The body is evaluated in the function's closure*, extended with a frame for the parameters. For a `defn` the closure is the module, for a `fn` it is the scope where the `fn` was written. A function cannot see its caller's bindings.

- *Recursion is forbidden.* Calling a function that is already being called (identified by name and definition position) is an error. Nesting a call, `(f (f x))`, is two calls, not recursion.

- *Functions are values:* they can be passed as arguments and returned, but cannot be compared (`eq` on functions is a type error) or serialised.

- *A returned function is called through `let`.* An expression whose first element is a call is an Array, so `((f x) y)` does not call the result of `(f x)`. Bind it first: `(let ($g (f x)) (g y))`. A function can return a `fn` waiting for the remaining arguments (or a [placeholder](#placeholders), `(+ ? n)`):

  ``` clojure
  (defn adder (n) (fn (x) (+ n x)))

  (graph partial
    (let ($inc (adder 1))
      (:r (inc 2))))
  ```

  ...compiles to `{"r": 3}`.

A `defn` body is checked when the file is parsed: it must be one expression and cannot be a spliced record (`(defn f (x) :k v)`). A `define` or a nested `defn` in it is reported with its replacement, `let` or a `let`-bound `fn`.

A function is documented with a `(doc "...")` among the direct elements of its body, before or after its result. It is never evaluated and is not part of the result:

``` clojure
(defn place #rec (#str slug #str label)
  (doc "A Linked Art Place, under the example namespace.")
  (base/core (example (+ "place/" slug)) "Place" label))
```

Names:

- Functions and graphs are named in kebab-case, `make-place`. A name with a capital or an underscore is a warning (`WRN0004`) that suggests the kebab-case spelling. Attribute keys and local bindings are exempt, since keys follow the data's vocabulary (`:_label`).
- A `defn` may take the name of a built-in, as in Scheme, and replaces it throughout the file, with a warning (`WRN0003`). A special form's name cannot be taken.
- A `/` qualifies an imported name, `base/core`, so no function, parameter or `let` binding can have one. The operators `/` and `/=` are names like any other.

### Signatures

A function's types are written as built-in properties in its header. One between the name and the parameters is the return type, and one before a parameter is that parameter's type. Any of them can be left out.

``` clojure
(defn node #rec (#uri id type)
  (:id id :type type))

(graph typed
  (let ($double (fn #num (#num n) (* n 2)))
    (:r (double 21)
     :n (node "https://linked.art/example/object/spring" "HumanMadeObject"))))
```

- A property in a signature behaves exactly as it would anywhere else: it checks the value, and `#uri` and `#date` convert a string. In `node` above, `id` arrives as a URI.
- Each argument is checked where the call is written, so a mismatch is reported at the argument, in the call. The result is checked where the body is.
- An argument is checked even if the function never uses it.
- Nothing is inferred: an untyped parameter or result is not checked.
- A return type of `#arr` says that a one-value Array is intended, so `(defn f #arr () ("x"))` does not warn.

### When signatures are checked

A signature is checked when a value crosses it, not before evaluation as a static type is, and a failed check is reported where the value came from: a bad argument at the call, a bad result at the function's body.

Since Ouro evaluates everything at compile time, the checks cost nothing in the output: every one has already run before the JSON-LD is written.

User-defined properties are described under [Planned](#planned).

### Placeholders

A call with `?` holes among its arguments is not called: it is a function of its holes. This fixes some arguments now and leaves the rest for later, without writing a `fn` that returns a `fn`:

``` clojure
(defn node (base type id label)
  (:id #uri (+ base id) :type type :_label label))

(graph placeholders
  (let ($aat-type (node "http://vocab.getty.edu/aat/" "Type" ? ?)
        $inc      (+ ? 1)
        $square   (* ?n ?n))
    (:classified_as (list (aat-type "300033618" "Painting"))
     :count         (inc 41)
     :area          (square 7))))
```

...compiles to:

``` json
{
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300033618",
      "type": "Type",
      "_label": "Painting"
    }
  ],
  "count": 42,
  "area": 49
}
```

- Each `?` is a parameter, in the order the holes are written: `(- ? ?)` takes two arguments.
- A named hole is one parameter, however often it is written: `(* ?n ?n)` takes one.
- A typed hole is a typed parameter: `(node #uri ? "Type")` checks and converts its argument, as a signature would.
- A hole belongs to the call it is an argument of, so `(f (g ?))` passes `f` a function.
- Any call takes holes: a `defn`, a `fn`, a builtin, an imported function, and the special forms that evaluate their arguments (`get`, `image`, `insert`, `replace-at`, `list`, `map`). In `get` and `image` a hole stands for the target, never a key or an index.
- A placeholder is an ordinary `fn` value. It is called with exactly as many arguments as it has holes, and functions themselves stay uncurried.
- A hole anywhere else (in an Array, in a Record, as a `let` value) is an error, and `?..rest` can only be written in a pattern. The holes in a `case` pattern or a [pattern parameter](#destructuring-parameters) match values; a placeholder can be written in a branch's body.

### Destructuring parameters

A parameter can be a pattern, written as a `case` pattern is. It takes one argument and names its parts, so a lambda can take a pair or a Record apart where it is written:

``` clojure
(defn person (slug label)
  (:id #uri (+ "https://linked.art/example/person/" slug) :type "Person" :_label label))

(defn label-of ((:_label ?l)) l)

(graph destructured
  (:carried_out_by (map (fn ((?slug ?label)) (person slug label))
                        (("allan" "Scott Allan") ("beeny" "Emily Beeny")))
   :label          (label-of (:type "Type" :_label "Beauty"))))
```

...compiles to:

``` json
{
  "carried_out_by": [
    {
      "id": "https://linked.art/example/person/allan",
      "type": "Person",
      "_label": "Scott Allan"
    },
    {
      "id": "https://linked.art/example/person/beeny",
      "type": "Person",
      "_label": "Emily Beeny"
    }
  ],
  "label": "Beauty"
}
```

- A pattern is one parameter: `(fn ((?a ?b)) ...)` takes one argument. Names and patterns can be mixed, as in `(defn sum (base (?a ?b)) ...)`.
- Any `case` pattern can be written: Array and Record patterns, `?..rest` holes, and values compared by value, as in `(fn (("Person" ?l)) l)`. A pattern must bind something.
- The argument is matched when the function is called, even if the body uses none of its holes. An argument that does not match is reported at the argument, or at the Array in `map`.
- A property before a pattern checks the argument first: `(fn (#arr (?a ?b)) ...)`.
- Each name is bound once: two parameters, two holes, or a hole and a parameter with the same name are an error.

## Syntax Sugar

Ouro offers some syntax sugar to make common Linked Art JSON-LD patterns a bit more ergonomic. An example of this is `thru`. `thru` handles the common pattern of setting a date/time occurrence to the last possible second i.e "1883-12-31T23:59:59Z". Instead of manually checking the correct months/days/minutes of a desired time in Ouro we can simply write `(thru (years 2))`. This allows us to make very human readable statements such as:

```clojure

 (graph syntax-sugar
   (:timespan (:type               "TimeSpan"
               :begin_of_the_begin #date "1881-01-01T00:00:00Z"
               :end_of_the_end     #date (+ begin_of_the_begin (thru (years 2))))))
```

# Lexical Scoping

Ouro uses Scheme style lexical scoping, this means that scope is defined by the structure of the source code, and not where it is located on the runtime callstack (Ouro does not have a runtime). When a function is evaluated, it looks up variables based on where its was defined, never where it was called.  If you want know what a variable evaluates to, you simply read outward through the nested blocks `()` in the source code. 

``` clojure
;; --- Arguments mean what they meant at the call site --------------------------
;; 'person' binds its own 'name'. The argument 'who' is still evaluated where
;; the call is written, so it is never captured by the function's 'name'.

(defn person (slug who)
  (let ($name "a name private to person"
        $base (+ (linked-art) "person/"))
    (:id     #uri (+ base slug)
     :type   "Person"
     :_label who)))
```

# Modules

A file uses another's functions by importing it, and offers its own by exporting them. Both are written first in the file:

``` clojure
(import std/base)                     ; base/core, base/name
(import linked-art/v1/people as la)   ; la/person
(export place named part-of)
```

- An imported function is always written with a qualifier, `qualifier/name`: the path's last part (`base`, `people`), or the name given after `as`. A file's own functions are written without one, so a local `name` and `base/name` are different functions.
- A file has one `(export f g ...)`, naming its own functions. Only functions cross files, never graphs or values, so modules hold no state.
- `(import a/b)` reads `a/b.ouro` beside the file being compiled. Each module is read, checked and desugared on its own, loaded once however often it is imported, and its functions close over its own module, never the importer.
- Modules cannot import each other in a circle (`SCP0009`): Ouro has no recursion, across files as within one.
- An error inside an imported module is reported, and shown, in that module's file and line.

`std` is a folder of modules bundled with the compiler, so they need no file. They hold Linked Art's own patterns, and each mirrors a section of [Linked Art's model](https://linked.art/model/), so `provenance/acquisition` is what linked.art/model/provenance describes. Importing a `std/` module says that its functions build Linked Art's nodes. There is one so far, `std/base`, the base patterns: `base/core` (a node's `id`, `type` and `_label`) and `base/name` (a Name). More will follow (`std/event`, `std/object`, `std/actor`, `std/place`, `std/provenance`, `std/exhibition`, `std/digital`, and the required vocabulary in `std/vocab`). `(import std)` on its own is an error that lists `std`'s modules. A module of the project with a bundled module's name is imported in its place, with a warning (`WRN0005`), so a project can replace one on purpose.

Module paths are written with `/` on every platform, Windows included.

# Canonical Example

*Idea for Manet and Modern Beauty*
``` clojure
(import std/base)

;; Not yet in std: Linked Art patterns kept here until std grows
(defn aat #rec (#str id #str label)
  (base/core #uri (+ "http://vocab.getty.edu/aat/" id) "Type" label))

(defn ulan #rec (#str id #str label)
  (base/core #uri (+ "http://vocab.getty.edu/ulan/" id) "Person" label))

(defn classified #rec (#rec r #rec t)
  (with r :classified_as (list t)))

(defn example #uri (#str path)
  (+ "https://linked.art/example/" path))

(defn person #rec (#str slug #str label)
  (base/core (example (+ "person/" slug)) "Person" label))

;; Steps: each takes a record first and returns it extended, so they thread with ->
(defn named      #rec (#rec r #str n)
  (with r :identified_by ((:type          "Name"
                           :classified_as ((aat "300404670" "Primary Name"))
                           :content       n))))
(defn about      #rec (#rec r xs)     (with r :about xs))
(defn created-by #rec (#rec r people)
  (with r :created_by (:type           "Creation"
                       :carried_out_by (map (λ ((?slug ?label)) (person slug label)) people))))

(graph idea-for-manet-and-modern-beauty
  (-> (base/core (example "concept/exhidea/1") "PropositionalObject" "Idea for Manet and Modern Beauty")
      (with :context "https://linked.art/ns/v1/linked-art.json")
      (classified (aat "300417531" "Exhibition"))
      (named "Manet and Modern Beauty")
      (about ((aat "300055821" "Beauty")
              (ulan "500010363" "Manet")))
      (created-by (("allan" "Scott Allan")
                   ("beeny" "Emily Beeny")
                   ("groom" "Gloria Groom")))))
```

Which compiles into the following JSON-LD:

``` json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://linked.art/example/concept/exhidea/1",
  "type": "PropositionalObject",
  "_label": "Idea for Manet and Modern Beauty",
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300417531",
      "type": "Type",
      "_label": "Exhibition"
    }
  ],
  "identified_by": [
    {
      "type": "Name",
      "classified_as": [
        {
          "id": "http://vocab.getty.edu/aat/300404670",
          "type": "Type",
          "_label": "Primary Name"
        }
      ],
      "content": "Manet and Modern Beauty"
    }
  ],
  "about": [
    {
      "id": "http://vocab.getty.edu/aat/300055821",
      "type": "Type",
      "_label": "Beauty"
    },
    {
      "id": "http://vocab.getty.edu/ulan/500010363",
      "type": "Person",
      "_label": "Manet"
    }
  ],
  "created_by": {
    "type": "Creation",
    "carried_out_by": [
      {
        "id": "https://linked.art/example/person/allan",
        "type": "Person",
        "_label": "Scott Allan"
      },
      {
        "id": "https://linked.art/example/person/beeny",
        "type": "Person",
        "_label": "Emily Beeny"
      },
      {
        "id": "https://linked.art/example/person/groom",
        "type": "Person",
        "_label": "Gloria Groom"
      }
    ]
  }
}
```

# Installation

Ouro can either be installed via **Nix** as a `flake`, or by compiling with the **GHC** toolchain.

## Nix

1. Installing Nix Tutorial: https://nixos.org/download/
2. Enable Nix Flakes and Nix Commands: https://nixos.wiki/wiki/Flakes#Enable_flakes

The Ouro compiler is packaged with **Nix**. To try out Ouro without installation, you can simply use:
`nix run "github:protocol-oberon/ouro" -- [flags]`

## GHC

If you prefer a traditional Haskell environment, you can build Ouro directly from source using the GHC toolchain.

### Prerequisites

* **GHC**: We recommend using [GHCup](https://www.haskell.org/ghcup/) to manage your toolchain.
* **Cabal**: The standard Haskell build system.
* **Dependencies**: Ensure you have the necessary system-level libraries for Ouro's dependencies (e.g., `zlib`, `gmp`).

### Steps

1. **Clone the repository**:
   ```bash
   git clone https://github.com/protocol-oberon/ouro
   cd ouro
   ```
2. **Build the project**:
   ```bash
   cabal build
   ```
3. **Install to your path**:
   ```bash
   cabal install --install-method=copy --installdir=$HOME/.local/bin
   ```
4. **Verify installation**
   ```bash
   ouro --version
   ```
   
# Using the Compiler

The ouro compiler has the following commands (shown via running `ouro -h`):

``` bash
Ouro v0.0.0.1

Usage: ouro COMMAND [-v|--version]

Available options:
  -h,--help                Show this help text
  -v,--version             Show Ouro version

Available commands:
  compile                  Compile Ouro to JSON-LD
  validate                 Validate JSON-LD
```

The sub commands for both `compile` and `validate` can be found by using the `-h` flag once more (`ouro compile -h`):

``` bash
Usage: ouro compile SOURCE_FILE (-t|--trgt STRING) [-o|--output DIR]

  Compile Ouro to JSON-LD

Available options:
  -t,--trgt STRING         Target graph name within the module
  -o,--output DIR          Output directory
```

For compile, if the `-o` path has a file extension then that is the file the compiled JSON will be written to. Otherwise the path is a directory, and the name of the graph expression will be the name of the resulting JSON file in it. Without `-o`, the file is written to the current directory. For example, `ouro compile purchase.ouro -t purchase-of-spring -o out` writes `out/purchase-of-spring.json`.

A file's imports are read from the files beside it: compiling `places/amsterdam.ouro` with `(import people)` reads `places/people.ouro`. The bundled `std` modules need no file.

# Planned

These are agreed directions for Ouro, roughly in the order they will be built. None of them is implemented yet.

- **Properties:** user-defined properties, with the same standing as `#str`. A property is a function marked `#property` whose body is any Boolean expression. Called, it answers `true` or `false`; written before a value, `#titled x`, it asserts; and in a signature it is a requirement, whose failure is reported with blame, naming who broke it, as Racket's contracts do:

  ``` clojure
  (defn titled #property (#rec r)
    (has r _label))

  (defn exhibit #rec (#titled r) ...)
  ```

- **Builds:** `ouro build`, described by an optional `build.ouro`, which holds a single graph and nothing else: the source folders, where the output goes, and dependencies pinned to commits, as Nix pins them. A file compiles on its own without one.
- **A REPL and a language server,** bundled with the compiler, as `std` is, so that they run the same parser and evaluator as `compile`.
- **Search:** an Ouro Hoogle, finding functions by name, by signature and by their documentation.
- **Error codes:** codes generated from what each error is, so that a code alone says what happened, and the same mistake is always worded the same way.

