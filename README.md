# Ouro

Ouro is a domain-specific language for writing JSON-LD knowledge graphs, such as CIDOC-CRM's in Linked Art. Its source has the shape of the JSON it produces, so a record reads like the record it compiles to, and its standard library, `std`, holds Linked Art's own patterns: the required terms, time spans, provenance, and more.

Compiled Ouro is always well-formed JSON. A graph compiles only when every value in it has a JSON form and no error is left anywhere in it. URIs and dates are checked wherever they are marked, and a record's `@context` is read as a JSON-LD context. The same source always compiles to the same output. Ouro does not check a record's terms against a vocabulary; that is for properties. `std`'s functions write Linked Art's terms for you, and because a compile is deterministic, a record that is right once stays right.

# Syntax

Ouro, like any other Lisp, is anchored around the opening parenthesis of an expression, `()`. Its compiler uses a lookahead to determine the target JSON-LD structure of incoming Ouro source code, whether it maps to a JSON-LD Array, Record or is an Ouro specific function application. Ouro also makes use of Lisp keywords to match the classic JSON *key* : *value* syntax (these are referred to as **Attributes**). This allows for an Ouro file's source code to roughly match the topology of JSON that it compiles to.

An Ouro file consists of a collection of top-level, _higher expressions_. There are three types of higher expressions:

1. `(import path)` and `(export name ...)`: the modules a file uses and the functions it offers, written first in the file (see [Modules](#modules)).
2. `(graph name expr)`: Expressions that encode JSON-LD graphs. A graph is one expression, usually a record.
3. `(define/procedure name (args) expr)`: Ouro supports _higher order functions_: `define/procedure` names a function, and `(fn (x y) expr)` is a lambda function. The older `defn`, `defn/const` and `defn/property` are errors that name their replacement.

The target structure of Ouro code is determined by a few simple rules, which only look at an expression's direct elements, never at their types:

1. An expression whose first element is a symbol `(sym expr expr)` is a function application, where the symbol is the function and the rest of the expression are its arguments.
2. Otherwise, an expression that contains an attribute pair `(:key value)` will compile to a JSON-LD **Object**.
3. Anything else, such as `(1 2 3)` or `()`, will compile to a JSON-LD **Array**.

Through composition of these rules it is possible to ergonomically express any JSON-LD structure in Ouro with just parentheses. For example, to make an array of records the following syntax would be used `((:key val) (:key val))`.

- Arrays in Ouro, like JSON arrays, may contain values of mixed types, `(1 "a" (:k 1))`. `(insert)` can put any value into an Array, but not a `:key value` attribute pair.

Ouro is 100% declarative, and lazily evaluated. This allows for the values of records to not only be referenced within the local scope of the expression, but also out of declaration order. To reference the value of an attribute pair, simply call the key without the colon, so `:lookup value` becomes `lookup`, which returns the `value`.

Each key can appear once in a record: writing a key twice is an error. An attribute pair never sees itself, so in `(:label label)` the `label` being looked up is the one from the enclosing scope. This is what lets a template simply write `(define/procedure node (id type) (:id id :type type))`.

A key always has a value: in `(:type :_label "Spring")`, `:type` has none, and the compile stops there (`SYN0011`) rather than lose the pair. A key can be a compact IRI, `(:crm:P2_has_type "x")`, and is referred to the same way, `crm:P2_has_type`.

Ouro has no `null`. Absence is never a value: a key is left out, by how the record is built, or the absence is stated as data, with its reason, such as a search that found nothing. `null` is an error wherever it is written, and is no name either; a name that only begins with it, `null-result`, is a name.

Source files are UTF-8 on every platform, so strings, names and keys can hold any text: `(:_label "Édouard Manet")` compiles to `"_label": "Édouard Manet"` on Windows as well as Linux. A file saved in another encoding is reported, not garbled.

## Built-in properties

In JSON-LD, there are many datatypes which can look similar to a compiler. To ensure that the compiled JSON from Ouro code is always well formed, we can give an expression a property, which tells the compiler how to treat it. A property is written before the expression, as in `#string (expr)`.

There are 8 built-in properties:

| Property        | Description                                                                              |
|:----------------|:-----------------------------------------------------------------------------------------|
| `#uri`          | Ensures that any expression that evaluates to a URI is properly formed.                  |
| `#date`         | Ensures valid ISO 8601 date-time formats and enables temporal desugaring (e.g., `thru`). |
| `#number`       | Asserts that the evaluated result is a numeric primitive.                                |
| `#string`       | Asserts that the evaluated result is a string.                                           |
| `#boolean`      | Asserts that the evaluated result is a boolean primitive.                                |
| `#record`       | Asserts that the evaluated result is a Record.                                           |
| `#array`        | Asserts that the evaluated result is an Array.                                           |
| `#function`     | Asserts that the evaluated result is a function (a `fn`, a `define/procedure` name or a built-in).   |

A value without the property it is given is an error, such as `Expected data to have the property #number, got #string instead.` The same properties type a function's parameters and result (see [Functions](#functions)).

Two further `#` forms are values rather than properties, written in place of an empty collection:

| Value           | Description                                                                              |
|:----------------|:-----------------------------------------------------------------------------------------|
| `#array-empty`  | An empty Array, `[]` (the same as `()`).                                                 |
| `#record-empty` | An empty Record, `{}`.                                                                   |

## Special forms

Ouro contains special forms designed for increased ergonomics for creating JSON-LD graphs. These forms interact with the JSON-LD graph in different ways.

Their names are not reserved. As in Scheme, a binding of the same name, whether an attribute key, a `let` binding, a parameter or a `define/procedure`, shadows the form where it is in scope, and the form is then a call of the binding: `(let ($list (fn (x) (+ x 1))) (list 1))` is `2`. The forms rewritten before any name is bound, `and`, `or`, `->`, `with`, `adjoin` and `thru`, are the exception, for now: a binding of their names is ignored where they are called.

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

A record's JSON-LD context is written `:context value`. The value is an ordinary expression, written as JSON-LD writes a context: the URL of a remote context, a local context record, or an array of them, applied in order. Most Linked Art documents need only the remote context:

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

- A key starting with `@` is a JSON-LD keyword: `(:@vocab "https://example.org/" :@language "en")` is a local context. Any other key defines a term, which maps to an IRI or to an expanded definition record (`:@id`, `:@type`, `:@container`, ...).
- A context is a value like any other, so a function can build one: `(define/constant linked-art "https://linked.art/ns/v1/linked-art.json")`, then `:context (list linked-art (:ex "https://example.org/ns/"))`.
- A malformed context is reported where it is written: a remote context that is not an absolute URL, an unknown `@` keyword, or a term mapped to a Number, for example.
- `:@context` means the same as `:context`. Ouro has no `null`, so a context cannot clear another, and no keyword or term is set to `null`; a record without `:context` has no `@context`.

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

`(has record key ...)` is `true` when the path reaches a value, and `(get-or record default key ...)` is the value at the path, or `default` when a key on the way is missing or an index is past the end:

``` clojure
(let ($p (:classified_as ((:_label "Painting") (:_label "Oil")) :xs (1 2 3)))
  (:first (get p classified_as 0 _label)                  ; "Painting"
   :last  (get p xs -1)                                   ; 3
   :has   (has p classified_as 5)                         ; false
   :or    (get-or p "none" classified_as 5 _label)))      ; "none"
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
(map (0 7 "x" true)
     (fn (v) (case v
               ((when (eq v 0))     "zero")
               ((satisfies #number) "a number")
               ((⊨ #string)         "a string")
               (otherwise           "other"))))
```

...is `["zero", "a number", "a string", "other"]`. Outside a branch, `(satisfies x #p)` is a question with a Boolean answer, x ∈ #p, while `#p x` is an assertion that fails. The value comes first in the question and the property first in the assertion, so the two never look alike; in a branch head the case's target is put first, as it is for a guard.

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

`(map xs f)` calls the function `f` on each element of the Array `xs`, and returns an Array of the results, in order. The Array comes first, as every form takes what it works on first, so `->` threads through `map`. `f` can be a `define/procedure`, a `fn`, a builtin or a [placeholder](#placeholders), so most uses need no `fn` at all:

``` clojure
(define/procedure aat-type (id label)
  (:id #uri (+ "http://vocab.getty.edu/aat/" id) :type "Type" :_label label))

(graph mapped
  (:classified_as (map ("300033618" "300177435") (aat-type ? "Painting"))
   :counts        (map (1 2 3) (+ ? 1))))
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

Each element is passed to `f` as an ordinary call, so `f` must take exactly one argument, and its signature checks every element: a mismatch is reported at the Array. Mapping over an empty Array gives an empty Array, and mapping over anything other than an Array is an error. `map` cannot be the name of a `define/procedure`, but `:map` is an ordinary key and `$map` an ordinary binding.

When the elements are themselves Arrays or Records, a [pattern parameter](#destructuring-parameters) takes each one apart: `(map pairs (fn ((?slug ?label)) (person slug label)))`.

### Fold

`(fold xs init f)` reduces an Array to one value: it calls `f` on the running value and each element in turn, starting from `init`, so `(fold (1 2 3) 0 f)` is `(f (f (f 0 1) 2) 3)`. As with `map`, the Array comes first; `f` takes the running value first, then the element. Folding an empty Array gives `init`.

``` clojure
(fold weights 0 +)                                      ; the sum of weights
(fold parts (:type "Set") (fn (set p) (adjoin set :member p)))
(-> parts (map (get ? _label)) (fold "" (+ ? ?)))       ; a step like any other
```

Every built-in takes a fixed number of arguments, and many values are an Array: `+` adds two numbers, and a sum of many is a fold.

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
(define/constant base (:type "Type" :_label "Painting"))

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
(import std/base)

(graph amsterdam
  (-> (:id #uri "https://linked.art/example/place/amsterdam/1" :type "Place" :_label "Amsterdam")
      (with :context       "https://linked.art/ns/v1/linked-art.json")
      (with :classified_as (list (base/aat "300008389" "City")))
      (with :identified_by ((:type "Name" :content "Amsterdam")))))
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
- `(-> ? step ...)` is a function of the value, so a pipeline can be mapped: `(map records (-> ? (with :seen true)))`.
- `with` extends an Array too: `(with (1 2) 3 4)` is `(1 2 3 4)`.
- `with` writes a value as it is given, so a property that holds a set is written as an array, `(list (base/aat ...))`, as Linked Art writes it.

### Adjoin

`(adjoin base :key value ...)` is `with`'s dual: the same shape, in `->` and under placeholders alike, but where `with` sets a key, `adjoin` adds to the key's set of values, as JSON-LD reads a property. So `with` suits a property that holds one value (`_label`, `produced_by`), and `adjoin` one that holds a set (`classified_as`, `identified_by`). Use `with` for a property's first value, and `adjoin` to add to it:

```clojure
(import std/base)
(import std/vocab)

(graph spring
  (-> (:type "HumanMadeObject" :classified_as (list vocab/artwork))
      (adjoin :classified_as (base/work-type "300033618" "Painting"))
      (adjoin :classified_as vocab/artwork)))
```

...compiles to:

``` json
{
  "type": "HumanMadeObject",
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300133025",
      "type": "Type",
      "_label": "Artwork"
    },
    {
      "id": "http://vocab.getty.edu/aat/300033618",
      "type": "Type",
      "_label": "Painting",
      "classified_as": [
        {
          "id": "http://vocab.getty.edu/aat/300435443",
          "type": "Type",
          "_label": "Type of Work"
        }
      ]
    }
  ]
}
```

- A value already there is not added again, compared as `eq` compares, by value; the values already there are kept as they are. A key that is added to always holds an Array.
- A list adds its elements: `(adjoin r :k (list a b))`. An Array base adds the elements not there: `(adjoin (1 2) 2 3)` is `(1 2 3)`.
- `with` after `adjoin` sets the key; `adjoin` after `with` adds to it.

### Insert

`(insert)` serves as the dual to `(inlay)`, while `(inlay)` is used to bring things into the current scope, `(insert)` takes either a Record, or an Array, and merges its contents into the current Record/Array, `(insert)` does the same from an outside perspective. It takes the **Record**/**Array** to insert into, an **insert position**, and the payload to be inserted, the collection first as in a path. As Ouro is a functional language, `(insert)` returns a new instance of the original record, with the values inserted rather than mutating in-place. In this sense, `(insert)` is not to dissimilar to *lenses* in Haskell.    

**Example**

The following Ouro code:

``` clojure
(:inserted_into (:id   "https://linked.art/example/person/proust"
             :type "Person")

 :record    (insert inserted_into 2 :_label "Proust"))
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

### And, Or, All, Any and Quantifiers

`(and a b)` is `true` when both operands are, and `(or a b)` when at least one is. For more than two, `(all (a b c))` is `true` when every condition is, and `(any (a b c))` when one is: like every built-in, `and` and `or` take a fixed number of arguments, and many values are an Array. All four stop at the operand that decides the answer, so later operands are never evaluated:

``` clojure
(graph tags
  (let ($tags ())
    (:featured (and (> (len tags) 0) (eq (get tags 0) "featured"))    ; false: get is never reached
     :listed   (or  (eq (len tags) 0) (eq (get tags 0) "listed")))))  ; true

(define/property labelled (#record r)
  (doc "It and every part, at any depth, have a label.")
  (all ((has r _label)                                  ; a property's rules, as a list
        (forall (get-or r () part) labelled))))
```

- Each operand that is reached must be a Boolean: `(and true 1)` is an error, `Expected data to have the property #boolean, got #number instead.` Ouro has no truthiness.
- `all` and `any` stop early over an Array written in place, a literal or a `(list ...)`; an Array given another way, a binding or a `map`'s result, is evaluated whole and then read. `(all ())` is `true`, and `(any ())` is `false`.
- `(and a)` or `(and a b c)` is an error that suggests `all`.
- They take holes like a call, `(map (true false) (or ? false))`, and are threading steps, `(-> true (and false))`.

The quantifiers ask the same of every element of an Array. `(forall xs f)` is `true` when `f` answers `true` for every element, ∀x ∈ xs. f(x), `(exists xs f)` when it does for at least one, and `(omits xs f)` when it does for none. The Array is the domain, so it comes first. Each stops at the element that decides. `(filter xs f)` keeps the elements `f` answers `true` for:

``` clojure
(let ($present ("leaf marker" "clasp")
      $absent  ("endband"))
  (:consistent (omits present (contains absent ?))        ; true: nothing is both present and absent
   :any-clasp  (exists present (= ? "clasp"))             ; true
   :long       (filter present (fn (s) (> (len s) 6)))))  ; ("leaf marker")
```

`omits` is named for what the data omits, not for what does not exist: a record's silence is not an observation, so `omits` says only that the Array has no such element.

### Symbols

The connectives, the quantifiers and the set operations can be written with the symbols of logic and sets. Each is read as the name it stands for, so `(∧ a b)` is `(and a b)`, and messages use the name:

| Symbol | Name           | Symbol | Name            |
|:-------|:---------------|:-------|:----------------|
| `¬`    | `not`          | `∪`    | `union`         |
| `∧`    | `and`          | `∩`    | `intersect`     |
| `∨`    | `or`           | `∖`    | `setminus`      |
| `⋀`    | `all`          | `⊆`    | `subset`        |
| `⋁`    | `any`          | `⋃`    | `unions`        |
| `∀`    | `forall`       | `⋂`    | `intersections` |
| `∃`    | `exists`       | `=`    | `eq`            |
| `∄`    | `omits`        | `/=`   | `neq`           |
| `⊨`    | `satisfies`    | `λ`    | `fn`            |

``` clojure
(define/procedure consistent #boolean (#record book)
  (∄ (present book) (contains (absent book) ?)))
```

## Builtin Functions

Ouro has the following built in functions:

**Arithmetic & Temporal Shifting:** The arithmetic functions take two operands, as every built-in takes a fixed number of arguments, and handle both standard mathematical computations and strict #date temporal adjustments. Many values are an Array, folded: `(fold (2 2 6) 0 +)` is `10`.

| Function | Syntax Example                         | Behaviour / Domain Rules                                          |
|:---------|:---------------------------------------|:------------------------------------------------------------------|
| **`+`**  | `(+ 2 6)` -> `8`                       | Adds two numbers.                                                 |
|          | `(+ #date "2026-06-14..." (years 1))`  | When provided a `#date` head, applies time windows forward.       |
|          | `(+ "Type: " label)`                   | Joins strings.                                                    |
|          | `(+ (1 2) (3))` -> `(1 2 3)`           | Concatenates arrays.                                              |
| **`-`**  | `(- 20 5)` -> `15`                     | Subtracts the second from the first.                              |
|          | `(- #date "2026-06-14..." (months 2))` | When provided a `#date` head, rolls time windows backward.        |
| **`*`**  | `(* 3 4)` -> `12`                      | Multiplies two numbers.                                           |
| **`/`**  | `(/ 100 4)` -> `25`                    | Divides the first by the second.                                  |

Time windows are made with `(years n)`, `(months n)`, `(days n)`, `(hours n)`, `(minutes n)` and `(seconds n)`. Shifting by months or years clips to the end of a shorter month instead of rolling over into the next one, so 31 March minus one month is the last day of February.

**Relational Comparisons:** Relational operators compare two numbers. A chain is `all` over its pairs: `(all ((< 0 x) (< x 10)))`.

| Function  | Syntax Example | Evaluation Matrix                                 |
|:----------|:---------------|:--------------------------------------------------|
| **`>`**   | `(> 10 5)`     | Evaluates to `true` (Strictly greater).           |
| **`>=`**  | `(>= 10 10)`   | Evaluates to `true` (Greater or equal).           |
| **`<`**   | `(< 2 5)`      | Evaluates to `true` (Strictly less).              |
| **`<=`**  | `(<= 5 5)`     | Evaluates to `true` (Less or equal).              |
| **`eq`**  | `(eq 1 1)`     | Evaluates to `true` (The values are equal).       |
| **`neq`** | `(neq 1 2)`    | Evaluates to `true` (The values differ).          |

`eq` compares records and arrays by value, and `=` and `/=` are `eq` and `neq`. Using an operator with any number of operands but two is an arity error, whose hint is the fold.

**Smallest and largest:** `min` and `max` take one Array of Numbers, so a summary is worked out from the data rather than typed beside it. Several Numbers are written as one Array.

| Function  | Syntax Example                  | Behaviour / Domain Rules                            |
|:----------|:--------------------------------|:----------------------------------------------------|
| **`min`** | `(min (3.72 3.36 4.19))` -> `3.36` | The smallest Number of an Array.                 |
| **`max`** | `(max weights)`                 | The largest; an empty Array is an error.            |

**Logic:** `not` negates a Boolean: `(not false)` is `true`. `and`, `or`, `all` and `any` are special forms, described above.

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
| **`union`**         | `(union (1 2) (2 3))` -> `(1 2 3)`       | Every value in either Array.                    |
| **`intersect`**     | `(intersect (1 2 3) (2 3 4))` -> `(2 3)` | The values in both.                             |
| **`setminus`**      | `(setminus (1 2 3) (2))` -> `(1 3)`      | The values of the first not in the second.      |
| **`subset`**        | `(subset (1 2) (1 2 3))` -> `true`       | Every value of the first is in the second.      |
| **`unions`**        | `(unions ((1) (1 2) (3)))` -> `(1 2 3)`  | The union of an Array of Arrays.                |
| **`intersections`** | `(intersections ((1 2) (2 3)))` -> `(2)` | The intersection of an Array of Arrays.         |

**Strings:** each takes the String it works on first, so `->` threads through them: `(-> label (trim) (upper-case))`.

| Function          | Syntax Example                               | Behaviour / Domain Rules                                    |
|:------------------|:---------------------------------------------|:------------------------------------------------------------|
| **`to-string`**   | `(to-string 123456789)` -> `"123456789"`     | A Number, Boolean, URI or Date as text, as the output writes it. |
| **`split`**       | `(split "Manet, Monet" ", ")` -> `("Manet" "Monet")` | The pieces between each separator.                  |
| **`join`**        | `(join ("a" "b") ", ")` -> `"a, b"`          | An Array of Strings, with the separator between each.       |
| **`upper-case`**  | `(upper-case "dutch")` -> `"DUTCH"`          | Every letter upper case; `lower-case` lower.                |
| **`trim`**        | `(trim "  Manet ")` -> `"Manet"`             | The white space at both ends removed.                       |
| **`starts-with`** | `(starts-with id "https://")` -> `true`      | Whether it begins with the other String; `ends-with` ends.  |
| **`slice`**       | `(slice "C-0042" 2 6)` -> `"0042"`           | From start up to, not including, end; negative from the end. |
| **`pad-start`**   | `(pad-start "42" 4 "0")` -> `"0042"`         | Padded with one character to a width.                       |
| **`replace-all`** | `(replace-all "a b" " " "-")` -> `"a-b"`     | Every one replaced.                                         |

A Number is written as JavaScript writes one, in the output and by `to-string`: a whole number as an integer, `123456789`, and any other in plain decimal, `0.00001`.

**Building strings:** `+` joins two Strings, as it adds two Numbers. A String of more pieces is built with `->`, which threads the value in as each step's first argument, so every `(+ piece)` adds a piece to what has been built so far:

``` clojure
(-> "https://example.org/"
    (+ kind)
    (+ "/")
    (+ slug))                                       ; "https://example.org/place/amsterdam"

(-> n (to-string) (pad-start 4 "0") (+ "C-" ?))     ; 7 is "C-0007": a hole puts the value second
```

When the pieces are already an Array, thread it into a `fold` from `""`, or into `join` with a separator:

``` clojure
(-> (list "https://example.org/" kind "/" slug) (fold "" +))
(-> labels (map trim) (join ", "))                  ; "Manet, Proust"
```

Every String built-in takes its String first, so a value is built and cleaned in one pipeline.

**Ranges and `zip`:** `(start..end)` is the whole numbers from start to end, both included, as Haskell's `[1..5]`, and `(first second .. end)` counts in steps. `zip` pairs two Arrays of the same length.

| Syntax                     | Result                           | Behaviour / Domain Rules                                  |
|:---------------------------|:---------------------------------|:----------------------------------------------------------|
| `(1..5)`                   | `(1 2 3 4 5)`                    | Each end is an expression: `(1..(len xs))`.               |
| `(5..1)`                   | `()`                             | An end below the start gives nothing.                     |
| `(2 4 .. 10)`              | `(2 4 6 8 10)`                   | Steps of the difference; `(5 4 .. 1)` counts down.        |
| `(zip (1 2) ("a" "b"))`    | `((1 "a") (2 "b"))`              | Arrays of different lengths are an error, not cut short.  |

Together they number a list without writing the numbers, and a summary comes from the same data:

```clojure
(define/constant weights #array
  (3.72 3.36 4.19))

(graph coins
  (:coins (map (zip (1..(len weights)) weights)
               (fn ((?n ?grams))
                 (:id    (+ "https://example.org/coin/C-" (pad-start (to-string n) 4 "0"))
                  :grams grams)))
   :range (-> (to-string (min weights)) (+ " to ") (+ (to-string (max weights))) (+ " g"))))
```

## Functions

Like any good Lisp, Ouro supports powerful first class functions. A function's body is one expression, written exactly as it would be anywhere else, and its value is the result: a Record, an Array or a Primitive.

```clojure
(define/procedure add-one (n)
  (+ 1 n))
```

| Form                     | Meaning                                                             |
|:-------------------------|:--------------------------------------------------------------------|
| `(define/procedure name (x y) expr)` | a named function; top level only; visible throughout the file       |
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
(define/procedure type-node (aat-id label)
  (let ($prefix "Type: ")
    (:id     #uri (+ "http://vocab.getty.edu/aat/" aat-id)
     :type   "Type"
     :_label (+ prefix label))))
```

Because parentheses around values make an Array, `(fn () ("hello"))` returns `["hello"]`, not `"hello"`. The compiler warns about this (`WRN0002`) when a result is a single plain value in parentheses.

Semantics of a call:

- *Arity* is checked: a call with the wrong number of arguments is an error. A call written with a function's name is checked where it is written, before anything is evaluated, so in a function no graph calls too; a call through a value is checked when it is made. Functions are not curried.

- *Arguments are call-by-need:* each argument becomes a promise over the caller's scope. It is evaluated at most once, when the function first uses it, and always means what it meant at the call site, so a function's own bindings can never capture it. An argument the function never uses is still checked for errors.

- *The body is evaluated in the function's closure*, extended with a frame for the parameters. For a `define/procedure` the closure is the module, for a `fn` it is the scope where the `fn` was written. A function cannot see its caller's bindings.

- *Recursion goes down into a value.* A function may call itself, directly or through other functions, only if each time round it passes on a part of one of its parameters, so it finishes: values are finite, so taking parts of one reaches its leaves. A part is a path into a parameter, `(get n part)`, a hole of a pattern matched against it, or an element of it given to `map`, `exists`, `forall`, `omits` or `filter`:

  ``` clojure
  (define/procedure outline (n)
    (:label (get n _label)
     :parts (map (get-or n () part) outline)))
  ```

  A recursion that might not finish is an error wherever it is written, whether or not a graph calls it: `(define/procedure f (n) (f n))` is reported as `'f' calls itself, and might never finish`. The check is size-change termination (Lee, Jones and Ben-Amram, 2001), and it errs on the safe side: a number counting down is not a part, and neither is `(get-or n () part)` passed straight on, which may be its fallback each time. Finishing is not speed: a function that calls itself twice on the same part finishes, but in time exponential in the value's depth.

  A function reached through a value, as in `(w w)`, is checked when it is called: calling it while it is already being called is an error. Nesting a call, `(f (f x))`, is two calls, not recursion.

- *Functions are values:* they can be passed as arguments and returned, but cannot be compared (`eq` on functions is a type error) or serialised.

- *A returned function is called through `let`.* An expression whose first element is a call is an Array, so `((f x) y)` does not call the result of `(f x)`. Bind it first: `(let ($g (f x)) (g y))`. A function can return a `fn` waiting for the remaining arguments (or a [placeholder](#placeholders), `(+ ? n)`):

  ``` clojure
  (define/procedure adder (n) (fn (x) (+ n x)))

  (graph partial
    (let ($inc (adder 1))
      (:r (inc 2))))
  ```

  ...compiles to `{"r": 3}`.

A `define/procedure` body is checked when the file is parsed: it must be one expression and cannot be a spliced record (`(define/procedure f (x) :k v)`). A nested definition in it is reported with its replacement, a `let`-bound `fn`.

A function is documented with a `(doc "...")` among the direct elements of its body, before or after its result. It is never evaluated and is not part of the result:

``` clojure
(define/procedure place #record (#string slug #string label)
  (doc "A Linked Art Place, under the example namespace.")
  (:id (example (+ "place/" slug)) :type "Place" :_label label))
```

A function of no parameters is a constant: named as a value, it is its result, so `vocab/artwork` is `(vocab/artwork)`. Ouro has no effects, so such a function cannot be told from its value; `(c)` still calls it. A constant cannot be passed on as a function, and an error in it is reported where it is defined. A constant's value is worked out the first time a graph names it, and reused after that.

`define/constant` writes a constant without the empty parameter list, and takes a return type and a `doc` as `define/procedure` does:

``` clojure
(define/constant museum-label #string
  (doc "How the museum is named in every record.")
  "Rijksmuseum")
```

Names:

- Functions and graphs are named in kebab-case, `make-place`. A name with a capital or an underscore is a warning (`WRN0004`) that suggests the kebab-case spelling. Attribute keys and local bindings are exempt, since keys follow the data's vocabulary (`:_label`).
- A `define/procedure` may take the name of a built-in or a special form, as in Scheme, and replaces it throughout the file, with a warning (`WRN0003`). The names read before any name is bound cannot be taken (`SYN0008`), since such a function could never be called: `define/procedure`, `define/property`, `define/constant`, `graph`, `doc`, `and`, `or`, `->`, `with`, `adjoin` and `thru`.
- A `/` qualifies an imported name, `base/aat`, so no function, parameter or `let` binding can have one. The operators `/` and `/=` are names like any other.

### Signatures

A function's types are written as built-in properties in its header. One between the name and the parameters is the return type, and one before a parameter is that parameter's type. Any of them can be left out.

``` clojure
(define/procedure node #record (#uri id type)
  (:id id :type type))

(graph typed
  (let ($double (fn #number (#number n) (* n 2)))
    (:r (double 21)
     :n (node "https://linked.art/example/object/spring" "HumanMadeObject"))))
```

- A property in a signature behaves exactly as it would anywhere else: it checks the value, and `#uri` and `#date` convert a string. In `node` above, `id` arrives as a URI.
- Each argument is checked where the call is written, so a mismatch is reported at the argument, in the call. The result is checked where the body is.
- An argument is checked even if the function never uses it.
- Nothing is inferred: an untyped parameter or result is not checked.
- A return type of `#array` says that a one-value Array is intended, so `(define/constant f #array ("x"))` does not warn.
- A `define/procedure` of no parameters is a constant, so it warns (`WRN0006`) and suggests `define/constant`.

### When signatures are checked

A signature is checked when a value crosses it, not before evaluation as a static type is, and a failed check is reported where the value came from: a bad argument at the call, a bad result at the function's body.

Since Ouro evaluates everything at compile time, the checks cost nothing in the output: every one has already run before the JSON-LD is written.

User-defined properties are described under [Planned](#planned).

### Placeholders

A call with `?` holes among its arguments is not called: it is a function of its holes. This fixes some arguments now and leaves the rest for later, without writing a `fn` that returns a `fn`:

``` clojure
(define/procedure node (base type id label)
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
- Any call takes holes: a `define/procedure`, a `fn`, a builtin, an imported function, and the special forms that evaluate their arguments (`get`, `image`, `insert`, `replace-at`, `list`, `map`). In `get` and `image` a hole stands for the target, never a key or an index.
- A placeholder is an ordinary `fn` value. It is called with exactly as many arguments as it has holes, and functions themselves stay uncurried.
- A hole anywhere else (in an Array, in a Record, as a `let` value) is an error, and `?..rest` can only be written in a pattern. The holes in a `case` pattern or a [pattern parameter](#destructuring-parameters) match values; a placeholder can be written in a branch's body.

### Destructuring parameters

A parameter can be a pattern, written as a `case` pattern is. It takes one argument and names its parts, so a lambda can take a pair or a Record apart where it is written:

``` clojure
(define/procedure person (slug label)
  (:id #uri (+ "https://linked.art/example/person/" slug) :type "Person" :_label label))

(define/procedure label-of ((:_label ?l)) l)

(graph destructured
  (:carried_out_by (map (("allan" "Scott Allan") ("beeny" "Emily Beeny"))
                        (fn ((?slug ?label)) (person slug label)))
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

- A pattern is one parameter: `(fn ((?a ?b)) ...)` takes one argument. Names and patterns can be mixed, as in `(define/procedure sum (base (?a ?b)) ...)`.
- Any `case` pattern can be written: Array and Record patterns, `?..rest` holes, and values compared by value, as in `(fn (("Person" ?l)) l)`. A pattern must bind something.
- The argument is matched when the function is called, even if the body uses none of its holes. An argument that does not match is reported at the argument, or at the Array in `map`.
- A property before a pattern checks the argument first: `(fn (#array (?a ?b)) ...)`.
- Each name is bound once: two parameters, two holes, or a hole and a parameter with the same name are an error.

## Syntax Sugar

Ouro offers some syntax sugar to make common Linked Art JSON-LD patterns a bit more ergonomic. An example of this is `thru`. `thru` handles the common pattern of setting a date/time occurrence to the last possible second i.e "1883-12-31T23:59:59Z". Instead of manually checking the correct months/days/minutes of a desired time in Ouro we can simply write `(thru (years 2))`. This allows us to make very human readable statements such as:

```clojure

 (graph syntax-sugar
   (:timespan (:type               "TimeSpan"
               :begin_of_the_begin #date "1881-01-01T00:00:00Z"
               :end_of_the_end     #date (+ begin_of_the_begin (thru (years 2))))))
```

## Time spans

`std/time` builds Linked Art's time spans from a date written to the precision it is known at, so the last second, the length of a month and leap years are worked out for you:

```clojure
(import std/time)

(graph time-spans
  (:year      (time/year "1881")
   :years     (time/year-range "1881" "1883")
   :day       (time/day "1832-01-23")
   :uncertain (time/between (time/day-range "1848-08-01" "1848-08-20")
                            (time/day-range "1848-09-09" "1848-09-30"))))
```

`(time/year-range "1881" "1883")` runs from "1881-01-01T00:00:00Z" to "1883-12-31T23:59:59Z", and the uncertain span began some time between the 1st and the 20th of August and ended between the 9th and the 30th of September, setting all four of a TimeSpan's ends.

# Lexical Scoping

Ouro uses Scheme style lexical scoping, this means that scope is defined by the structure of the source code, and not where it is located on the runtime callstack (Ouro does not have a runtime). When a function is evaluated, it looks up variables based on where its was defined, never where it was called.  If you want know what a variable evaluates to, you simply read outward through the nested blocks `()` in the source code. 

``` clojure
;; --- Arguments mean what they meant at the call site --------------------------
;; 'person' binds its own 'name'. The argument 'who' is still evaluated where
;; the call is written, so it is never captured by the function's 'name'.

(define/procedure person (slug who)
  (let ($name "a name private to person"
        $base (+ (linked-art) "person/"))
    (:id     #uri (+ base slug)
     :type   "Person"
     :_label who)))
```

Every name is resolved as it is written, before anything is evaluated, so an unbound name is an error even in a function no graph calls or a `case` branch that is not taken. Its "Perhaps you meant" suggestion is ranked over every name in scope where it is written, local bindings included.

# Modules

A file uses another's functions by importing it, and offers its own by exporting them. Both are written first in the file:

``` clojure
(import std/base)                     ; base/aat, base/name, ...
(import linked-art/v1/people as la)   ; la/person
(export place named part-of)
```

- An imported function is always written with a qualifier, `qualifier/name`: the path's last part (`base`, `people`), or the name given after `as`. A file's own functions are written without one, so a local `name` and `base/name` are different functions. No import is qualified `define` or `defn`, since its functions would read as the definition keywords, `define/constant`, or their retired forms, `defn/const`: name such a module with `as`.
- A file has one `(export f g ...)`, naming its own functions. Only functions cross files, never graphs or values, so modules hold no state.
- `(import a/b)` reads `a/b.ouro` under the project's source folder (see [Builds](#builds)), or, when `a` names a library in `vendor/`, `b.ouro` from that library (see [Libraries](#libraries)). Each module is read, checked and desugared on its own, loaded once however often it is imported, and its functions close over its own module, never the importer.

- Modules cannot import each other in a circle (`SCP0009`): imports go one way, so a module is checked before the modules that import it.

- An error inside an imported module is reported, and shown, in that module's file and line.

`std` is a folder of modules bundled with the compiler, so they need no file. They hold Linked Art's own patterns, and each mirrors a section of [Linked Art's model](https://linked.art/model/), so `provenance/acquisition` is what linked.art/model/provenance describes. Importing a `std/` module says that its functions build Linked Art's nodes. Each function takes values and builds a node; none extends a record, which is written as a literal or threaded through `with` and `adjoin`:

| Module           | Holds                                                                                                                 |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------|
| `std/vocab`      | the 18 terms Linked Art requires, as constants: `vocab/primary-name`, `vocab/artwork`, ...                             |
| `std/base`       | Getty entities by number (`aat`, `tgn`, `ulan`, `language`); types with their meta-type (`work-type`, `nationality`, ...); `name`, `identifier`, `statement`, `description` |
| `std/time`       | time spans, whose end is included: `span`, `between` (an uncertain span), `day`, `day-range`, `month`, `year`, `year-range` |
| `std/object`     | dimensions (`height`, `width`, `depth`, `weight`), units, `material`, dimension and material statements, `part-type`  |
| `std/actor`      | name parts, `gender`, `active` (a professional life), contact points                                                   |
| `std/provenance` | `purchase` (an Acquisition and a Payment, their roles reversed), `acquisition`, `payment`, `money`, currencies        |
| `std/exhibition` | `exhibiting`, `concept` and `set`                                                                                      |
| `std/digital`    | `picture`, an image for `representation`, and `web-page`, for `subject_of`                                             |

For example, `(base/name vocab/primary-name "Spring")` is a *Primary Name**, and `(time/year "1881")` runs from its first second to 23:59:59 on 31 December. `(import std)` on its own is an error that lists `std`'s modules. An import that starts with `std` or `aat` always means the bundled module, so `std/base` means the same in every project; a module of the project under a `std` or `aat` folder is an error (`BLD0009`).

`aat` is a second folder of bundled modules: the terms Linked Art [recommends](https://linked.art/model/vocab/recommended/), one module per section of its list, 195 terms in all. Each is a constant, built with `std`'s pattern for its kind, so a nationality is classified as Nationality and an object type as Type of Work, as Linked Art requires:

``` clojure
(import aat/nationality)   ; nationality/dutch
(import aat/language)      ; language/dutch
(import aat/object-type)   ; object-type/painting
(import aat/unit)          ; unit/centimeters
```

The modules are `name-type`, `identifier-type`, `statement-type`, `object-type`, `place-type`, `group-type`, `digital-type`, `set-type`, `dimension-type`, `language`, `unit`, `material`, `currency`, `nationality`, `occupation` and `shape`. A term is named as Linked Art names it, and labelled as Linked Art describes it, capitalised: `(occupation/collector)` is "Art Collector". Seven names are taken: the six time units are `unit/year-unit`, `unit/month-unit` and so on, since `years` and its kin are built-ins, and Map is `object-type/map-object`, since a `map` would shadow the special form throughout its module. The modules are generated from Linked Art's list by `scripts/aat/generate.pl`, never written by hand.

Module paths are written with `/` on every platform, Windows included.

# Canonical Example

*Idea for Manet and Modern Beauty*
``` clojure
(import std/base)
(import std/vocab)
(import std/exhibition)

(define/procedure example #uri (#string path)
  (+ "https://linked.art/example/" path))

(define/procedure person #record (#string slug #string label)
  (:id (example (+ "person/" slug)) :type "Person" :_label label))

;; std/exhibition builds the concept, classified as an Exhibition; the steps add the rest
(graph idea-for-manet-and-modern-beauty
  (-> (exhibition/concept (example "concept/exhidea/1") "Idea for Manet and Modern Beauty")
      (with :context       "https://linked.art/ns/v1/linked-art.json")
      (with :identified_by (list (base/name vocab/primary-name "Manet and Modern Beauty")))
      (with :about         (list (base/aat "300055821" "Beauty")
                                 (base/ulan "500010363" "Person" "Manet")))
      ;; A pattern parameter takes each pair apart
      (with :created_by    (:type           "Creation"
                            :carried_out_by (map (("allan" "Scott Allan")
                                                  ("beeny" "Emily Beeny")
                                                  ("groom" "Gloria Groom"))
                                                 (λ ((?slug ?label)) (person slug label)))))))
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
Ouro v0.6.2.0

Usage: ouro COMMAND [-v|--version]

Available options:
  -h,--help                Show this help text
  -v,--version             Show Ouro version

Available commands:
  build                    Build the graphs of the project in a folder, with the
                           settings of its package.ouro, if it has one
  validate                 Validate JSON-LD
```

The options of each command are shown with the `-h` flag once more (`ouro build -h`):

``` bash
Usage: ouro build [DIR] [-g|--graph MODULE/GRAPH]

  Build the graphs of the project in a folder, with the settings of its
  package.ouro, if it has one

Available options:
  DIR                      The project's folder, the current one when not given
  -g,--graph MODULE/GRAPH  A graph to build, as people/person; may be given more
                           than once, and replaces :graphs
  -h,--help                Show this help text
```

## Builds

Ouro's build system, or lack thereof, is heavily inspired by the programming language *Odin*.

Like Odin, `ouro build` is how Ouro is compiled. A folder of `.ouro` files is a project as it is, with no file to describe it: `ouro build` builds every graph of every module in the current folder, or in the folder given, `ouro build christies`, to `out/<module>/<graph>.json`. A module is named by its path, written with `/`: the graph `purchase-of-spring` in `provenance/spring.ouro` is written to `out/provenance/spring/purchase-of-spring.json`. If any graph fails, nothing is written, and every error is reported.

`-g` builds only the graphs it names, by module and name, and may be given more than once:

``` bash
ouro build -g provenance/spring/purchase-of-spring
```

Only the modules of the graphs named, and what they import, are compiled, so a module not chosen cannot fail the build. A graph its module does not have is an error that suggests the module's closest graph.

A `package.ouro` in the project's folder changes the defaults. It holds one function, `package`, whose value is the project's settings, none of them required:

``` clojure
(define/constant package
  (:sources "src"
   :outputs "dist"
   :graphs  ("provenance/spring/purchase-of-spring")
   :ouro    "0.6"))
```

- `:sources` is the folder the modules are in, `"."` when not given; `:outputs` is where the graphs are written, `"out"` when not given.

- `:graphs` chooses the graphs built, as `-g` does, every one when not given; `-g` replaces it.

- `:ouro` is the version of Ouro the project needs: a newer one than the compiler's is an error that names both.

- The settings are Ouro values, so `(+ "di" "st")` is a folder name too. A setting `package.ouro` does not have is an error that suggests the nearest, `:sources` for `:sorces`. Besides its function, `package.ouro` can import only the bundled modules, and holds nothing else.

- The folder is the project: `ouro build` never looks above it, and a folder that does not exist is a message, not the build of a project above it.

- Hidden folders (`.git`, `.jj`), the outputs folder, `vendor/`, and any folder with a `package.ouro` of its own are skipped, but every other `.ouro` file is built, so a scratch file under the source folder is built too.

## Libraries

A library is used by copying it into `vendor/`, in the project's folder, beside `package.ouro`: each folder of `vendor/` is a library, named after its folder, and is imported by that name, `(import tate/people)`. There is no other place for libraries, and nothing is fetched.

- A library's own imports are read from the library, so it works unchanged once copied in: its `people` and the project's `people` are different modules.
- A library that needs another imports it by the other's name, so the libraries a library needs are vendored beside it, in the project's `vendor/`; a library with a `vendor/` of its own is an error.
- A library's modules are compiled only when imported, and its graphs are never written.
- A library cannot be named `std` or `aat`, nor share its name with a folder or module of the project, so `(import tate/…)` never has two meanings.
- An import spelt in another case than its file, `(import People)` for `people.ouro`, is an error everywhere, since a file system that ignores case would find it and one that does not would not.

# Planned

These are agreed directions for Ouro, roughly in the order they will be built. None of them is implemented yet.

- **Properties:** user-defined properties, with the same standing as `#string`. A property is a function written with `define/property`, whose body is any Boolean expression. Called, it answers `true` or `false`; written before a value, `#titled x`, it asserts; and in a signature it is a requirement, whose failure is reported with blame, naming who broke it, as Racket's contracts do:

  ``` clojure
  (define/property titled (#record r)
    (has r _label))

  (define/procedure exhibit #record (#titled r) ...)
  ```

- **Types written as sets** (0.7): every type, `string` as much as `titled`, is a set in one tree of refinements rooted at `data`, and a refinement has one name with no `#`. Properties are renamed refinements, defined with `define/refinement`, so that "property" keeps the meaning it has in RDF and Linked Art, a key of a record. `⊢` (`assert`) asserts that a value is in a set, a type error with blame when it is not; `∈` (`in`) asks, answering `true` or `false`, in place of `satisfies`. Types combine as sets do: `(∪ string uri)`, `(∖ record titled)`, `(array titled)` for an array of titled records, `(record :label string)` for a record shape. Angle brackets write any form infix, `<a op b>` being `(op a b)`, so a signature reads as its sets:

  ``` clojure
  (define/procedure person-name ⊢ name-tree (<given ⊢ string> <family ⊢ string>)
    (:type "Name"
     :part ((:type "Name" :content given)
            (:type "Name" :content family))))
  ```

  ``` clojure
  (define/refinement titled <r ⊢ record>
    (has r _label))
  ```

  With it, `(list …)` becomes `(array …)`, as the value is an Array everywhere else, and comparisons become words, `lt`, `leq`, `gt` and `geq`, freeing `<` and `>`. Each old spelling becomes an error that names its replacement.

- **The rest of the static analysis:** names, arity and recursion are already checked as written (see [Functions](#functions) and [Lexical Scoping](#lexical-scoping)). Still to come: warnings for unused bindings, imports and functions, once the error codes are redesigned; with properties, property names checked as written too, a recursive property under the same rule as any function; and whether an expression is a call, a record or an array decided there, once, for the evaluator to follow, by the same rules as now.
- **A REPL and a language server,** bundled with the compiler, as `std` is, so that they run the same parser and evaluator as `build`. The language server is where spelling belongs: a key or class that is not a term of the record's context can be underlined as it is written, with the nearest term offered, without ever stopping a compile.
- **Search:** an Ouro Hoogle, finding functions by name, by signature and by their documentation.
- **Geometry** (on hold): a `#wkt` property for the WKT string of a place's `defined_by`, and functions that build it from numbers, `(place/point 4.9 52.37)`, with `to-string`.
- **Error codes:** codes generated from what each error is, so that a code alone says what happened, and the same mistake is always worded the same way.
- **Prose:** a long string written over several lines, as a statement's text often is. `(prose lines)` takes an Array of Strings, trims each, and joins them with single spaces, so the text is written at any width and output as the one paragraph it is:

  ``` clojure
  (base/statement statement-type/biography-statement
    (prose ("A group of British painters formed in 1911; although it only lasted two years,"
            "its name is used in a broader sense to characterize a particular strain of British"
            "painting from about 1905 to 1920.")))
  ```

  A `prose` of written strings is the string it joins, so it can be written wherever a string can, a function's `doc` included: `(doc (prose (...)))`.

