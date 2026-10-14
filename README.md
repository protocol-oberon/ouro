# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax

Ouro like any other lisp is anchored around the opening parentheses of an expression `()`. Its compiler uses a semi-complex lookahead to determine the target JSON-LD structure of incoming Ouro source code, weather it maps to an JSON-LD Array, Record or is an Ouro specific function application. Ouro also makes use of lisp key words to match the classic JSON *key* : *value* syntax (these are referred to as **Attributes**). This allows for an Ouro file's source code to roughly match the topology of JSON that it compiles to. 

A Ouro file consists of a collection of top-level, _higher expressions_. There are two types of higher expressions:

1. `(graph)`: Expressions then encode JSON-LD graphs
2. `(defn)`: Ouro supports _higher order functions_,  `(defn)` is a named function. `(fn (x y) expr)` is a lambda function. 

The target structure of Ouro code is determined by a few simple rules: 

1. An expression that contains a attribute pair `(:key value)` will compile to a JSON-LD **Object**.
2. An expression that contains only literals `(1 2 3)` will compile to a JSON-LD **List** of those literals.
3. An expression that contains only symbols `(sym expr expr)` will be treated as function application, where the first symbol in the expression is the function and the body of the expression are its arguments.

Through composition of these rules it is possible to ergonomically express any JSON-LD structure in Ouro with just parentheses. For example to make an nested list of objects the following syntax would be used `((:key val) (:key val))`. I

- Arrays in Ouro, like JSON arrays, may contain values of mixed types. Note that `(insert)` into an Array requires the inserted value to match the type of the Array's first element.

Ouro is 100% declarative, and lazily evaluated. This allows for the values of records to not only be referenced within the local scope of the expression, but also out of declaration order. To reference the value of a field, simply call the key without the colon, so `:lookup value` becomes `lookup`, which returns the `value`.

## Type assertions

In JSON-LD, there are many datatypes which can look similar to a compiler. To ensure that the compiled JSON from Ouro code is always well formed, we can use type assertions to tell the compiler how to treat the next expression it encounters. Type assertions are denoted by `#type (expr)`.

There are 7 type assertion tags:

| Tag          | Description                                                                              |
|:-------------|:-----------------------------------------------------------------------------------------|
| `#uri`       | Ensures that any expression that evaluates to a URI is properly formed.                  |
| `#date`      | Ensures valid ISO 8601 date-time formats and enables temporal desugaring (e.g., `thru`). |
| `#num`       | Asserts that the evaluated result is a numeric primitive.                                |
| `#str`       | Asserts that the evaluated result is a string literal.                                   |
| `#bool`      | Asserts that the evaluated result is a boolean primitive.                                |
| `#arr-empty` | Conveys to the compiler that the targeted array structure contains zero elements.        |
| `#rec-empty` | Conveys to the compiler that the targeted graph node contains no key-value attributes.   |

## Special forms

Ouro contains special forms designed for increased ergonomics for creating JSON-LD graphs. These forms interact with the JSON-LD graph in different ways.

### Let

In Ouro there are two ways to bind a name, with difference in serialisation.

| Form                           | Binds          | Output?                | Scope                         |
|--------------------------------+----------------+------------------------+-------------------------------|
| `(:name expr ...)` (a record)  | its fields     | yes, every field       | the record's own expressions  |
| `(let ($name expr ...) body)`  | its bindings   | no, only =body=        | the bindings and =body=       |

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

To add context to a JSON-LD record, the `(context)` special form is used. This form expects any number of Schema Directives:

1. `(remote-context uri)`: Imports an external JSON-LD context from the specified URI.
2. `(define-term term definition)`: Maps a local string term to a specific URI or complex term definition.
3. `(set-base uri)`: Defines the `@base` IRI against which relative document URIs are resolved.
4. `(set-vocab uri)`: Defines the default `@vocab` IRI used to resolve property and class names.
5. `(set-language lang)`: Sets the default `@language` tag (e.g., "en", "fr") for all string values in the scope.
6. `(clear-context)`: Nullifies the currently active context (evaluates to `@context: null`), effectively resetting the scope.

...and creates a `@context` sub-record in the current scope.

The `(context)` special form also has syntax sugar for the common case of having a single remote context. Using `:context value` will automatically desugar into an isolated schema directive frame.

```clojure
;; Surface Syntax Written by User
:context #uri "https://linked.art/ns/v1/linked-art.json"

;; Canonical Desugared Result Form
(context ((remote-context #uri "https://linked.art/ns/v1/linked-art.json")))
```

Which is then serialised to:

``` json
"@context": "https://linked.art/ns/v1/linked-art.json",
```

### Get


The `(get)` special form evaluates its target and walks the resulting records, so it works through function results, such as `(inlay)`, `(case)` and `(let)`. Only a record's fields can be reached, `(let)` bindings are private. Path components are bare symbols (`(get p q r)`, not `(get p :q)`). 

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
- `?..name` binds everything that is left: the remaining elements of an Array, or the fields of a Record that the pattern doesn't mention.
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

Array patterns match by position. The following:

 ``` clojure
 (graph case-statements
   :trgt (1 2 3)
   :res  (case trgt
               ((?head ?..rest) rest)
               (otherwise       trgt)))
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

Record patterns match by key, in any order, and fields the pattern doesn't mention are allowed, so `((:type "Person") ...)` matches any Person record. The following Ouro graph matches the `name` field without binding it, and returns the other fields:

``` clojure
 (graph case-statements
   :trgt (:name "Manet"
          :id   "https://linked.art/example/person/manet"
          :type "Person")
   :res  (case trgt
               ((:name ? ?..rest) rest)
               (otherwise         trgt)))
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

While the Ouro is statically typed and the compiler has enough type inference to tell potentially ambiguous forms apart, there are still cases where the developer intent must be made clear to the compiler. An example of this is the following expression `(id name type)`. The compiler will infer this as an expression where a function, `id` takes two arguments `name` and `type`.  However if the developer indents for this to be a list of values where the variables `id`, `name` and `type` are looked up, then they will have to use the `(list)` special form.

```clojure
(list id name type)
```

This form simply tells the compiler to evaluation every expression after a `list` and store them in-place. So the following Ouro code:

``` clojure
((define
  :name "Manet"
  :id   "https://linked.art/example/person/manet"
  :type "Person")

 :_list (list name id type))
```

...complies to the following JSON-LD:

``` json
{
  "_list" : [ "Manet", "https://linked.art/example/person/manet", "Person"]
}
```

### Nth

`(nth)` is a special form that takes a number and either a record for a list and returns the value of the element at the index of the given number. It is 0 indexed.

For example:

``` clojure
(:trgt    (1 2 3)
 :get_nth (nth 1 trgt))
```

...will compile to the following JSON:

``` json
{
  "trgt" : [1, 2, 3]
  "get_nth" : 2
}
```

In the case of the target being a record, then:

``` clojure
(:trgt    (:key1 "val0" :key2 "val1" :key3 "val2")
 :get_nth (nth 1 trgt))
```

..will compile to:

``` json
{
  "trgt" : {
    "key1" : "val0",
    "key2" : "val1",
    "key3" : "val2"
  },
  "get_nth" : "val1"
}
```

To get the head value of record or list, `(nth 0 trgt)` can be used. You can also access elements using negative indexing. `(nth -1 trgt)` will return the last element of the list, `(nth -2 trgt)` the second to last element and so forth.

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

### Insert

`(insert)` serves as the dual to `(inlay)`, while `(inlay)` is used to bring things into the current scope, `(insert)` takes either a Record, or an Array, and merges its contents into the current Record/Array, `(insert)` does the same from an outside perspective. It takes an **insert position**, the payload to be inserted and a **Record**/**Array** to insert into. As Ouro is a functional language, `(insert)` returns a new instance of the original record, with the values inserted rather than mutating in-place. In this sense, `(insert)` is not to dissimilar to *lenses* in Haskell.    

**Example**

The following Ouro code:

``` clojure
(:inserted_into (:id   "https://linked.art/example/person/proust"
             :type "Person")

 :record    (insert 2 (attr "_label" "Proust") inserted_into))
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

## Builtin Functions

Ouro has the following built in functions:

**Variadic Arithmetic & Temporal Shifting:** The arithmetic functions in Ouro are fully variadic and handle both standard mathematical computations and strict #date temporal adjustments.

| Function | Syntax Example                         | Behaviour / Domain Rules                                          |
|:---------|:---------------------------------------|:------------------------------------------------------------------|
| **`+`**  | `(+ 2 2 6)` -> `10`                    | Computes the sum of all numerical arguments.                      |
|          | `(+ #date "2026-06-14..." (years 1))`  | When provided a `#date` head, applies time windows forward.       |
| **`-`**  | `(- 20 5 2)` -> `13`                   | Sequentially subtracts values from the first argument.            |
|          | `(- #date "2026-06-14..." (months 2))` | When provided a `#date` head, rolls time windows backward.        |
| **`*`**  | `(* 2 3 4)` -> `24`                    | Computes the product of all numerical arguments.                  |
| **`/`**  | `(/ 100 2 5)` -> `10`                  | Sequentially divides the first argument by the subsequent values. |
|          |                                        |                                                                   |

**Relational Comparisons:** Relational operators evaluate numbers and dates. They support variadic chaining, verifying that the comparison holds true sequentially across every element in the expression.

| Function | Syntax Example | Evaluation Matrix                                 |
|:---------|:---------------|:--------------------------------------------------|
| **`>`**  | `(> 10 5 2)`   | Evaluates to `true` (Strictly decreasing stream). |
| **`>=`** | `(>= 10 10 5)` | Evaluates to `true` (Decreasing or equal stream). |
| **`<`**  | `(< 2 5 10)`   | Evaluates to `true` (Strictly increasing stream). |
| **`<=`** | `(<= 5 5 10)`  | Evaluates to `true` (Increasing or equal stream). |

## Functions

Like any good Lisp, Ouro supports powerful first order functions. Functions in Ouro return either a Record, Array or a Primitive. To return a Primitive from a function the `(return)` special form must be used.

```clojure
(defun add-one! (n)
  (return (+ 1 n)))
```

| Form                     | Meaning                                                             |
|--------------------------+---------------------------------------------------------------------|
| =(defn name (x y) expr)= | a named function; top level only; visible throughout the file       |
| =(fn (x y) expr)=        | an anonymous function value, closed over the scope it is written in |
| =(f a b)=                | a call: =f= is any expression that evaluates to a function          |

A local function is a =let=-bound =fn=:

 ``` clojure
(let (:attribute (fn (label) (+ label " (attributed)")))
  (:attributed (attribute (attribute "Manet"))))
```

Semantics of a call:

- *Arity* is checked: a call with the wrong number of arguments is an error.

- *Arguments are call-by-need:* each argument becomes a promise over the caller's scope. It is evaluated at most once, only if the function uses it, and always means what it meant at the call site, so a function's own bindings can never capture it.

- *The body is evaluated in the function's closure*, extended with a frame for the parameters. For a `defn` the closure is the module, for a `fn` it is the scope where the `fn` was written.

A function cannot see its caller's bindings.

- *Recursion is forbidden.* Calling a function that is already being called (identified by name and definition position) is an error. Nesting a call, `(f (f x))`, is two calls, not recursion.

- *Functions are values:* they can be passed as arguments and returned, but cannot be compared (`eq` on functions is a type error) or serialised.

A `defn` body is checked when the file is parsed: it must be one expression, cannot be a spliced record (`(defn f (x) :k v)`), and cannot contain or a nested `defn`.
     
## Syntax Sugar

Ouro offers some syntax sugar to make common Linked Art JSON-LD patterns a bit more ergonomic. An example of this is `thru`. `thru` handles the common pattern of setting a date/time occurrence to the last possible second i.e "1883-12-31T23:59:59Z". Instead of manually checking the correct months/days/minutes of a desired time in Ouro we can simply write `(thru (years 2))`. This allows us to make very human readable statements such as:

```clojure

 (graph syntax-sugar
   :timespan (:type               "TimeSpan"
              :begin_of_the_begin #date "1881-01-01T00:00:00Z"
              :end_of_the_end     #date (+ begin_of_the_begin (thru (years 2)))))
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

# Canonical Example

*Purchase of Spring by Proust*
``` clojure
(defn transfer! (id-type type-of-type name)
  (let ($getty      "http://vocab.getty.edu/aat/"
        $linked-art "https://linked.art/example/"
        $uri_type   (case id-type
                          (("manet" "proust") (+ linked-art "person/"))
                          ("spring"           (+ linked-art "object/"))
                          ("manet_proust/1"   (+ linked-art "provenance/"))
                          (otherwise          getty)))

    (:id     #uri (+ uri_type id-type)
     :type   type-of-type
     :_label name)))

(graph purchase-of-spring
 (:context "https://linked.art/ns/v1/linked-art.json"
  (inlay (transfer! "manet_proust/1" "Activity" "Purchase of Spring by Proust"))
  :classified_as ((transfer! "300055863" "Type" "Provenance Activity"))

  :identified_by ((:type          "Name"
                   :classified_as ((transfer! "300404670" "Type" "Primary Name"))
                   :content       "Purchase of Spring by Proust from Manet"))

  :timespan (:type               "TimeSpan"
             :begin_of_the_begin #date "1881-01-01T00:00:00Z"
             :end_of_the_end     #date (+ begin_of_the_begin (thru (years 2))))

  ;; A List of objects in Ouro is made from nested parens `(())`
  :part ((:type   "Acquisition"
          :_label "Ownership of Spring to Proust"
          :transferred_title_of   ((transfer! "spring" "HumanMadeObject" "Spring"))
          :transferred_title_from ((transfer! "manet"  "Person"          "Manet"))
          :transferred_title_to   ((transfer! "proust" "Person"          "Proust")))

         (:type        "Payment"
          :_label      "3000 Francs to Manet"
          :paid_amount (:type      "MonetaryAmount"
                        :value     3000
                        :currency  (transfer! "300412016" "Currency" "French Francs"))
         :paid_from    ((transfer! "proust" "Person" "Proust"))
         :paid_to      ((transfer! "manet"  "Person" "Manet"))))))
```

Which compiles into the following JSON-LD:

``` json
{
  "@context": "https://linked.art/ns/v1/linked-art.json",
  "id": "https://linked.art/example/provenance/manet_proust/1",
  "type": "Activity",
  "_label": "Purchase of Spring by Proust",
  "classified_as": [
    {
      "id": "http://vocab.getty.edu/aat/300055863",
      "type": "Type",
      "_label": "Provenance Activity"
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
      "content": "Purchase of Spring by Proust from Manet"
    }
  ],
  "timespan": {
    "type": "TimeSpan",
    "begin_of_the_begin": "1881-01-01T00:00:00Z",
    "end_of_the_end": "1883-12-31T23:59:59Z"
  },
  "part": [
    {
      "type": "Acquisition",
      "_label": "Ownership of Spring to Proust",
      "transferred_title_of": [
        {
          "id": "https://linked.art/example/object/spring",
          "type": "HumanMadeObject",
          "_label": "Spring"
        }
      ],
      "transferred_title_from": [
        {
          "id": "https://linked.art/example/person/manet",
          "type": "Person",
          "_label": "Manet"
        }
      ],
      "transferred_title_to": [
        {
          "id": "https://linked.art/example/person/proust",
          "type": "Person",
          "_label": "Proust"
        }
      ]
    },
    {
      "type": "Payment",
      "_label": "3000 Francs to Manet",
      "paid_amount": {
        "type": "MonetaryAmount",
        "value": 3000,
        "currency": {
          "id": "http://vocab.getty.edu/aat/300412016",
          "type": "Currency",
          "_label": "French Francs"
        }
      },
      "paid_from": [
        {
          "id": "https://linked.art/example/person/proust",
          "type": "Person",
          "_label": "Proust"
        }
      ],
      "paid_to": [
        {
          "id": "https://linked.art/example/person/manet",
          "type": "Person",
          "_label": "Manet"
        }
      ]
    }
  ]
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

The ouro compile as the following commands (shown via running `ouro -h`):

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

For compile if you supply a fully filepath then that is what the compiled JSON will be written to, otherwise the name of the graph expression will be the name of the resulting JSON file in the specified output directory.
