# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax

Ouro like any other lisp is anchored around the opening parentheses of an expression `()`. Its compiler uses a lookahead to determine the target JSON-LD structure of incoming Ouro source code, whether it maps to a JSON-LD Array, Record or is an Ouro specific function application. Ouro also makes use of lisp key words to match the classic JSON *key* : *value* syntax (these are referred to as **Attributes**). This allows for an Ouro file's source code to roughly match the topology of JSON that it compiles to. 

A Ouro file consists of a collection of top-level, _higher expressions_. There are two types of higher expressions:

1. `(graph name expr)`: Expressions that encode JSON-LD graphs. A graph is one expression, usually a record.
2. `(defn name (args) expr)`: Ouro supports _higher order functions_,  `(defn)` is a named function. `(fn (x y) expr)` is a lambda function. 

The target structure of Ouro code is determined by a few simple rules, which only look at an expression's direct elements, never at their types:

1. An expression whose first element is a symbol `(sym expr expr)` is a function application, where the symbol is the function and the rest of the expression are its arguments.
2. Otherwise, an expression that contains an attribute pair `(:key value)` will compile to a JSON-LD **Object**.
3. Anything else, such as `(1 2 3)` or `()`, will compile to a JSON-LD **Array**.

Through composition of these rules it is possible to ergonomically express any JSON-LD structure in Ouro with just parentheses. For example to make an nested list of records the following syntax would be used `((:key val) (:key val))`.

- Arrays in Ouro, like JSON arrays, may contain values of mixed types, `(1 "a" (:k 1))`. `(insert)` can put any value into an Array, but not a `:key value` attribute pair.

Ouro is 100% declarative, and lazily evaluated. This allows for the values of records to not only be referenced within the local scope of the expression, but also out of declaration order. To reference the value of an attribute pair, simply call the key without the colon, so `:lookup value` becomes `lookup`, which returns the `value`.

Each key can appear once in a record: writing a key twice is an error. An attribute pair never sees itself, so in `(:label label)` the `label` being looked up is the one from the enclosing scope. This is what lets a template simply write `(defn node (id type) (:id id :type type))`.

Source files are UTF-8 on every platform, so strings, names and keys can hold any text: `(:_label "Édouard Manet")` compiles to `"_label": "Édouard Manet"` on Windows as well as Linux. A file saved in another encoding is reported, not garbled.

## Type assertions

In JSON-LD, there are many datatypes which can look similar to a compiler. To ensure that the compiled JSON from Ouro code is always well formed, we can use type assertions to tell the compiler how to treat the next expression it encounters. Type assertions are denoted by `#type (expr)`.

There are 8 type assertion tags:

| Tag          | Description                                                                              |
|:-------------|:-----------------------------------------------------------------------------------------|
| `#uri`       | Ensures that any expression that evaluates to a URI is properly formed.                  |
| `#date`      | Ensures valid ISO 8601 date-time formats and enables temporal desugaring (e.g., `thru`). |
| `#num`       | Asserts that the evaluated result is a numeric primitive.                                |
| `#str`       | Asserts that the evaluated result is a string.                                           |
| `#bool`      | Asserts that the evaluated result is a boolean primitive.                                |
| `#rec`       | Asserts that the evaluated result is a Record.                                           |
| `#arr`       | Asserts that the evaluated result is an Array.                                           |
| `#fn`        | Asserts that the evaluated result is a function (a `fn`, a `defn` name or a built-in).   |

The same tags are used to type a function's parameters and result (see [Functions](#functions)).

Two further tags are values rather than assertions, written in place of an empty collection:

| Tag          | Description                                                                              |
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


The `(get)` special form evaluates its target and walks the resulting records, so it works through function results, such as `(inlay)`, `(case)` and `(let)`. Only a record's attribute pairs can be reached, `(let)` bindings are private. Path components are bare symbols (`(get p q r)`, not `(get p :q)`). 

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
  "trgt" : [1, 2, 3],
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

A record can't end up with a key twice, so inserting an attribute pair whose key already exists is an error. To change an existing attribute pair or element, use `(replace-at)`, which takes the same arguments and replaces whatever is at the index. Negative indexes count from the end in `(insert)` and `(replace-at)`, as they do in `(nth)`.

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

Using an operator with the wrong number of operands is an arity error.

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

### Signatures

A function's types are written as type assertion tags in its header. A tag between the name and the parameters is the return type, and a tag before a parameter is that parameter's type. Any of them can be left out.

``` clojure
(defn node #rec (#uri id type)
  (:id id :type type))

(graph typed
  (let ($double (fn #num (#num n) (* n 2)))
    (:r (double 21)
     :n (node "https://linked.art/example/object/spring" "HumanMadeObject"))))
```

- A signature's tag behaves exactly as the same tag would anywhere else: it checks the value, and `#uri` and `#date` convert a string. In `node` above, `id` arrives as a URI.
- Each argument is checked where the call is written, so a mismatch is reported at the argument, in the call. The result is checked where the body is.
- An argument is checked even if the function never uses it.
- Nothing is inferred: an untyped parameter or result is not checked.
- A return type of `#arr` says that a one-value Array is intended, so `(defn f #arr () ("x"))` does not warn.

### Contracts

Ouro's signatures are *contracts*, in the sense of Racket's contracts, rather than static types. A signature is a promise between two parties: the caller promises arguments of the right types, and the function promises a result of the right type. It is checked when values cross it, and when a check fails, the party that broke its promise is blamed: a bad argument is reported at the call, a bad result at the function's body.

Since Ouro evaluates everything at compile time, contracts cost nothing in the output: every check has already run before the JSON-LD is written.

Where contracts are going next is described under [Planned](#planned).

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
- A tagged hole is a typed parameter: `(node #uri ? "Type")` checks and converts its argument, as a signature would.
- A hole belongs to the call it is an argument of, so `(f (g ?))` passes `f` a function.
- Any call takes holes: a `defn`, a `fn`, a builtin, and the special forms that evaluate their arguments (`get`, `nth`, `insert`, `replace-at`, `list`, `map`).
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
- A tag before a pattern checks the argument first: `(fn (#arr (?a ?b)) ...)`.
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

# Canonical Example

*Purchase of Spring by Proust*
``` clojure
(defn core #rec (#str id #str type #str label)
  (let ($getty      "http://vocab.getty.edu/aat/"
        $linked-art "https://linked.art/example/"
        $base       (case id
                          (("manet" "proust") (+ linked-art "person/"))
                          ("spring"           (+ linked-art "object/"))
                          ("manet_proust/1"   (+ linked-art "provenance/"))
                          (otherwise          getty)))

    (:id     #uri (+ base id)
     :type   type
     :_label label)))

(graph purchase-of-spring
 (let ($aat-type (core ? "Type" ?)
       $person   (core ? "Person" ?))

  (:context "https://linked.art/ns/v1/linked-art.json"
   (inlay (core "manet_proust/1" "Activity" "Purchase of Spring by Proust"))
   :classified_as ((aat-type "300055863" "Provenance Activity"))

   :identified_by ((:type          "Name"
                    :classified_as ((aat-type "300404670" "Primary Name"))
                    :content       "Purchase of Spring by Proust from Manet"))

   :timespan (:type               "TimeSpan"
              :begin_of_the_begin #date "1881-01-01T00:00:00Z"
              :end_of_the_end     (+ begin_of_the_begin (thru (years 2))))

   ;; A list of records in Ouro is made from nested parens `(())`
   :part ((:type                   "Acquisition"
           :_label                 "Ownership of Spring to Proust"
           :transferred_title_of   ((core "spring" "HumanMadeObject" "Spring"))
           :transferred_title_from ((person "manet"  "Manet"))
           :transferred_title_to   ((person "proust" "Proust")))

          (:type        "Payment"
           :_label      "3000 Francs to Manet"
           :paid_amount (:type     "MonetaryAmount"
                         :value    3000
                         :currency (core "300412016" "Currency" "French Francs"))
           :paid_from   ((person "proust" "Proust"))
           :paid_to     ((person "manet"  "Manet")))))))
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

# Planned

These are agreed directions for Ouro, roughly in the order they will be built. None of them is implemented yet.

- **Imports and exports:** modules will import and export functions only, never graphs or data, so modules stay stateless and there can be no data cycles between files. Builds will be described in `build.ouro`, itself written in Ouro.
- **Contracts:** user-defined types that refine JSON's types. A contract is a function tagged `#contract` whose body is a `(case)`: its patterns describe a shape, and its branches can add checks of their own. A contract checks a value and passes it through unchanged, so `#person` can be used in a signature just like `#str`:

  ``` clojure
  (defn person #contract (#rec r)
    (case r
      ((:type "Person" :_label #str ?))))
  ```

