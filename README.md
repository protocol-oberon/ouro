# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax

Ouro like any other lisp is anchored around the opening parentheses of an expression `()`. Its compiler uses a semi-complex lookahead to determine the target JSON-LD structure of incoming Ouro source code, weather it maps to an JSON-LD Array, Record or is an Ouro specific function application. Ouro also makes use of lisp key words to match the classic JSON *key* : *value* syntax (these are referred to as **Attributes**). This allows for an Ouro file's source code to roughly match the topology of JSON that it compiles to, further enhancing the homoiconity of the language.

A Ouro file consists of a collection of top-level, _higher expressions_. There are two types of higher expressions:

1. `(graph)`: Expressions then encode JSON-LD graphs
2. `(defun)`: Function expressions, in ouro the line between lisp macros and higher-order functions is blurred so this kind of expression can be thought of as a JSON-LD graph node with a hole that will be filled via the arguments. Functions in Ouro always return either an Array, Record, Primitive value or another function.

The target structure of Ouro code is determined by a few simple rules: 

1. An expression that contains a attribute pair `(:key value)` will compile to a JSON-LD **Object**.
2. An expression that contains only literals `(1 2 3)` will compile to a JSON-LD **List** of those literals.
3. An expression that contains only symbols `(sym expr expr)` will be treated as function application, where the first symbol in the expression is the function and the body of the expression are its arguments.

Through composition of these rules it is possible to ergonomically express any JSON-LD structure in Ouro with just parentheses. For example to make an nested list of objects the following syntax would be used `((:key val) (:key val))`. I

- It is important to note that Arrays in Ouro, unlike lists in JSON are strictly **Homogeneous**, making a mixed array will cause a type error.

Ouro is 100% declarative, and uses defered evaluation during compilation. This allows for the values of records to not only be referenced within the local scope of the expression, but also out of declaration order. To reference the value of a field, simply call the key without the colon, so `:lookup value` becomes `lookup`, which returns the `value`.

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

Ouro contains special forms designed for increased ergonomics for creating JSON-LD graphs. These forms interact with the JSON-LD graph in deferent ways.

### Define

`(define)` is a special form in Ouro which injects the attribute pairs defined within, into the current lexical scope (scope in Ouro is the current record an expression is written in), but erases them at compile time from the serialised JSON out. This allows for local variables to be easily defined without pollution the resulting JSON-LD graph.

``` clojure
 (define
   :auction-start   #date "1848-08-01T00:00:00Z"
   :auction-restart #date "1848-09-09T00:00:00Z"
   :getty           "http://vocab.getty.edu/aat/")
```

This `define` block defines two attributes of type `date` in `auction-start` and `auction-restart` as well as an un-typed string attribute in `getty`.

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
:context #uri "[https://linked.art/ns/v1/linked-art.json](https://linked.art/ns/v1/linked-art.json)"

;; Canonical Desugared Result Form
(context ((remote-context #uri "[https://linked.art/ns/v1/linked-art.json](https://linked.art/ns/v1/linked-art.json)")))

Which is then serialised to:

``` json
"@context": "https://linked.art/ns/v1/linked-art.json",
```
### Get

The `(get)` special form allows for you traverse deeply nested structures and return target values. It takes a list of attribute keys to lookup.

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

It is also possible to retrieve the un-evaluated AST of a deeply nested symbol using `get'`. Given a record

```clojure
(:trgt   (:nest (:nest_2 (+ 2 2)))
 :quoted (get' nest nest2))
```

### Case

The `(case)` special form serves as the Ouro's main mode of control flow, it expects a target expr `(case <trgt>)` followed by a list of tuples, (pattern value).

``` clojure
(:trgt   99
 :result (case trgt
               ((= 100)   "fails")
               ((> 100)   "fails")
               (otherwise "fails")))
```

`case` statements must always contain a default branch `otherwise`, to ensure totality. When a case statement is triggered, the target expr is eagerly evaluated, and passed into the first element of branch tuple of the branch as its first argument `((= trgt 100) ret1)`. If the resulting expression evaluates to `True`, then the second element of the branches tuple (the return value) is evaluated and returned.

If a quoted expression is passed into a `case` statement as the target, then structural pattern matching can be performed. When pattern matching, the pattern for a branch must also be quoted. For cases where the entirety of the targeted structure isn't relevant, a hole type (**?**) can be used.

```clojure
(:trgt   ((+ 2 3) (- 2 2))
 :result (case (quote trgt)
               ('((- ? ?) ?)  "fails")
               ('((+ ? ?) ?)  "matched")
               (otherwise     "default")))
```

Here we are pattern matching on a list of expressions and checking to see if the operation of each element in the list is addition. This block resolves to the following just before compilation:

```clojure
(:trgt   (5 0)
 :result "matched")
```

...which can then be serialised.

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

`(case)` can also be used to de-structure and pattern match on **quoted** Arrays and Records, the following:

 ``` clojure
 (graph case-statements
   :trgt (1 2 3)
   :res  (case (quote trgt)
               ('(?head ?..rest) rest)
               (otherwise        trgt)))
 ```
 
... matches the head of the list, and binds it the variable `head`, which is the returned. This uses the syntax `?..` to indicate to the compiler to de-structure. This graph compiles to:

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

...similarly the following Ouro graph:

``` clojure
 (graph case-statements
   :trgt (:name "Manet"
          :id   "https://linked.art/example/person/manet"
          :type "Person")
   :res  (case (quote trgt)
               ('(?head ?..rest) rest)
               (otherwise        trgt)))
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

### Quote

The `(quote)` special form is used to return un-evaluated AST nodes of the argument expression.

1. `(quote (+ 2 3))` returns `'(+ 2 3)` without evaluation, it programmatically the same as just `'(+ 2 3)`.
2. `(quote symbol)` will lookup the symbol in the environment and return the AST bound to it un-evaluated.

### Eval

The dual of `(quote)`, `(eval)` takes a quoted expression and simply evaluates it.

``` clojure
(eval '(+ 2 3))
```

Evaluates to 5.

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

To make a function in Ouro you use the keyword `(defun name! (args) body)` followed by the name of the function. The name of each function macro must always end in a **!**, otherwise the compiler will throw a syntax error. This is to allow for user defined function symbols to remain easily identifiable.

An example of a simple function is the following:

``` clojure
(defun transfer! (id-type type-of-type name)
  (define
    :getty      "http://vocab.getty.edu/aat/"
    :linked-art "https://linked.art/example/"
    :uri_type   (case id-type
                      (("manet" "proust") (+ linked-art "person/"))
                      ("spring"           (+ linked-art "object/"))
                      ("manet_proust/1"   (+ linked-art "provenance/"))
                      (otherwise          getty)))

  :id     #uri (+ uri_type id-type)
  :type   type-of-type
  :_label name)
```

...which takes 3 arguments; `id-type`, `type-of-type` and `name` all of which are strings. If the wrong number of arguments are supplied then the compiler will fail with an arity error. This function pattern matches on the incoming name argument and produces a valid `uri`, which is type asserted within the function, ensuring that arguments of the correct type are supplied. 

Ultimately functions in Ouro can be seen a JSON-LD graph nodes with a hole in them, where said hole is filled by the passed in arguments.

### Meta-programming Functions 

Functions can also support quoted arguments and defer evaluation of said arguments until an arbitrary point within the function, allowing for the powerful meta-programming abilities Lisps are synonymous with.By default, functions will eagerly evaluate their arguments at the call site before passing them in. To defer this, you use the standard quote '(...) to pass the argument as raw, un-evaluated data (an AST node). 

- If the variable needs to be looked up before passing into the functions then the special form `(quote)` can be used. 
- If the variable is deeply nested then `get'` can also be used to return the quoted expression. 

The function can then decide if, when, and how to execute that data using the (eval) special form. This deferred evaluation is incredibly useful for creating conditional logic or custom control structures that need to guarantee certain JSON-LD outputs aren't prematurely evaluated or injected into the graph.

Consider the following Ouro code:

``` clojure
;; Global default configuration

(defun local-sandbox! ()
  ;; Shadowing the global environment variable strictly inside this block
  (define :environment "development")
  :function-env environment) ;; => "development"

 (defun meta-sandbox! (fn)
    (inlay (eval fn))
    :desc "the function arg was eval'd and inlayed")

;; Anywhere else in the file, 'environment' still evaluates to "production"
(graph test
  (define :environment "production")
  :global-env   environment
  (inlay (local-sandbox!))
  ;; This meta function takes a function, calls it and inlays it in the return Record
  :meta-function (meta-sandbox! (quote local-sandbox!)))
```

Here the function `meta-sandbox!` takes a quoted function as an argument, evaluates it, and returns that functions content inlay-ed in a Record. This code compiles to the following JSON:

``` json
{
  "global-env": "production",
  "function-env": "development",
  "meta-function": {
    "function-env": "development",
    "desc": "the function arg was eval'd and inlayed"
  }
}
```

This gives developers the tools to write their own data pipelines in user-space, ensuring that the final JSON-LD is both semantically correct and mathematically guaranteed by the compiler.

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

This kind of scoping allows for closures (see `(defun)` for a good example), as the functions remembers the environment that it was created in, scope blocks can be used to create private data, without the need for classes. 

``` clojure
(defun transfer! (id-type type-of-type name)
  (define
    :getty      "http://vocab.getty.edu/aat/"
    :linked-art "https://linked.art/example/"
    :uri_type   (case id-type
                      (("manet" "proust") (+ linked-art "person/"))
                      ("spring"           (+ linked-art "object/"))
                      ("manet_proust/1"   (+ linked-art "provenance/"))
                      (otherwise          getty)))

  :id     #uri (+ uri_type id-type)
  :type   type-of-type
  :_label name)
```
- In this example, the variables defined at the top (`getty`, `linked-art`, and `uri_type`) are completely private to the functions  internal scope. They are safely encapsulated to help construct the final yielded record (`:id`, `:type`, `:_label`) but will never leak into or conflict with the global scope.

## Shadowing

Since Ouro resolves scopes from the inside out, it allows for variable shadowing. In Ouro everything besides the core structural primitives (special forms) can be shadowed. Shadowing allows you to establish broad defaults globally, while giving specific functions the freedom to override those defaults locally without leaking changes back out to the rest of the system.

The following Ouro code:

``` clojure
;; Global default configuration

(defun local-sandbox! ()
  ;; Shadowing the global environment variable strictly inside this block
  (define :environment "development")
  :function-env environment) ;; => "development"

;; Anywhere else in the file, 'environment' still evaluates to "production"
(graph shadowing 
  (define :environment "production")
  :global-env   environment
  (inlay (local-sandbox!)))
```

...compiles to:

``` json
{
  "global-env": "production",
  "function-env": "development"
}
```

# Canonical Example

*Purchase of Spring by Proust*
``` clojure
(defun transfer! (id-type type-of-type name)
  (define
    :getty      "http://vocab.getty.edu/aat/"
    :linked-art "https://linked.art/example/"
    :uri_type   (case id-type
                      (("manet" "proust") (+ linked-art "person/"))
                      ("spring"           (+ linked-art "object/"))
                      ("manet_proust/1"   (+ linked-art "provenance/"))
                      (otherwise          getty)))

  :id     #uri (+ uri_type id-type)
  :type   type-of-type
  :_label name)

(graph purchase-of-spring
 :context "https://linked.art/ns/v1/linked-art.json"
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
        :paid_to      ((transfer! "manet"  "Person" "Manet")))))
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
   git clone [https://github.com/protocol-oberon/ouro](https://github.com/protocol-oberon/ouro)
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
  compile                  Compile JSON-LD to Ouro
  validate                 Validate JSON-LD
```

The sub commands for both `compile` and `validate` can be found by using the `-h` flag once more (`ouro compile -h`):

``` bash
Usage: ouro compile SOURCE_FILE (-t|--trgt STRING) [-o|--output DIR]

  Compile JSON-LD to Ouro

Available options:
  -t,--trgt STRING         Target graph name within the module
  -o,--output DIR          Output directory
```

For compile if you supply a fully filepath then that is what the compiled JSON will be written to, otherwise the name of the graph expression will be the name of the resulting JSON file in the specified output directory.
