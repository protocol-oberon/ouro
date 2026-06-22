# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax

Expressions in Ouro are formed around lisp *parentheses* and *keywords* (referred to as attributes). It is also highly contextual, allowing for the syntax to remain sparse. It also allows for Ouro code's topology to roughly match that of its output JSON, further enhancing the homoiconity of the language.

- Lisp keywords `:keyword value` are used to create JSON key-value pairs.
- Parens `()` followed by a keyword `:keyword` creates a JSON Record `(:record-key value)`.
- Parens followed by another set of parentheses creates a JSON array of Records `((:array-record inner-value))`.
- Parens wrapped around raw data constants or expressions automatically evaluate to a JSON Array `(1 2 3)`. This looks ahead recursively to naturally support multi-dimensional matrices like `((1 2) (3 4))` without requiring dedicated brackets or boilerplate.
- To reference the value of a field, simply call the key without the colon, so `:lookup value` becomes `lookup`.

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

## Builtin Functions

Ouro has the following built in functions:

**Variadic Arithmetic & Temporal Shifting:** The arithmetic functions in Ouro are fully variadic and handle both standard mathematical computations and strict #date temporal adjustments.

| Function | Syntax Example                         | Behaviour / Domain Rules                                           |
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

## Syntax Sugar



## Canonical Example

*Purchase of Spring by Proust*
``` clojure
((template transfer! (id-type type-of-type name)
   (define
     :uri_type (case name
                     ((eq "Manet")   linked-art-person)
                     ((eq "Proust")  linked-art-person)
                     ((eq "Spring")  linked-art-object)
                     (otherwise      getty)))

   :id     #uri (+ uri_type id-type)
   :type   type-of-type
   :_label name)

 (define
   :getty             "http://vocab.getty.edu/aat/"
   :linked-art-object "https://linked.art/example/object/"
   :linked-art-person "https://linked.art/example/person/")

 :context  "https://linked.art/ns/v1/linked-art.json"
 :id       #uri "https://linked.art/example/provenance/manet_proust/1"
 :type     "Activity"
 :_label   "Purchase of Spring by Proust"

 :classified_as ((transfer! "300055863" "Type" "Provenance Activity"))

 :identified_by ((:type          "Name"
                  :classified_as ((transfer! "300404670" "Type" "Primary Name"))
                  :content       "Purchase of Spring by Proust from Manet"))

 :timespan      (:type "TimeSpan"
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
