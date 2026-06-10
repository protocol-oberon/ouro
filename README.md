# Ouro

Ouro is a domain-specific language (DSL) engineered for the ergonomic construction, validation, and serialisation of CIDOC-CRM knowledge graphs. It is designed to bridge the gap between human-readable data-modelling and strict Semantic Web standards. Ouro's compiler mathematically guarantees that compiled Ouro code produces well-formed JSON-LD.

# Syntax 

*Jeanne (Spring) by Manet*
``` clojure
(:context "https://linked.art/ns/v1/linked-art.json"
  ;; Define Blocks are elided at compilation (local variables)
 (define
   :getty-uri      "http://vocab.getty.edu/aat/"
   :linked-art-uri "https://linked.art/example/")

 ;; Type assertions
 :id     #uri (+ linked-art-uri "object/spring/13")
 :type   "HumanMadeObject"
 :_label "Jeanne (Spring) by Manet"
 :classified_as ((:id     #uri (+ getty-uri "300033618")
                  :type   "Type"
                  :_label "Painting"
                  :classified_as ((:id     #uri (+ getty-uri "300435443")
                                   :type   "Type"
                                   :_label "Type of Work"))))
 :identified_by ((:type    "Name"
                  :content "Jeanne (Spring)"))
 :member_of     ((:id   #uri (+ linked-art-uri "set/exhset")
                  :type "Set")))
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
