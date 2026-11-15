# Fixtures

Each `test/fixtures/ouro/name.ouro` holds a graph named after its file, which compiles to
`test/fixtures/json/name.json`. Ouro's test suite checks that each does, and `cabal bench` times
them.

## Linked Art's examples

Most of the `.json` files in this folder are the examples of [Linked Art](https://linked.art/),
as published at `https://linked.art/example/…`, one file for each example on the site's model
pages, named after the example's path (`object/nightwatch/1.json` is `object-nightwatch-1`).
They are © the Linked Art community and published under the
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) licence; see
<https://linked.art/> for the originals.

Their `.ouro` files write them in Ouro, made for Ouro's tests and benchmarks. Each file stands
alone, compiled with the bundled modules only, so each defines its own small constructors,
`example`, `object`, `person` and the like. A project would write those once, in a module of
its own, and import it.

### Changes made to them

The examples label some Getty terms otherwise than Linked Art's own list of recommended terms
does: AAT 300128207 is "Nation" in two examples and "Country" in the list. Ouro writes each
term once, with the list's label, so these files are changed to it. A term off the list that
the examples label two ways, as AAT 300054216, "Painting Technique" and "Painting", takes the
label Ouro's `aat` modules give it. Each change is a line of
`scripts/fixtures/labels.tsv`, the term's AAT number, the example's label and the list's, and
`scripts/fixtures/relabel.pl` applies them. Nothing else in them is changed.

## Ouro's own

`encoding` checks that text is read and written as UTF-8. The fixtures written for Ouro from
earlier Linked Art examples were removed, as linked.art publishes each of them.
