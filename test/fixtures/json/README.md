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

Their `.ouro` files write them in Ouro, made for Ouro's tests and benchmarks.

### Changes made to them

The examples label some Getty terms otherwise than Linked Art's own list of recommended terms
does: AAT 300128207 is "Nation" in two examples and "Country" in the list. Ouro writes each
term once, with the list's label, so these files are changed to it. Each change is a line of
`scripts/fixtures/labels.tsv`, the term's AAT number, the example's label and the list's, and
`scripts/fixtures/relabel.pl` applies them. Nothing else in them is changed.

## Ouro's own

`auction-of-stowe-house`, `bust-of-a-man`, `christie-ams`, `idea-for-manet-and-modern-beauty`,
`jeanne-spring`, `purchase-of-spring` and `the-night-watch` were written for Ouro, from earlier
Linked Art examples; `encoding` checks that text is read and written as UTF-8. Where linked.art
publishes the same example, its version is kept and Ouro's is removed.
