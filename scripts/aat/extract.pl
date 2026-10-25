# Extracts Linked Art's recommended terms from its page, linked.art/model/vocab/recommended,
# into a table: one "## Section" line per table, then one line per term, its name, its AAT
# number and its description, separated by tabs.
#
#   curl -sSL https://linked.art/model/vocab/recommended/ -o recommended.html
#   perl scripts/aat/extract.pl recommended.html > scripts/aat/recommended.tsv
use strict;
use warnings;

local $/;
my $page = <>;

while ($page =~ /<h3 id="[^"]+">([^<]*)<\/h3>(.*?)(?=<h[23] |\z)/gs) {
    my ($section, $body) = ($1, $2);
    print "## $section\n";
    while ($body =~ /<tr>\s*(.*?)\s*<\/tr>/gs) {
        my $row = $1;
        next if $row =~ /<th>/;
        my @cells = map { my $cell = $_; $cell =~ s/<[^>]+>//g; $cell =~ s/^\s+|\s+$//g; $cell =~ s/&amp;/&/g; $cell }
                        ($row =~ /<td>(.*?)<\/td>/gs);
        $cells[1] =~ s/^aat://;
        print join("\t", @cells), "\n";
    }
}
