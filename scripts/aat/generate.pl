# Generates the bundled aat/ modules, lib/aat/*.ouro, from Linked Art's recommended terms,
# the table extract.pl writes, and from the terms Linked Art's examples use beyond them, the
# table examples.tsv, written by hand in the same form: one module per section, one constant
# per term, built with the std constructor of the pattern Linked Art gives that kind of term.
#
#   perl scripts/aat/generate.pl scripts/aat/recommended.tsv scripts/aat/examples.tsv lib/aat
#
# The modules are generated, never edited: change a table or this script, and run it again.
use strict;
use warnings;
use Text::Wrap qw(wrap $columns);

my ($table, $examples, $out) = @ARGV;
die "usage: generate.pl recommended.tsv examples.tsv lib/aat\n" unless $table && $examples && $out;

# Each section of the table: its module, the std module and constructor that build its terms,
# what its terms are, and, for a term that is an instance rather than a Type, what the
# documentation says it is. A Type's label is its name in the table, and its documentation the
# table's description; an instance's label is the table's description as the table writes it,
# lower-case for a material or a unit, and its documentation that description, capitalised.
my %sections = (
    "Name Types"        => ["name-type",       "std/base",       "base/aat",            "each a Type, to classify a Name"],
    "Identifier Types"  => ["identifier-type", "std/base",       "base/aat",            "each a Type, to classify an Identifier"],
    "Statement Types"   => ["statement-type",  "std/base",       "base/statement-kind", "each a kind of statement, classified as Brief Text, as Linked Art requires"],
    "Object Types"      => ["object-type",     "std/base",       "base/work-type",      "each a type of work, classified as Type of Work, as Linked Art requires"],
    "Place Types"       => ["place-type",      "std/base",       "base/aat",            "each a Type, to classify a Place"],
    "Group Types"       => ["group-type",      "std/base",       "base/aat",            "each a Type, to classify a Group"],
    "Digital Types"     => ["digital-type",    "std/base",       "base/aat",            "each a Type, to classify a DigitalObject"],
    "Set Types"         => ["set-type",        "std/base",       "base/aat",            "each a Type, to classify a Set"],
    "Dimension Types"   => ["dimension-type",  "std/base",       "base/aat",            "each a Type, to classify a Dimension"],
    "Languages"         => ["language",        "std/base",       "base/language",       "each a Language",                             "a language"],
    "Measurement Units" => ["unit",            "std/object",     "object/unit",         "each a MeasurementUnit, for a Dimension",     "a unit of measurement"],
    "Materials"         => ["material",        "std/object",     "object/material",     "each a Material, for made_of",                "a material"],
    "Currencies"        => ["currency",        "std/provenance", "provenance/currency", "each a Currency, for a MonetaryAmount",       "a currency"],
    "Nationalities"     => ["nationality",     "std/base",       "base/nationality",    "each classified as Nationality, as Linked Art requires", "a nationality"],
    "Occupations"       => ["occupation",      "std/base",       "base/occupation",     "each classified as Occupation, as Linked Art requires",  "an occupation"],
    "Shapes"            => ["shape",           "std/base",       "base/shape",          "each classified as Shape, as Linked Art requires",       "a shape"],
    "Activity Types"    => ["activity-type",   "std/base",       "base/aat",            "each a Type, to classify an Activity"],
    "Price Types"       => ["price-type",      "std/base",       "base/aat",            "each a Type, to classify a MonetaryAmount"],
    "Right Types"       => ["right-type",      "std/base",       "base/aat",            "each a Type, to classify a Right"],
    "Provenance Types"  => ["provenance-type", "std/base",       "base/aat",            "each a Type, to classify an event of provenance or a part of one"],
    "Genders"           => ["gender",          "std/actor",      "actor/gender",        "each classified as Gender, as Linked Art requires"],
    "Styles"            => ["style",           "std/base",       "base/style",          "each classified as Style, as Linked Art requires"],
    "Visual Types"      => ["visual-type",     "std/base",       "base/aat",            "each a Type, to classify a VisualItem"],
    "Role Types"        => ["role-type",       "std/base",       "base/aat",            "each a Type, to classify a part of an Activity by the role of who carries it out"],
);

# Names that cannot be a term's own: the time units are built-ins, and map is a special form
my %renamed = (
    "unit/years"   => "year-unit",
    "unit/months"  => "month-unit",
    "unit/days"    => "day-unit",
    "unit/hours"   => "hour-unit",
    "unit/minutes" => "minute-unit",
    "unit/seconds" => "second-unit",
    "object-type/map" => "map-object",
);

# The ISO codes of aat/language/iso and aat/currency/iso, which give every language and every
# currency its code as its notation, as Linked Art's examples mostly write them: a module's
# variant, what its codes are, and each term's code. A language's code is ISO 639-1's two
# letters; Ancient Greek has none, so it has ISO 639-3's grc. A currency's is ISO 4217's.
my %variants = (
    "language" => ["ISO 639", "The codes are ISO 639-1's two letters; Ancient Greek has none, so it has ISO 639-3's grc.", \my %iso],
    "currency" => ["ISO 4217", "The codes are ISO 4217's three letters.", \my %iso4217],
);

%iso4217 = (
    "british-pounds" => "GBP", "canadian-dollar" => "CAD", "euros"      => "EUR",
    "japanese-yen"   => "JPY", "swiss-franc"     => "CHF", "us-dollars" => "USD",
    "french-francs"  => "FRF",
);

%iso = (
    "ancient-greek" => "grc", "arabic"     => "ar", "chinese"    => "zh", "danish"   => "da",
    "dutch"         => "nl",  "english"    => "en", "farsi"      => "fa", "finnish"  => "fi",
    "french"        => "fr",  "german"     => "de", "greek"      => "el", "hebrew"   => "he",
    "hindi"         => "hi",  "icelandic"  => "is", "irish"      => "ga", "italian"  => "it",
    "japanese"      => "ja",  "korean"     => "ko", "latin"      => "la", "norwegian" => "no",
    "polish"        => "pl",  "portuguese" => "pt", "romanian"   => "ro", "russian"  => "ru",
    "spanish"       => "es",  "swedish"    => "sv", "tamil"      => "ta", "thai"     => "th",
    "turkish"       => "tr",  "urdu"       => "ur", "welsh"      => "cy",
);

# A term's function name, from its name in the table: Given Name is given-name, partsUnit is
# parts, and Museum (as Place) is museum, the module saying it is a Place, as Auction House
# (place) is place-type/auction-house
sub function_name {
    my ($module, $name) = @_;
    $name =~ s/\s*\([^)]*\)$//;
    $name =~ s/Unit$//;
    $name = lc $name;
    $name =~ s/[^a-z0-9]+/-/g;
    $name =~ s/^-|-$//g;
    return $renamed{"$module/$name"} // $name;
}

# Capitalises each word, keeping the capitals it has: us dollars is US Dollars as the table
# writes it, and (music) composer is (Music) Composer
sub capitalised {
    my ($text) = @_;
    $text =~ s/(^|[\s(-])([a-z])/$1\u$2/g;
    return $text;
}

sub quoted {
    my ($text) = @_;
    $text =~ s/\\/\\\\/g;
    $text =~ s/"/\\"/g;
    return "\"$text\"";
}

# The tables, by section, in their order: the recommended terms, then those of the examples,
# each section's after the recommended ones, and a section only the examples have last
my (@order, %terms, %examined);
for my $source ([$table, 0], [$examples, 1]) {
    my ($file, $from_examples) = @$source;
    my $section;
    open(my $in, "<", $file) or die "cannot read $file: $!\n";
    while (my $line = <$in>) {
        chomp $line;
        $line =~ s/\r$//;
        next if $line eq "";
        if ($line =~ /^## (.*)$/) {
            $section = $1;
            die "unknown section '$section'\n" unless $sections{$section};
            push @order, $section unless $terms{$section};
            $terms{$section} //= [];
            next;
        }
        my ($name, $number, $description) = split /\t/, $line;
        die "not an AAT number in '$line'\n" unless $number =~ /^300\d{6}$/;
        push @{$terms{$section}}, [$name, $number, $description, $from_examples];
        $examined{$section} = 1 if $from_examples;
    }
    close $in;
}

for my $section (@order) {
    my ($module, $import, $constructor, $what, $noun) = @{$sections{$section}};
    my (@names, %seen, @defs, $recommended);
    for my $term (@{$terms{$section}}) {
        my ($name, $number, $description, $from_examples) = @$term;
        my $function = function_name($module, $name);
        die "two terms named '$function' in aat/$module\n" if $seen{$function}++;
        my ($label, $doc) = defined $noun
                          ? ($description, capitalised($description) . ", $noun.")
                          : ($name, $description =~ /\.$/ ? $description : "$description.");
        $doc .= " From Linked Art's examples, not its recommended list." if $from_examples;
        $recommended = 1 unless $from_examples;
        push @names, $function;
        push @defs, [$function, $doc, $number, $label];
    }

    my $width = 0;
    for (@names) { $width = length if length > $width }

    my $file = "$out/$module.ouro";
    open(my $fh, ">", $file) or die "cannot write $file: $!\n";
    binmode $fh;
    $columns = 96;
    my $source = $recommended && $examined{$section}
               ? "the " . lc($section) . " Linked Art recommends, from linked.art/model/vocab/recommended, and those its examples use"
               : $recommended
               ? "the " . lc($section) . " Linked Art recommends, from linked.art/model/vocab/recommended"
               : "the " . lc($section) . " Linked Art's examples use";
    my $tables = $examined{$section}
               ? ($recommended ? "scripts/aat/recommended.tsv and scripts/aat/examples.tsv" : "scripts/aat/examples.tsv")
               : "scripts/aat/recommended.tsv";
    print $fh wrap(";; ", ";; ", "aat/$module: $source, $what."
                               . " Generated by scripts/aat/generate.pl from $tables: regenerate it, never edit it.") . "\n";
    print $fh "(import $import)\n\n";
    print $fh "(export " . join("\n        ", @names) . ")\n\n";
    for my $def (@defs) {
        my ($function, $doc, $number, $label) = @$def;
        printf $fh "(define/constant %-*s (doc %s) (%s \"%s\" %s))\n", $width, $function, quoted($doc), $constructor, $number, quoted($label);
    }
    close $fh;
    print "$file: " . scalar(@names) . " terms\n";

    write_iso($out, $module, lc $section, $width, @defs) if $variants{$module};
}

# aat/language/iso and aat/currency/iso: every term of the module with its ISO code as its notation
sub write_iso {
    my ($out, $module, $plural, $width, @defs) = @_;
    my ($standard, $codes, $table) = @{$variants{$module}};
    mkdir "$out/$module";
    my $file = "$out/$module/iso.ouro";
    open(my $fh, ">", $file) or die "cannot write $file: $!\n";
    binmode $fh;
    $columns = 96;
    print $fh wrap(";; ", ";; ", "aat/$module/iso: the $plural of aat/$module, each with its $standard code as its notation,"
                               . " as Linked Art's examples mostly write a $module. $codes"
                               . " Generated by scripts/aat/generate.pl from scripts/aat/recommended.tsv: regenerate it, never edit it.") . "\n";
    print $fh "(import aat/$module)\n\n";
    print $fh "(export " . join("\n        ", map { $_->[0] } @defs) . ")\n\n";
    for my $def (@defs) {
        my ($function, $doc, $number, $label) = @$def;
        my $code = $table->{$function} // die "no $standard code for the $module '$function'\n";
        $doc =~ s/\.$/, written $code./;
        printf $fh "(define/constant %-*s (doc %s) (with $module/%s :notation \"%s\"))\n", $width, $function, quoted($doc), $function, $code;
    }
    close $fh;
    print "$file: " . scalar(@defs) . " terms\n";
}
