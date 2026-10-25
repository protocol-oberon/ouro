# Generates the bundled aat/ modules, lib/aat/*.ouro, from Linked Art's recommended terms,
# the table extract.pl writes: one module per section of the table, one constant per term,
# built with the std constructor of the pattern Linked Art gives that kind of term.
#
#   perl scripts/aat/generate.pl scripts/aat/recommended.tsv lib/aat
#
# The modules are generated, never edited: change the table or this script, and run it again.
use strict;
use warnings;
use Text::Wrap qw(wrap $columns);

my ($table, $out) = @ARGV;
die "usage: generate.pl recommended.tsv lib/aat\n" unless $table && $out;

# Each section of the table: its module, the std module and constructor that build its terms,
# what its terms are, and, for a term that is an instance rather than a Type, what the
# documentation says it is. A Type's label is its name in the table, and its documentation the
# table's description; an instance's label is the table's description, capitalised.
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

# A term's function name, from its name in the table: Given Name is given-name, partsUnit is
# parts, and Museum (as Place) is museum, the module saying it is a Place
sub function_name {
    my ($module, $name) = @_;
    $name =~ s/\s*\(as [^)]*\)//;
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

# The table, by section, in the table's order
my (@order, %terms, $section);
open(my $in, "<", $table) or die "cannot read $table: $!\n";
while (my $line = <$in>) {
    chomp $line;
    $line =~ s/\r$//;
    next if $line eq "";
    if ($line =~ /^## (.*)$/) {
        $section = $1;
        die "unknown section '$section'\n" unless $sections{$section};
        push @order, $section;
        next;
    }
    my ($name, $number, $description) = split /\t/, $line;
    die "not an AAT number in '$line'\n" unless $number =~ /^300\d{6}$/;
    push @{$terms{$section}}, [$name, $number, $description];
}

for my $section (@order) {
    my ($module, $import, $constructor, $what, $noun) = @{$sections{$section}};
    my (@names, %seen, @defs);
    for my $term (@{$terms{$section}}) {
        my ($name, $number, $description) = @$term;
        my $function = function_name($module, $name);
        die "two terms named '$function' in aat/$module\n" if $seen{$function}++;
        my ($label, $doc) = defined $noun
                          ? (capitalised($description), capitalised($description) . ", $noun.")
                          : ($name, $description =~ /\.$/ ? $description : "$description.");
        push @names, $function;
        push @defs, [$function, $doc, $number, $label];
    }

    my $width = 0;
    for (@names) { $width = length if length > $width }

    my $file = "$out/$module.ouro";
    open(my $fh, ">", $file) or die "cannot write $file: $!\n";
    binmode $fh;
    $columns = 96;
    print $fh wrap(";; ", ";; ", "aat/$module: the " . lc($section) . " Linked Art recommends, from linked.art/model/vocab/recommended, $what."
                               . " Generated by scripts/aat/generate.pl from scripts/aat/recommended.tsv: regenerate it, never edit it.") . "\n";
    print $fh "(import $import)\n\n";
    print $fh "(export " . join("\n        ", @names) . ")\n\n";
    for my $def (@defs) {
        my ($function, $doc, $number, $label) = @$def;
        printf $fh "(defn %-*s () (doc %s) (%s \"%s\" %s))\n", $width, $function, quoted($doc), $constructor, $number, quoted($label);
    }
    close $fh;
    print "$file: " . scalar(@names) . " terms\n";
}
