# Relabels the Getty terms in linked.art's examples, test/fixtures/json/*.json, as linked.art's
# recommended list labels them. The examples are inconsistent: one term is "Nation" in one and
# "Country" in the list. Ouro writes each term once, with the list's label, so its fixtures
# correct the examples to it. labels.tsv holds each correction, an AAT number, the label an
# example gives it and the list's, separated by tabs; this script applies them, and says how
# many of each it made. Running it again changes nothing.
#
#   perl scripts/fixtures/relabel.pl scripts/fixtures/labels.tsv test/fixtures/json/*.json
use strict;
use warnings;

my ($table, @files) = @ARGV;
die "usage: relabel.pl labels.tsv file.json ...\n" unless $table && @files;

open(my $in, "<", $table) or die "cannot read $table: $!\n";
my @labels;
while (my $line = <$in>) {
    chomp $line;
    $line =~ s/\r$//;
    next if $line eq "";
    my ($number, $given, $recommended) = split /\t/, $line;
    die "not an AAT number in '$line'\n" unless $number =~ /^300\d{6}$/;
    push @labels, [$number, $given, $recommended];
}
close $in;

my %made;
for my $file (@files) {
    open(my $fh, "<:raw", $file) or die "cannot read $file: $!\n";
    local $/;
    my $json = <$fh>;
    close $fh;
    my $changed = 0;
    for my $label (@labels) {
        my ($number, $given, $recommended) = @$label;
        my $count = ($json =~ s/("id":\s*"http:\/\/vocab\.getty\.edu\/aat\/$number",\s*"type":\s*"[^"]*",\s*"_label":\s*)"\Q$given\E"/$1"$recommended"/g) || 0;
        $made{"$number $given"} += $count;
        $changed += $count;
    }
    next unless $changed;
    open(my $out, ">:raw", $file) or die "cannot write $file: $!\n";
    print $out $json;
    close $out;
}
printf "%s -> %s: %d\n", $_->[1], $_->[2], $made{"$_->[0] $_->[1]"} // 0 for @labels;
