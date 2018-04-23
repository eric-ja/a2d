#!/usr/bin/env perl

use strict;
use warnings;

my $vers = shift(@ARGV) // "";

$vers =~ /(?<VMajor>[0-9]+)\.(?<VMedium>[0-9]+)\.(?<VMinor>[0-9]+)(?<VStatus>.)(?<VRelease>[0-9]+)-(?<Variant>[a-z]+)/;
my %vars = %+;

$vars{"VStatus"} = ord uc $vars{"VStatus"};
$vars{"Variant"} = ord uc $vars{"Variant"};

foreach my $name (keys %vars) {
    print "-D$name=" . $vars{$name} . "\n";
}

