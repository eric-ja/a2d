#!/usr/bin/env perl

use strict;
use warnings;

my $vers = shift(@ARGV) // "";

$vers =~ /(?<VMajor>[0-9]+)\.(?<VMedium>[0-9]+)\.(?<VMinor>[0-9]+)(?<VStatus>.)(?<VRelease>[0-9]+)/;
my %vars = %+;

$vars{"VStatus"} = ord uc $vars{"VStatus"};

foreach my $name (keys %vars) {
    print "-D$name=" . $vars{$name} . "\n";
}

