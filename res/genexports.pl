#!/usr/bin/env perl

# genexports.pl [scopeprefix] < source.s   -- generate the export statements for all symbols
#
# ca65 scopes are tracked and are mangled using __ as the scope separator.
#
# If the optional scopeprefix is given, then all exported symbols are prefixed with
# <scopeprefix>__ (i.e., it simulates a toplevel scope encompassing all exports.)

use strict;
use warnings;

my %syms;
my @scopes;

my $prefix = shift(@ARGV) // "";
if ($prefix) {
   $prefix .= "__";
}

push @scopes, "";
while (<STDIN>) {
    s/;.*//;
    next if m/\.assert|\.org|PAD_TO/;

    if (m/\.proc +([0-9A-Za-z_]+)/ || m/\.scope +([0-9A-Za-z_]+)/ || m/PARAM_BLOCK +([0-9A-Za-z_]+)/) {
       my $sym = $scopes[-1] . $1;
       $syms{$sym} = $#scopes;
       push @scopes, $scopes[-1] . $1 . "::";
    }
    pop @scopes if m/\.endproc/ || m/\.endscope/ || m/END_PARAM_BLOCK/;

    if (m/([0-9A-Za-z_]+)(?::|\s+:=)[^:](.*)/) {
        my $sym = $scopes[-1] . $1;
        $syms{$sym} = $#scopes;
    }
}

foreach my $sym (keys %syms) {
    if ($syms{$sym}) {
        my $mangled_sym = $sym;
        $mangled_sym =~ s/::/__/g;
        print "        .export " . $prefix . $mangled_sym . " := " . $sym . "\n";
    } elsif ($prefix) {
        print "        .export " . $prefix . $sym . " := " . $sym . "\n";
    } else {
        print "        .export " . $sym . "\n";
    }
}
