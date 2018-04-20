#!/usr/bin/env perl

# genexports.pl < source.s   -- generate the export statements for all symbols

use strict;
use warnings;

my %syms;
my @scopes;

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
        print "        .export " . $mangled_sym . " := " . $sym . "\n";
    } else {
        print "        .export " . $sym . "\n";
    }
}
