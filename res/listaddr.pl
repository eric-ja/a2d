#!/usr/bin/env perl

# listaddr.pl <mapfile> <listfile> [<listfile> ...]
#
# Modify <listfiles> in place by adding absolute offsets in the front of each
# line, which are computed by looking up the link data for the object in
# <mapfile>.
#
# <listfile> should be a ca65 listing file. <mapfile> should be an ld65 map
# output file.

use strict;
use File::Basename;

my $mapfile = shift(@ARGV) or die "<mapfile> is required";

my $segment = "CODE";
my %segoffs;
my %segstart;

while (my $listfile = shift(@ARGV)) {
    $_, my $objname = fileparse($listfile, ".list");
    $objname .= ".o";

    open(my $map, "<", $mapfile) or die "Can't open $mapfile: $!";
    while (<$map>) {
        if (m/:$/ and index($_, $objname) != -1) {

            # Capture offset of object file within segment.
            while (defined($_ = <$map>) and !m/:$/) {
                m/(?<seg>[A-Za-z-_]*) *Offs=(?<offs>[0-9A-F]+) *Size=(?<size>[0-9A-F]+) *Align=(?<align>[0-9A-F]+) *Fill=(?<fill>[0-9A-F]+)/;
                $segoffs{$+{"seg"}} = hex $+{"offs"};
            }
        }

        # Capture start address of each segment.
        if (m/(?<seg>[A-Za-z-_]*) +(?<start>[0-9A-F]+) +(?<end>[0-9A-F]+) +(?<size>[0-9A-F]+) +(?<align>[0-9A-F]+)/) {
            $segstart{$+{"seg"}} = hex $+{"start"};
        }
    }
    close($map);

    {
        local $^I = "";
        local @ARGV = ($listfile);

        while (<>) {
            if (m/.segment "([^"]+)"/) {
                $segment = $1;
            }

            if (m/^([0-9A-F]+)r (.*)/) {
                my $raddr = hex $1;
                my $rest = $2;
                printf("%06x %4xr %s\n", $raddr + $segoffs{$segment} + $segstart{$segment}, $raddr, $rest);
            } elsif (m/^([0-9A-F]+) (.*)/) {
                print $1 . "      " . $2 . "\n";
            } else {
                print "      " . $_;
            }
        }
    }
}
