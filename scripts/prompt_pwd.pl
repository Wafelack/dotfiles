#!/usr/bin/env perl
use strict;
use warnings;
use Cwd;

my $directory = getcwd;
my @parts = split(/\//, $directory);
shift @parts; # Remove empty value.
my $username = getpwuid $<;

# Replace $HOME by a tilde.
if ($parts[0] eq "home" and $parts[1] eq $username) {
  shift @parts; shift @parts;
  print "~";
}

for (my $i = 0; $i < $#parts; $i++) {
  my $first = substr($parts[$i], 0, 1);
  print "/$first";
}

if (my $var = $parts[-1]) {
  print "/$var";
}
print "\n";
