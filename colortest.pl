#!/usr/bin/perl
# -*- cperl -*-

my $reset = chr(27) . qq{[0m};

require "./widgets.pl";
my @clr = named_colors();

for my $cdepth (4, 8, 24) {
  print $cdepth . "-bit color:\n";
  my ($black) = grep { $$_{name} eq "black" } @clr;
  my ($white) = grep { $$_{name} eq "white" } @clr;
  for my $c (@clr) {
    print        colorcode($c, $cdepth, "bg") . colorcode($black, $cdepth) . " $$c{name} "
      . $reset . colorcode($c, $cdepth, "bg") . colorcode($white, $cdepth) . " $$c{name} "
      . $reset . colorcode($black, $cdepth, "bg") . colorcode($c, $cdepth) . " $$c{name} "
      . $reset . colorcode($white, $cdepth, "bg") . colorcode($c, $cdepth) . " $$c{name} "
      . $reset . (($cdepth == 8) ? "($$c{fg}{8} on $$c{bg}{8})" : "") . "\n";
  }
}
