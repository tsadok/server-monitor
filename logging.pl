#!/usr/bin/perl -wT
# -*- cperl -*-

use strict;
use DateTime;
use DateTime::HiRes;

our $timezone; require "./timezone.pl";
$timezone ||= "UTC";

our ($datadir, $logdir,
     $widgetlogfile, $colorlogfile, $monitorlogfile, $verboselogfile
    );
require "./paths.pl";

sub sendtologfiles {
  # Only call this directly, when sending to more than one logfile.
  # That way less will have to change if we later need to complicate
  # its argument list for any reason.
  my ($info, @logfile) = @_;
  my $now = DateTime->now(time_zone => $timezone);
  for my $lf (unique(@logfile)) {
    open LOG, ">>", $lf
      or die "Cannot append to $lf: $!";
    print LOG "" . $now->day_abbr() . " " . $now->month_abbr()
      . " " . sprintf("%02d", $now->mday()) . " "
      . sprintf("%02d%02d", $now->hour(), $now->minute()) . "." . sprintf("%02d", $now->second()) . " "
      . $info . "\n";
    close LOG;
  }
}

sub sendtologfile {
  my ($logfile, $info) = @_;
  sendtologfiles($info, $logfile);
}

sub verboselog {
  my ($info) = @_;
  my $now = DateTime::HiRes->now(time_zone => $timezone);
  my $ts  = (sprintf "%02d", $now->hour()) . ":"
    . (sprintf "%02d", $now->minute()) . ":"
    . (sprintf "%02d", $now->second()) . "."
    . (sprintf "%03d", $now->microsecond());
  sendtologfile($verboselogfile, $ts . " " . $info);
}

sub widgetlog {
  my ($info) = @_;
  sendtologfile($widgetlogfile, $info);
}
sub colorlog {
  my ($info) = @_;
  sendtologfile($colorlogfile, $info);
}

sub monitorlog {
  my ($info) = @_;
  sendtologfile($monitorlogfile, $info);
}

# A lot of my code will have a uniq function already, but it tends to
# be defined in files that are going to end up depending on this one,
# and I don't like dependency loops.  And it's too trivial to warrant
# its own separate include file; frankly it should be a Perl builtin.
sub unique {
  my %seen;
  my @answer = map { $seen{$_}++ ? () : ($_) } @_;
  return @answer;
}

42;
