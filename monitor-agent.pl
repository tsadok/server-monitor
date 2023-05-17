#!/usr/bin/perl
# -*- cperl -*-

# You can place copies of this file on various servers, and edit
# their $thissystem, $workdir, and $remote variables as needed,
# in order to gather df and clock data from multiple systems
# all to one place for convenient monitoring.

# The system that is running monitor.pl does not need to run
# monitor-agent.pl, because monitor.pl can gather the data on
# its own from the local system.

# Note that if one of the systems running monitor-agent goes
# offline for some reason, its clock data will stop updating
# and go out of sync with the other systems.  So if you see
# one system's clock stop keeping up, check whether it is
# actually up and running at all.

my $thissystem = `hostname` || "Error_$$";
chomp $thissystem;

my $workdir = "/home/jonadab/monitor/workdir";
my @destination =
  (
   "jonadab@" . "cogitation:/home/jonadab/monitor/$thissystem",
   #"jonadab@" . "hippo:/home/jonadab/monitor/$thissystem",
  );

my $df  = "/bin/df";
my $scp = "/usr/bin/scp";

# From here down _probably_ doesn't need to be edited much.

use DateTime;
use File::Spec::Functions;

my $utcfile = catfile($workdir, "monitor-utc.dat");
my $df_file = catfile($workdir, "monitor-df.dat");

for my $remote (@destination) {
  my $utcdest = catfile($remote,  "monitor-utc.dat");
  my $df_dest = catfile($remote,  "monitor-df.dat");

  print qq[
UTC local file:   $utcfile
UTC destination:  $utcdest
df local file:    $df_file
df destination:   $df_dest
];

  $ENV{PATH} = undef;

  open PIPE, "$df |";
  open DF,  ">", $df_file or die "Cannot write $df_file: $!";
  while (<PIPE>) {
    print DF $_;
  }
  close PIPE;
  close DF;
  system($scp, $df_file, $df_dest);

  my $utc = DateTime->now( time_zone => "UTC" );

  open UTC, ">", $utcfile or die "Cannot write $utcfile: $!";
  print UTC $utc->ymd() . " at " . $utc->hms() . "\n";
  close UTC;
  system($scp, $utcfile, $utcdest);
}
