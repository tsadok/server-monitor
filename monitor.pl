#!/usr/bin/perl

# -*- cperl -*-
use strict;
use Imager::Color;
use Term::Size;
use DateTime;
use File::Spec::Functions;
use Data::Dumper;
#use open ':encoding(UTF-8)';
#use open ":std";
#use utf8;

our (%bigtextfont, $monitorlogfile, @namedcolor, %namedcolor, %option, %magictext);
require "./logging.pl";
require "./widgets.pl";
require "./monitor-bigtextfonts.pl";
require "./monitor-sitecode.pl";

my %arg = @ARGV;
my @staffemail;
my $reset = chr(27) . qq{[0m};
$monitorlogfile = $arg{logfile} || $monitorlogfile;
my $eightbit = ($arg{"colordepth"} <= 8) ? 1 : undef;
system("clear");
$|=1;

my $configfile = $arg{configfile} ||= "monitor.cfg";
my @widget;
open CFG, "<", $configfile or die "Cannot read config file ($configfile): $!";
my $n = 0;
logit("================================== STARTING ==================================");
while (<CFG>) {
  chomp;
  my $line = $_;
  if (not $line) {
    ; # Ignore blank lines.
  } elsif ($line =~ /^\s*[#]/) {
    ; # Ignore commented-out lines.
  } elsif ($line =~ /^widget\s*[:]\s*(.*)/) {
    my @token = (split /;\s*|\s+/, $1);
    push @widget, +{ id => ++$n,
                     (map { my $t = $_;
                            my ($key,$val) = split(/\s*=\s*/, $t, 2);
                            if ($val =~ /,/) {
                              $val = +[ split /,/, $val ]; }
                            $key => $val
                          } @token),
                   };
    logit("Configured widget $n");
  } elsif ($line =~ /^\s*(\w+)\s*=\s*(.+?)\s*([#].*)?$/) {
    $arg{$1} ||= $2;
  } else {
    logit("Config line not understood: $line");
  }
}

biff_get_stafflist() if grep { $$_{type} eq "biff" } @widget;

if ($arg{debugwidgetlist}) {
  logit(Dumper(+{ widgets => \@widget }));
}

my ($xmax, $ymax) = Term::Size::chars *STDOUT{IO};
$xmax--; $ymax--; # Term::Size::chars returns counts, not zero-indexed maxima.
$xmax = $arg{xmax} ||= $xmax || 79;
$ymax = $arg{ymax} ||= $ymax || 23;
logit("Set ymax to $arg{ymax}, xmax to $arg{xmax}");

my $screen = +[
               map {
                 [ map {
                   +{ bg   => ($option{bg} || "black"),
                      fg   => ($option{fg} || "white"),
                      char => " " };
                 } 0 .. $ymax ]
               } 0 .. $xmax ];
while (1) {
  logit("************************ *** New Iteration *** ************************");
  $option{$_} = $arg{$_} for keys %arg;
  # logit("option{colordepth} == $option{colordepth}");
  my ($ut, $users) = uptime();
  %magictext = ( __UPTIME__ => $ut,
                 __USERS__  => $users . " users", );
  for my $w (@widget) {
    logit("widget $$w{id}, interval=$$w{interval}, ipos=$$w{__INTERVAL_POS__}");
    $$w{interval} ||= 1;
    if (not $$w{__INTERVAL_POS__}) {
      if (($$w{x} <= $xmax) and ($$w{y} <= $ymax)) {
        if (not $$screen[$$w{x}][$$w{y}]{bg}) {
          croak("Screen array has no bg at position ($$w{x},$$w{y}), but widget $$w{id} ($$w{title}) is positioned there.  "
                . Dumper(+{ xmax => $xmax, ymax => $ymax, widget => $w, cell => $$screen[$$w{x}][$$w{y}] }));
        }
        dowidget($w, $screen);
        $$w{__INTERVAL_POS__} = $$w{interval};
      }
    } else {
      redraw_widget($w, $screen);
    }
    $$w{__INTERVAL_POS__} = $$w{__INTERVAL_POS__} - 1;
  }
  drawscreen($screen, %arg);
  select undef, undef, undef, ($arg{delay} || 1);
}
exit 0; # Can't Happen
# Subroutines follow...

sub dowidget {
  my ($w, $s, @more) = @_;
  if (is_standard_widget($w)) {
    dostandardwidget($w, $s, @more);
  } elsif ($$w{type} eq "tictactoe") {
    dotictactoe($w, $s, @more);
  } elsif ($$w{type} eq "bargraph") {
    dobargraph($w, $s, @more);
  } elsif ($$w{type} eq "biff") {
    dobiff($w, $s, @more);
  } elsif ($$w{type} eq "df") {
    dodf($w, $s, @more);
  } elsif ($$w{type} eq "multidf") {
    domultidf($w, $s, @more);
  } elsif ($$w{type} eq "multiutc") {
    domultiutc($w, $s, @more);
  } elsif ($$w{type} eq "agenda") {
    doagenda($w, $s, @more);
  } elsif ($$w{type} eq "sitewidget") {
    # If you use this, it has to be defined in monitor-sitecode.pl
    site_widget($w, $s, @more);
  } else {
    widgetlog("monitor dowidget(): fallthrough, type=$$w{type}, id=$$w{id}");
    dotext($w, $s, @more);
  }
}

# moved to widgets.pl
#sub redraw_widget {
#  my ($w, $s) = @_;
#  logit("Using redraw-only mode for widget $$w{id}");
#  dowidget($w, $s, "redrawonly" => 1);
#}

sub dooldbiff {
  my ($w, $s, %more) = @_;
  logit("dooldbiff(widget ". $$w{id} .")");
  $$w{tick}++;
  $$w{contentsizex} ||= 11 + ($$w{fromlen} || 8) + ($$w{subjlen} || 12);
  if ((not ref $$w{__LINES__}) or (not @{$$w{__LINES__}}) or (1 == ($$w{tick} % ($$w{tickspercheck} || 90)))) {
    $$w{__LINES__} = [biffcheck($w, $s, %more)];
  }
  $$w{title} = ($$w{tick} % ($$w{tickspercheck} || 90))
    . " " . $$w{boxtitle} . " "
    . (defined($$w{matchcount}) ? $$w{matchcount} : @{$$w{__LINES__}}) . "/" . $$w{count}
    if $$w{boxtitle};
  return dologtext($w, $s, %more);
}

sub dobiff {
  my ($w, $s, %more) = @_;
  logit("dobiff(widget ". $$w{id} .")");
  $$w{tick}++;
  $$w{contentsizex} ||= 7 + ($$w{fromlen} || 9) + ($$w{subjlen} || 13);
  $$w{fieldlist}    ||= ["from", "subject", "category"];
  if ((not ref $$w{__LINES__}) or (not @{$$w{__LINES__}}) or (1 == ($$w{tick} % ($$w{tickspercheck} || 90)))) {
    $$w{table} = [biffcheck($w, $s, %more)];
  }
  $$w{title} = ($$w{tick} % ($$w{tickspercheck} || 90))
    . " " . $$w{boxtitle} . " "
    . (defined($$w{matchcount}) ? $$w{matchcount} : @{$$w{table}}) . "/" . $$w{count}
    if $$w{boxtitle};
  dotablewidget($w, $s, %more);
}

sub biffcheck {
  my ($w, $s, %more) = @_;
  my @errorline;
  if (not $$w{popserver})   { push @errorline, "Not configured: popserver"; }
  if (not $$w{popusername}) { push @errorline, "Not configured: popusername"; }
  if (not $$w{poppassword}) { push @errorline, "Not configured: poppassword"; }
  logit("biffcheck(widget ". $$w{id} .": $$w{popserver}:$$w{popusername})");
  return @errorline if @errorline;
  use Mail::POP3Client;
  my $pop = new Mail::POP3Client( HOST     => $$w{popserver},
                                  USER     => $$w{popusername},
                                  PASSWORD => $$w{poppassword},
                                  PORT     => $$w{popport} || 110,
                                  DEBUG    => $$w{debug},
                                );
  $$w{count} = $pop->Count();
  logit("POP3 count: $$w{count}");
  return if $$w{count} < 0;
  my @item = sort {
    ($$b{value} <=> $$a{value}) or ($$a{index} <=> $$b{index})
  } map {
    my $i = $_;
    my $headers = $pop->Head($i);
    my $ph = parseheaders($headers);
    my ($category, $value) = biff_hilight_msg($ph);
    my $flen = $$w{fromlen} || 8;
    my $slen = $$w{subjlen} || 12;
    my $fg   = $$w{$category . "catfg"} || ($value ? $$w{hilightfg} : $$w{boringfg}) || $$w{fg} || $option{fg} || "white";
    +{ value      => $value,
       category   => $category,
       from       => substr($$ph{from}    . (" " x $flen), 0, $flen),
       subject    => substr($$ph{subject} . (" " x $slen), 0, $slen),
       categoryfg => $fg,
       subjectfg  => $fg,
       fromfg     => $fg,
       index      => $i,
     }
  } 1 .. $$w{count};
  $$w{matchcount} = grep { $$_{value} } @item;
  return @item;
}

sub oldbiffcheck {
  my ($w, $s, %more) = @_;
  my @errorline;
  if (not $$w{popserver})   { push @errorline, "Not configured: popserver"; }
  if (not $$w{popusername}) { push @errorline, "Not configured: popusername"; }
  if (not $$w{poppassword}) { push @errorline, "Not configured: poppassword"; }
  logit("biffcheck(widget ". $$w{id} .": $$w{popserver}:$$w{popusername})");
  return @errorline if @errorline;
  use Mail::POP3Client;
  my $pop = new Mail::POP3Client( HOST     => $$w{popserver},
                                  USER     => $$w{popusername},
                                  PASSWORD => $$w{poppassword},
                                  PORT     => $$w{popport} || 110,
                                  DEBUG    => $$w{debug},
                                );
  $$w{count} = $pop->Count();
  logit("POP3 count: $$w{count}");
  return if $$w{count} < 0;
  my (@line, @altline);
  for my $i (1 .. $$w{count}) {
    my $headers = $pop->Head($i);
    my $ph = parseheaders($headers);
    my ($category, $value) = biff_hilight_msg($ph);
    my $flen = $$w{fromlen} || 8;
    my $slen = $$w{subjlen} || 12;
    if ($value) {
      push @line, [" " . substr($$ph{from}    . (" " x $flen), 0, $flen)
                   . " | "       . substr($$ph{subject} . (" " x $slen), 0, $slen)
                   . " | " . $category . " ",
                   $$w{$category . "catfg"} || $$w{hilightfg} || $$w{fg} || $option{fg} || "grey"];
    } else {
      push @altline, [" " . substr($$ph{from}    . (" " x $flen), 0, $flen)
                      . " | "       . substr($$ph{subject} . (" " x $slen), 0, $slen)
                      . " | " . $category . " ",
                      $$w{boringfg} || $$w{fg} || $option{fg} || "grey"];
    }
  }
  $$w{matchcount} = scalar @line;
  while (($$w{contentsizey} > scalar @line) and (scalar @altline)) {
    unshift @line, pop @altline;
  }
  return @line;
}


sub parseheaders {
  my ($headers) = @_;
  my %hdr;
  for my $h (grep { $_ } split /^(?=\w)/m, $headers) {
    my $hline = unwrap_header_line($h);
    if ($hline =~ /^From[:]\s*(.*?)\s*$/) {
      my $f = $1;
      $hdr{from} ||= $f;
    } elsif ($hline =~ /^Subject[:]\s*(.*?)\s*$/) {
      my $s = $1;
      $hdr{subject} ||= $s;
    } elsif ($hline =~ /^Return-Path[:]\s*(.*?)\s*$/) {
      my $rp = $1;
      $hdr{returnpath} ||= $rp;
    } elsif ($hline =~ /^Delivered-To[:]\s*(.*?)\s*$/) {
      $hdr{deliveredto} ||= $1;
    # TODO: any other headers we care about.
    }
  }
  return \%hdr;
}

sub unwrap_header_line {
  my ($raw) = @_;
  my $fixed = join " ", map { $_ } split /\r?\n\s*/, $raw;
  return $fixed;
}

sub dologtail {
  my ($w, $s, %more)  = @_;
  $$w{__LINES__}    ||= +[];
  if (not $more{redrawonly}) {
    $$w{logfile}      ||= $0;
    $$w{logfile}        = $0 if not -e $$w{logfile};
    $$w{title}        ||= $$w{logfile};
    $$w{contentsizex} ||= $xmax - $$w{x} - 2;
    $$w{contentsizey} ||= $ymax - $$w{y} - 2;
    $$w{__PADDING__}  ||= " " x $$w{contentsizex};
    my $line;
    open LOGTAIL, "<", $$w{logfile};
    seek LOGTAIL,-1, 2;  #get past last eol
    my $count=0; my $byte;
    while (1) {
      seek LOGTAIL, -1,1;
      read LOGTAIL, $byte, 1;
      if (ord($byte) == 10) {
        $count++;
        last if $count == $$w{contentsizey};
      }
      seek LOGTAIL,-1,1;
      last if (tell LOGTAIL == 0);
    }
    local $/ = undef;
    my $tail = <LOGTAIL>; close LOGTAIL;
    $$w{__LINES__} = +[ map { chomp $_; s/\s+/ /g; my $l = $_;
                              my $c = $$w{fg} || $option{fg} || "wheat";
                              if ($$w{colorize}) {
                                $c = colorize_logline($l, $w, caller => "dologtail", %more);
                              }
                              [$l, $c]
                            } split /\r?\n/, $tail ];
  }
  dologtext($w, $s, %more);
}

sub colorize_logline {
  my ($line, $w, %more) = @_;
  if ($$w{colorize} eq "perl") {
    return colorize_perl_logline($line, $w, %more);
  } # TODO: other types of colorization.
}

sub colorize_perl_logline {
  my ($line, $w, %more) = @_;
  if ($line =~ /uninitialized value/) {
    return "orange";
  } elsif ($line =~ /used only once/) {
    return "azure";
  } elsif ($line =~ /did you forget/) {
    return "red-orange";
  } elsif ($line =~ /wide character/i) {
    return "cyan";
  } elsif (($line =~ /BEGIN not safe/) or
           ($line =~ /End of script output before headers/)) {
    return "red";
  # TODO: more Perl stuff here.
  } elsif ($line =~ /script not found or unable to stat/) {
    return "black";
  } else {
    return $$w{fg} || $option{fg} || "grey";
  }
}

# TODO: Eventually I want to completely obviate dologtext() in favor
#       of something more nuanced, that can do color-based hilighting
#       of relevant bits within a line, or even syntax coloring.
sub dologtext {
  my ($w, $s, %more)  = @_;
  $$w{contentsizex} ||= $xmax - $$w{x} - 2;
  $$w{contentsizey} ||= $ymax - $$w{y} - 2;
  $$w{__PADDING__}  ||= " " x $$w{contentsizex};
  if (not $$w{__DIDBORDER__}) {
    doborder($w, $s);
    $$w{__DIDBORDER__} = 1 unless $$w{redraw};
  }
  my $offset = 0; $offset = (@{$$w{__LINES__}} - $$w{contentsizey})
    if $$w{contentsizey} < scalar @{$$w{__LINES__}};
  for my $n (1 .. $$w{contentsizey}) {
    my $line = $$w{__LINES__}[$n + $offset - 1] || "";
    my $fg   = $$w{fg} || $option{fg} || "white";
    if (ref $line) {
      ($line, $fg) = @$line;
    }
    dotext(+{ id          => $$w{id} . "_line" . $n,
              text        => substr(($line . $$w{__PADDING__}), 0, $$w{contentsizex}),
              x           => $$w{x} + 1,
              y           => $$w{y} + $n,
              transparent => $$w{transparent},
              bg          => $$w{bg} || $option{bg} || "black",
              fg          => $fg,
            }, $s);
  }
}

sub dobargraph {
  my ($w, $s, %more) = @_;
  if (not $more{redrawonly}) {
    $$w{iter}         ||= 0;
    $$w{bars}         ||= 1 + ($$w{x} + 22 < $xmax) ? 20 : ($xmax - $$w{x} - 2);
    $$w{bars}           = 1 if $$w{bars} < 1;
    $$w{contentsizex} ||= $$w{bars};
    $$w{contentsizey} ||= 5;
    $$w{threshhold}   ||= [5, 15, 25, 35];
    $$w{palette}      ||= "green"; # See %grad in createbggradient
    $$w{gradient}     ||= createbggradient($w);
    $$w{data} ||= +[ map { bgdatum($w, 0) } 1 .. $$w{bars}];
    $$w{minval}       ||= 0;
    $$w{maxval}       ||= $$w{contentsizey} * 8;
    $$w{avgval}       ||= $$w{minval} + (($$w{maxval} - $$w{minval}) / 3);
    my $newest          = getnextbgdatum($w);
    push @{$$w{data}}, $newest;
    shift @{$$w{data}};
    if ($$w{peakcpu} || $$w{peakmem}) {
      if (not grep { $$_{number} > $$newest{number} } @{$$w{data}}) {
        $$w{pslist} = [get_pslist($w)];
        if ($$w{peakcpu}) {
          update_peak($w, $$w{peakcpu}, "cpu", $s);
        }
        if ($$w{peakmem}) {
          update_peak($w, $$w{peakmem}, "mem", $s);
        }
      }}
  }
  if (not $$w{__DIDBORDER__}) {
    doborder($w, $s);
    $$w{__DIDBORDER__} = 1 unless $$w{redraw};
  }
  for my $n (1 .. $$w{contentsizey}) {
    for my $b (0 .. ($$w{bars} - 1)) {
      my $x = $$w{x} + 1 + $b;
      my $y = $$w{y} + $$w{contentsizey} + 1 - $n;
      $$s[$x][$y] = +{ fg   => (($arg{colordepth} >= 3) ? ($$w{data}[$b]{fg} || widgetfg($w)) : "white"),
                       bg   => widgetbg($w, undef, $$s[$x][$y]),
                       char => $$w{data}[$b]{char}[($n - 1)], };
    }
  }
}

sub update_peak {
  my ($parentwidget, $wchan, $type, $s) = @_;
  for my $w (grep { $$_{channel} eq $wchan } @widget) {
    blankrect($s, $$w{x}, $$w{y}, $$w{x} + $$w{contentsizex} + 2, $$w{y} + $$w{contentsizey} + 2,
              sub {
                my ($x,$y) = @_;
                return widgetbg($w, undef, $$s[$x][$y]),
              },
              " ",
              widgetfg($w));
    my $now = DateTime->now( time_zone => 'UTC' );
    $$w{title}  = "top " . $type . " at " . $now->hms();
    my @list = sort {
      $$b{"p" . $type} <=> $$a{"p" . $type}
    } @{$$parentwidget{pslist} || []};
    $$w{fieldlist} = ["p" . $type,
                      "args"];
    $$w{table} = [
                  map {
                    my $i = $_;
                    $list[$i];
                  } 0 .. $$w{contentsizey} - 1
                 ];
  }
}

sub get_pslist {
  my ($w) = @_;
  #my @psfield = qw(pcpu pmem pid nice size args);
  my @psfield = qw(pcpu pmem args);
  my @psarg   = ("-e", "-o", (join ",", @psfield));
  my $psbin   = "/bin/ps";
  local %ENV;
  $ENV{PATH}  = undef;
  open PIPE, "$psbin @psarg |";
  my @ps;
  <PIPE>; # Skip the header.
  while (<PIPE>) {
    chomp;
    s/^\s*//;
    my @v = (split /\s+/, $_, scalar @psfield);
    my @f = @psfield;
    my $p = +{};
    while (@f) {
      my $v = shift @v;
      my $f = shift @f;
      $$p{$f} = $v;
    }
    push @ps, $p;
  }
  return @ps;
}

sub createbggradient {
  my ($w) = @_;
  $$w{palette} ||= "green";
  $$w{palette} = "green" if $$w{palette} eq "default";
  my %grad = ( green   => +{ start  => +{ h => 151, s =>  37, v =>  80, },
                             finish => +{ h => 100, s => 100, v =>  50, }},
               rainbow => +{ start  => +{ h => 272, s => 100, v => 100, },
                             finish => +{ h =>   0, s =>  93, v => 100, }},
             );
  my $steps = scalar(@{$$w{threshhold}});
  my %delta = map {
    $_ => (($grad{$$w{palette}}{finish}{$_} - $grad{$$w{palette}}{start}{$_}) / $steps),
  } qw(h s v);
  my %current = map { $_ => $grad{$$w{palette}}{start}{$_} } qw(h s v);
  my @c = ();
  for my $s (0 .. $steps) {
    my @min = (($$w{minval} || 0), @{$$w{threshhold}});
    push @c, +{ min => $min[$s],
                h   => $current{h},
                s   => $current{s},
                v   => $current{v},
              };
    $current{$_} += $delta{$_} for qw(h s v);
  }
  @c = map { my $c = hsv2rgb($_);
             #$$c{rgb} = rgb($$c{r}, $$c{g}, $$c{b});
             $c
           } @c;
  return \@c;
}

sub hsv2rgb {
  my ($c) = @_;
  my $hsv = Imager::Color->new( hsv =>  [ $$c{h}, ($$c{v} / 100), ($$c{s} / 100) ] ); # hue, val, sat
  ($$c{r}, $$c{g}, $$c{b}) = $hsv->rgba;
  return $c;
}

sub pingdatum {
  my ($w) = @_;
  my $x = $$w{maxval};
  eval {
    use Net::DNS;
    $$w{target}   ||= "jonadab.jumpingcrab.com";
    $$w{targetip} ||= dnsresolve($$w{target});
  };
  if ($$w{targetip}) {
    eval {
      use Net::Ping;
      my $p = Net::Ping->new();
      $p->hires();
      my ($ret, $dur, $ip) = $p->ping($$w{targetip}, 1);
      if ($dur) {
        $x = $dur; #log(1 + $dur);
        $x = $$w{maxval} if $x > $$w{maxval};
      }
    };
  }
  return $x;
}

sub dnsresolve {
  my ($name) = @_;
  my $res   = Net::DNS::Resolver->new();
  my $reply = $res->search($name, "A");
  for my $ip ($reply->answer) {
    return $ip->address if $ip->can("address");
  }
}

sub getnextbgdatum {
  my ($w) = @_;
  if ($$w{datasource} eq "load") {
    my ($up, $users, $oneminuteload, $fiveminuteload, $fifteenminuteload) = uptime();
    $$w{corecount} ||= countcpucores($w);
    return bgdatum($w, $oneminuteload # In principle, this number goes from 0 to the number of CPU cores.
                   * $$w{maxval} / (($$w{headroomratio} || 2) * ($$w{corecount} || 1)));
  } elsif ($$w{datasource} eq "ping") {
    my $newdatum = pingdatum($w);
    return bgdatum($w, $newdatum);
  } else {
    return fakenextbgdatum($w);
  }
}

sub countcpucores {
  my ($w) = @_; # Probably don't actually need this.
  open INFO, "<", "/proc/cpuinfo";
  my $count = 0;
  while (<INFO>) {
    if (/^processor\s*[:]\s*(\d+)/) {
      $count++;
    }
  }
  close INFO;
  return $count;
}

sub fakenextbgdatum {
  my ($w) = @_;
  $$w{hist} ||= +[ map { $$w{avgval} } 1 .. 40 ];
  my $totalhist = 60 * $$w{avgval};# + (5 * $$w{maxval}) + (5 * $$w{minval});
  for my $h (@{$$w{hist}}) { $totalhist += $h }
  my $avg = $totalhist / 100;
  my $num = $avg;
  $$w{flutter} ||= ($$w{maxval} / 20);
  my $flutter = rand($$w{flutter});
  $num = $num - ($$w{flutter} / 2) + $flutter;
  if ($$w{spikeprob} > rand 100) {
    $$w{spike} ||= ($$w{maxval} / 8);
    my $spike = rand($$w{spike});
    $num += rand $spike;
  }
  $num = $$w{minval} if $num < $$w{minval};
  $num = $$w{maxval} if $num > $$w{maxval};
  my $d = bgdatum($w, $num);
  push @{$$w{hist}}, $$d{number};
  shift @{$$w{hist}};
  return $d;
}

sub bgdatum {
  my ($w, $number) = @_;
  $number ||= 0;
  my $fg = $option{fg} || "white";
  for my $c (@{$$w{gradient}}) {
    $fg = [$$c{r}, $$c{g}, $$c{b}] if $$c{min} <= $number;
  }
  my @block = ( +{ min => (1/8), char => "▁" },
                +{ min => (2/8), char => "▂" },
                +{ min => (3/8), char => "▃" },
                +{ min => (4/8), char => "▄" },
                +{ min => (5/8), char => "▅" },
                +{ min => (6/8), char => "▆" },
                +{ min => (7/8), char => "▇" },
                +{ min => (8/8), char => "█" },
              );
  my $valperchar = $$w{maxval} / $$w{contentsizey};
  my @char = map {
    my $n = $_;
    my $c = " ";
    for my $b (@block) {
      if ($number > (($n - 1) * $valperchar) + ($$b{min} * $valperchar)) {
        $c = $$b{char};
      }}
    $c
  } 1 .. $$w{contentsizey};
  return +{ number => $number,
            iter   => $$w{iter}++,
            fg     => $fg,
            char   => \@char,
          };
}

sub donotepad {
  my ($w, $s, %more) = @_;
  if ((not $$w{xmax}) or (not $$w{ymax})) {
    for my $k (grep { /line/ } keys %$w) {
      $$w{xmax} = 1 + length($$w{$k}) if $$w{xmax} <= length($$w{$k});
      my ($n) = $k =~ /(\d+)/;
      $$w{ymax} = $n if $$w{ymax} < $n;
      if (not $$w{__DID_UNESCAPES__}) {
        $$w{$k} = notepad_unescape($$w{$k});
      }
    }
    $$w{__DID_UNESCAPES__} = 1;
  }
  $$w{contentsizex} ||= $$w{xmax};
  $$w{contentsizey} ||= $$w{ymax};
  doborder($w,$s);
  for my $n (1 .. $$w{ymax}) {
    my $id = $$w{id} . "_line" . $n;
    dotext(+{ id          => $id,
              text        => substr((($$w{"line" . $n} || "") .
                                     (" " x $$w{xmax})), 0, $$w{xmax}),
              x           => $$w{x} + 1,
              y           => $$w{y} + $n,
              bg          => $$w{bg} || $option{bg} || "black",
              fg          => $$w{fg},
              transparent => $$w{transparent},
            }, $s);
  }
}

sub notepad_unescape {
  my ($text) = @_;
  # TODO: improve on this.
  $text =~ tr/_/ /;
  return $text;
}

sub dotictactoe {
  my ($w, $s, %more) = @_;
  $$w{title}        ||= "Tic-Tac-Toe";
  $$w{cols}         ||= 3;
  $$w{rows}         ||= 3;
  $$w{squaresizex}  ||= 5;
  $$w{squaresizey}  ||= 3;
  $$w{paddingx}     ||= 2;
  $$w{paddingy}     ||= 1;
  $$w{contentsizex} ||= ($$w{squaresizex} * $$w{cols}) + ($$w{cols} - 1) + (2 * $$w{paddingx});
  $$w{contentsizey} ||= ($$w{squaresizey} * $$w{rows}) + ($$w{rows} - 1) + (2 * $$w{paddingy});
  $$w{board}        ||= [ map { [map { " " } 1 .. $$w{rows}] } 1 .. $$w{cols}];
  $$w{turn}         ||= "X";
  $$w{iter}         ||= 0; $$w{iter}++;
  if (not $$w{__DIDBOARD__}) {
    $$w{title} = $$w{iter} . ($more{redrawonly} ? "R" : "") . ":" . $$w{__INTERVAL_POS__} . "/" . $$w{interval}
      if $$w{debugiter};
    doborder($w,$s);
    blankrect($s, $$w{x} + 1, $$w{y} + 1, $$w{x} + $$w{contentsizex}, $$w{y} + $$w{contentsizey},
              $$w{transparent} ? '__TRANSPARENT__' : sub { my ($x,$y) = @_;
                                                           return widgetbg($w, "boardbg", $$s[$x][$y])
                                                         });
    # We're kind of abusing blankrect() to paint foreground here:
    blankrect($s, $$w{x} + $$w{paddingx} + 1, $$w{y} + $$w{paddingy} + 1,
              $$w{x} + $$w{paddingx} + ($$w{squaresizex} * $$w{cols}) + 2,
              $$w{y} + $$w{paddingy} + ($$w{squaresizey} * $$w{rows}) + 2,
              sub { my ($x,$y) = @_;
                    return widgetbg($w, "boardbg", $$s[$x][$y])   },
              ($$w{boardchar} || "█"), widgetfg($w, "boardfg", "boardbg"));
    tictactoe_clearboard($w,$s);
    $$w{__DIDBOARD__} = 1 unless $$w{redraw};
  }
  tictactoe_taketurn($w) unless $more{redrawonly};
  for my $col (1 .. $$w{cols}) {
    for my $row (1 .. $$w{rows}) {
      my $tilex  = $$w{x} + $$w{paddingx} + (($col - 1) * ($$w{squaresizex} + 1)) + 1;
      my $tiley  = $$w{y} + $$w{paddingy} + (($row - 1) * ($$w{squaresizey} + 1)) + 1;
      my $symbol = $$w{board}[$col - 1][$row - 1];
      my $x = $tilex + int($$w{squaresizex} / 2);
      my $y = $tiley + int($$w{squaresizey} / 2);
      $$s[$x][$y] =
        +{ fg    => widgetfg($w, (lc($symbol) || "blankspace") . "fg"),
           bg    => widgetbg($w, (lc($symbol) || "blankspace") . "bg", $$s[$x][$y]),
           char  => $symbol, };
    }
  }
}

sub tictactoe_countrow {
  my ($w, $y) = @_;
  my $counts = +{ X => 0, Y => 0, " " => 0 };
  for my $x (0 .. ($$w{cols} - 1)) {
    $$counts{$$w{board}[$x][$y]}++;
  }
  return $counts;
}

sub tictactoe_countcol {
  my ($w, $x) = @_;
  my $counts = +{ X => 0, Y => 0, " " => 0 };
  for my $y (0 .. ($$w{rows} - 1)) {
    $$counts{$$w{board}[$x][$y]}++;
  }
  return $counts;
}

sub tictactoe_countdiagonal {
  my ($w, $dir) = @_;
  my $counts = +{ X => 0, Y => 0, " " => 0 };
  for my $x (0 .. ($$w{cols} - 1)) {
    my $y = ($dir > 0) ? $x : ($$w{cols} - $x - 1);
    $$counts{$$w{board}[$x][$y]}++;
  }
  return $counts;
}

sub tictactoe_taketurn {
  my ($w) = @_;
  if ($$w{turn} > 0) {
    $$w{turn}--;
    if ($$w{turn} == 0) {
      $$w{board} = [ map { [map { " " } 1 .. $$w{rows}] } 1 .. $$w{cols}];
    }
    return;
  }
  my $opponent = ($$w{turn} eq "X") ? "O" : "X";
  my @score = sort { $$b[0] <=> $$a[0] } sort { $$a[3] <=> $$b[3] } map {
    my $x = $_;
    map {
      my $y = $_;
      my $colcount = tictactoe_countcol($w, $x);
      my $rowcount = tictactoe_countrow($w, $y);
      my ($diacounta, $diacountb) = +{ map { $_ => 0 } ("X", "O", " ") };
      if ($$w{rows} == $$w{cols}) {
        if ($x == $y) {
          $diacounta = tictactoe_countdiagonal($w, 1); }
        if ($x + $y == 0) {
          $diacountb = tictactoe_countdiagonal($w, -1); }
      }
      my $score = 500 * $$rowcount{$$w{turn}} + 500 * $$colcount{$$w{turn}}
        + 500 * $$diacounta{$$w{turn}}        + 500 * $$diacountb{$$w{turn}}
        + 300 * $$rowcount{$opponent}         + 300 * $$colcount{$opponent}
        + 300 * $$diacounta{$opponent}        + 300 * $$diacountb{$opponent}
        +  25 * $$diacounta{" "}              + 25 * $$diacountb{" "} # prefer diagonals generally
        + 1;
      if (($$w{board}[$x][$y] eq "X") or ($$w{board}[$x][$y] eq "O")) {
        $score = -1;
      }
      [$score, $x, $y, rand 1000];
    } 0 .. ($$w{rows} - 1)
  } 0 .. ($$w{cols} - 1);
  $$w{board}[$score[0][1]][$score[0][2]] = $$w{turn};
  $$w{turn} = $opponent;
  if (tictactoe_complete($w, $score[0][1], $score[0][2])) {
    $$w{turn} = $$w{windelay} || 5;
  }
}

sub tictactoe_complete {
  # Check whether the move that was just made completed the game.
  # Does not loop over all positions, because that's unnecessary.
  my ($w, $x, $y) = @_;
  my $colcount  = tictactoe_countcol($w, $x);
  my $rowcount  = tictactoe_countrow($w, $y);
  my $diacounta = tictactoe_countdiagonal($w,1);
  my $diacountb = tictactoe_countdiagonal($w,-1);
  return
    (($$colcount{X} == $$w{cols}) or ($$colcount{O} == $$w{cols}) or
     ($$rowcount{X} == $$w{rows}) or ($$rowcount{O} == $$w{rows}) or
     (($$w{cols} == $$w{rows}) and
      (($$diacounta{X} == $$w{cols}) or ($$diacounta{O} == $$w{cols}) or
       ($$diacountb{X} == $$w{cols}) or ($$diacountb{O} == $$w{cols}))));
}

sub tictactoe_clearboard {
  my ($w, $s) = @_;
  for my $col (1 .. $$w{cols}) {
    for my $row (1 .. $$w{rows}) {
      my $tilex = $$w{x} + $$w{paddingx} + (($col - 1) * ($$w{squaresizex} + 1)) + 1;
      my $tiley = $$w{y} + $$w{paddingy} + (($row - 1) * ($$w{squaresizey} + 1)) + 1;
      blankrect($s, $tilex, $tiley, $tilex + $$w{squaresizex} - 1, $tiley + $$w{squaresizey} - 1,
                sub {
                  my ($x, $y) = @_;
                  return widgetbg($w, "tilebg", $$s[$x][$y]); },
                ($$w{tilechar} || " "), widgetfg($w, "tilefg"));
    }}
}

sub domultidf {
  my ($w, $s, %arg) = @_;
  $$w{count} ||= 0;
  $$w{freq}  ||= 10;
  $$w{title} ||= "df";
  my @item;
  if (not ($$w{count} % $$w{freq})) {
    logit("domultidf(widget " . $$w{id} . ")");
    doborder($w, $s, %arg);
    for my $i (1 .. $$w{contentsizey}) {
      if ($$w{"label" . $i} or $$w{"src" . $i} or $$w{"fg" . $i}) {
        my $label   = $$w{"label" . $i} || $$w{"src" . $i} || "";
        my $labelfg = $$w{"fg" . $i} || $$w{fg} || "grey";
        my $datadir = $$w{datadir} || "./";
        my $file = $$w{"src" . $i}
          ? catfile($datadir, $$w{"src" . $i}, "monitor-df.dat")
          : "/bin/df |";
        my ($dev, $size, $used, $avail, $pct, $mnt);
        $$w{fslist} ||= "root";
        #if (not ref $$w{fslist}) {
        #  $$w{fslist} = [split /,\s*/, $$w{fslist}];
        #}
        my $list = $$w{"fs" . $i} || $$w{fslist};
        if (not ref $list) {
          $list = [split /,\s*/, $list];
        }
        logit(" label=$label; file=$file");
        if (open PIPE, $file) {
          while (<PIPE>) {
            chomp;
            my $input = $_;
            if ($input =~ m!(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)[%]\s+(.+?)\s*$!) {
              ($dev, $size, $used, $avail, $pct, $mnt) = ($1, $2, $3, $4, $5, $6);
              my @match = grep { (index($dev, $_) >= 0) or ($mnt eq $_) } @{$list};
              if ((grep { /^root$/ } @{$list}) and ($mnt eq "/")) { # Magic
                @match = ($dev =~ /(\w+)$/);
              }
              if (@match) {
                push @item, +{ label   => $label,
                               labelfg => $labelfg,
                               dev     => $match[rand @match],
                               size    => $size,
                               used    => $used,
                               avail   => $avail,
                               pct     => sprintf("%3d", $pct) . "%",
                               pctfg   => pctclr($pct),
                               mnt     => $mnt,
                             };
                logit(" Showing: $label $dev $mnt");
              } else {
                logit(" Not Showing: $label $dev $mnt");
              }
            }
          }
        } else {
          logit(" Error: failed to open $file");
          push @item, +{ label   => $label,
                         labelfg => $labelfg,
                         dev     => $$w{"src" . $i},
                         mnt     => $$w{"src" . $i},
                         size    => 0,
                         used    => 0,
                         pct     => "ERROR",
                         pctfg   => [255,127,255],
                       }
        }
      }}
    my @field = ("label", "pct");
    push @field, "dev" if $$w{showdev};
    push @field, "mnt" if $$w{showmnt};
    dotable($w, $s, [@field], \@item, %arg);
  }
}

sub doagenda {
  my ($w, $s, %arg) = @_;
  logit("doagenda(widget " . $$w{id} . ")");
  doborder($w, $s, %arg);
  $$w{title} ||= "Agenda";
  my %tzalias = ( localtime => $arg{localtimezone} || "America/New_York" );
  my $file = catfile($$w{src}, "monitor-agenda.dat");
  if ((-e $file) and (open AGF, "<", $file)) {
    my $now = DateTime->now( time_zone => $tzalias{lc($$w{tz})} || $$w{tz} || $tzalias{localtime} );
    $$w{table} = [ map {
      my $line = $_;
      my ($year, $month, $mday, $hour, $minute, $second, $message) = $line
        =~ m!(\d{4})-(\d+)-(\d+)\s+at\s+(\d+)[:](\d+)[:](\d+)\s+(.*)!;
      my $dt; eval {
        $dt = DateTime->new( year      => $year,
                             month     => $month,
                             day       => $mday,
                             hour      => $hour,
                             minute    => $minute,
                             time_zone => "UTC"
                           )->set_time_zone($tzalias{lc($$w{tz})} || $$w{tz} || $tzalias{localtime});
      };
      my $when;
      my $whenfg = $$w{fg};
      if (ref $dt) {
        my $hour = ($dt->hour() % 12) || 12;
        my $min  = $dt->minute();
        my $ampm = (($dt->hour() >= 12) ? "pm" : "am");
        if ($dt->ymd() eq $now->ymd()) {
          if ($min) {
            $when = $hour . ":" . sprintf("%2d", $min) . $ampm;
          } elsif ($hour eq "12") {
            $when = (($dt->hour()) ? "Noon" : "Midnight");
          } else {
            $when = $hour . $ampm;
          }
          if ($dt->ymd() lt $now->clone()->add( minutes => 90 )->ymd()) {
            $whenfg = $$w{soonfg} || $$w{todayfg} || $$w{fg};
          } else {
            $whenfg = $$w{todayfg} || $$w{fg};
          }
        } elsif ($dt->ymd() lt $now->clone()->add( days => 5 )->ymd()) {
          $when = $dt->day_abbr() . " " . $hour . $ampm;
          if ($dt->ymd() lt $now->clone()->add( hours => 34 )->ymd()) {
            $whenfg = $$w{tomorrowfg} || $$w{thisweekfg} || $$w{fg};
          } else {
            $whenfg = $$w{thisweekfg} || $$w{fg};
          }
        } elsif ($dt->ymd() lt $now->clone()->add( months => 5 )) {
          $when   = $dt->month_abbr() . " " . sprintf("%02d", $dt->mday());
          $whenfg = $$w{thisyearfg} || $$w{fg};
        } else {
          $when   = $dt->year() . " " . $dt->month_abbr();
          $whenfg = $$w{distantfg} || $$w{fg};
        }
      } else {
        $when    = "ERROR";
        $whenfg  = $$w{errorfg} || [255, 127, 255],
        $message = $line;
      }
      +{ when => $when, whenfg => $whenfg, message => $message, };
    } grep { $_ } map { chomp; $_; } <AGF> ];
    close AGF;
  }
  dotable($w, $s, ["when", "message"], $$w{table} || [], %arg);
}

sub dodf {
  my ($w, $s, %arg) = @_;
  $$w{count} ||= 0;
  $$w{freq}  ||= 10;
  $$w{title} ||= "df";
  #$$w{title} = "df " . scalar @{$$w{fs} || []};
  if (not ($$w{count} % $$w{freq})) {
    open PIPE, "/bin/df |";
    my @fs = ();
    while (<PIPE>) {
      chomp;
      my $line = $_;
      if ($line =~ m!(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)[%]\s+(.+?)\s*$!) {
        my ($dev, $size, $used, $avail, $pct, $mnt) = ($1, $2, $3, $4, $5, $6);
        if ($$w{fslist}) {
          if (not ref $$w{fslist}) {
            $$w{fslist} = [split /,\s*/, $$w{fslist}];
          }
          my @match = grep { index($dev, $_) >= 0 } @{$$w{fslist}};
          if ((grep { /^root$/ } @{$$w{fslist}}) and ($mnt eq "/")) { # Magic
            @match = ($dev =~ /(\w+)$/);
          }
          if (@match) {
            push @fs, +{ dev   => $match[rand @match],
                         size  => $size,
                         used  => $used,
                         avail => $avail,
                         pct   => $pct,
                         mnt   => $mnt
                       };
          }
        } elsif ($dev =~ m!^/dev/(.*)!) {
          push @fs, +{ dev   => $1,
                       size  => $size,
                       used  => $used,
                       avail => $avail,
                       pct   => $pct,
                       mnt   => $mnt
                     };
        }
      }
    }
    $$w{fs} = [@fs];
  }
  doborder($w, $s, %arg);
  if (not @{$$w{fs}}) {
    push @{$$w{fs}}, +{ dev => "No Filesystems", pct => "100" };
  }
  my $yoffset = 0;
  for my $f (@{$$w{fs}}) {
    $yoffset++;
    my $id = $$w{id} . "_fs" . $yoffset;
    dotext(+{ id          => $id,
              text        => sprintf("%3d", $$f{pct}) . "% " . $$f{dev}
                             . ($$w{showmount} ? " $$f{mnt}" : ""),
              transparent => $$w{transparent},
              x           => $$w{x} + 1,
              y           => $$w{y} + $yoffset,
              bg          => $$w{bg} || $option{bg} || "black",
              fg          => pctclr($$f{pct}),
            }, $s);
  }
  $$w{count}++;
}

sub pctclr {
  my ($pct) = @_;
  if ($pct =~ /\d+/) {
    return (($pct < 10) ? "blue" :
            ($pct < 20) ? "azure" :
            ($pct < 30) ? "cyan" :
            ($pct < 40) ? "teal" :
            ($pct < 50) ? "green" :
            ($pct < 60) ? "spring-green" :
            ($pct < 70) ? "yellow" :
            ($pct < 80) ? "orange" :
            ($pct < 90) ? "red-orange" :
            ($pct < 95) ? "red" :
            ($pct < 98) ? "purple-red" :
            "magenta");
  } else {
    return [200,96,255];
  }
}

sub domultiutc {
  my ($w, $s, %arg) = @_;
  logit("domultiutc(widget " . $$w{id} . ")");
  doborder($w, $s, %arg);
  $$w{title} ||= "UTC";
  my @row;
  my $localutc = DateTime->now( time_zone => "UTC" );
  for my $i (1 .. $$w{contentsizey}) {
    if ($$w{"label" . $i} or $$w{"src" . $i} or $$w{"fg" . $i}) {
      my $label   = $$w{"label" . $i} || $$w{"src" . $i} || "";
      my $labelfg = $$w{"fg" . $i} || $$w{fg} || "grey";
      my $datadir = $$w{datadir} || "./";
      my $file = $$w{"src" . $i}
        ? catfile($datadir, $$w{"src" . $i}, "monitor-utc.dat")
        : undef;
      logit(" label=$label; file=$file");
      my ($time, $timefg);
      if (defined $file) {
        if (open TIME, "<", $file) {
          my $line = <TIME>;
          if ($line =~ /(\d{4})-(\d+)-(\d+)\s*at\s*(\d+)[:](\d+)[:](\d+)/) {
            my ($year, $month, $day, $hour, $minute, $second) = ($1, $2, $3, $4, $5, $6);
            $time = sprintf("%02d:%02d", $hour, $minute);
            my $dt = DateTime->new( time_zone => "UTC",
                                    year      => $year,
                                    month     => $month,
                                    day       => $day,
                                    hour      => $hour,
                                    minute    => $minute,
                                    second    => $second,
                                  );
            my $diffdur = ($dt >= $localutc)
              ? $dt->clone()->subtract_datetime($localutc)
              : $localutc->clone()->subtract_datetime($dt);
            my $diff    = (((((($diffdur->months() * 30) # close enough, for color-coding purposes; the important thing is don't wrap to 0.
                               + $diffdur->days()) * 24 * 60) + $diffdur->minutes()) * 60) + $diffdur->seconds()) / 60; # Difference in minutes.
            $timefg     = ($diff < (90/60)) ? ($$w{offby0fg} || $$w{fg} || $option{fg} || "white") :
              $$w{"offby" . int($diff) . "fg"} || pctclr(int(10 * (log($diff * 30) - 2)));
          } else {
            logit("Failed to parse UTC time from $file");
            $time   = "ERROR";
            $timefg = $$w{errorfg} || "magenta";
          }
        } else {
          logit("Failed to open tiem file, $file: $!");
          $time   = "ERROR";
          $timefg = $$w{errorfg} || "magenta";
        }
      } else {
        $time   = sprintf("%02d:%02d", $localutc->hour(), $localutc->minute());
        $timefg = $$w{offby0fg} || $$w{fg} || $option{fg} || "white";
      }
      push @row, +{ label   => $label,
                    labelfg => $labelfg,
                    time    => $time,
                    timefg  => $timefg,
                  };
    }}
  dotable($w, $s, ["label", "time"], \@row, %arg);
}

sub doclock {
  my ($w, $s) = @_;
  my %tzalias = ( localtime => $arg{localtimezone} );
  my $dt = DateTime->now(
                         time_zone => ($tzalias{lc $$w{tz}} || $$w{tz}
                                       || $tzalias{localtime}),
                        );
  my $hour = ($dt->hour() % 12); $hour = 12 if $hour < 1;
  $hour = sprintf("%02d",$dt->hour()) if $$w{military};
  my @tpart = ( +[hour => $hour, "fg", "bg"] );
  if (($dt->hour() == 12) and ($dt->minute() < 1)) {
    @tpart = ( +[hour => "Noon", "fg", "bg"] );
  } elsif (($dt->hour() == 0) and ($dt->minute() < 1)) {
    my @tpart = ( +[hour => "Midnight", "fg", "bg"] );
  } else {
    push @tpart, [colona => ":", "colonfg", "colonbg"];
    push @tpart, [minute => sprintf("%02d", $dt->minute()) => "minutefg", "minutebg" ];
    if ($$w{showseconds}) {
      push @tpart, [colonb => ":", "colonfg", "colonbg"];
      push @tpart, [second => sprintf("%02d", $dt->second()), "secondfg", "secondbg" ];
    }
    push @tpart, [pm => "pm", "pmfg", "pmbg" ]
      if (($dt->hour >= 12) and (not $$w{military}));
    push @tpart, [am => "am", "pmfg", "pmbg" ]
      if (($dt->hour < 12) and $$w{showam});
    # Having the sense not to set showam and military for the
    # same clock widget is left as an exercise for the configurer.
  }
  my $time = join "", map { $$_[1] } @tpart;
  my $clen = length($time);
  if (not ($$w{minutefg} || $$w{secondfg} || $$w{colonfg})) {
    @tpart = ( +[ time => $time => "fg" => "bg" ]);
  }
  my $date = $dt->year() . "-" . $dt->month_abbr() . "-" . $dt->mday();
  if ($$w{showdate} and (length($date) > $clen))    { $clen = length($date); }
  if ($$w{title} and (length($$w{title}) > $clen))  { $clen = length($$w{title}); }
  if ($$w{showdow} and length($dt->day_name()) > $clen) { $clen = length($dt->day_name()); }
  if ($$w{contentsizex} and $$w{contentsizey}) {
    # Blank at the _previous_ size (in case it is shrinking):
    blankrect($s, $$w{x}, $$w{y}, $$w{x} + $$w{contentsizex} + 1, $$w{y} + $$w{contentsizey} + 1,
              widgetbg($w), " "); #"░", widgetfg($w));
  }
  $$w{contentsizey} = 1 + ($$w{showdate} ? 1 : 0) + ($$w{showdow} ? 1 : 0);
  $$w{contentsizex} = $clen;
  blankrect($s, $$w{x}, $$w{y}, $$w{x} + $$w{contentsizex} + 1, $$w{y} + $$w{contentsizey} + 1,
            widgetbg($w), " ");
  doborder($w,$s);
  my $pos = $$w{x} + 1 + (($clen > length($time))
                          ? (int(($clen - length($time)) / 2)) : 0);
  for my $p (@tpart) {
    dotext(+{ id          => $$w{id} . "_" . $$p[0],
              x           => $pos,
              y           => $$w{y} + 1,
              fg          => $$w{$$p[2]} || $$w{fg} || $option{clockfg} || $option{fg} || "green",
              bg          => $$w{$$p[3]} || $$w{bg} || $option{bg} || "black",
              text        => $$p[1],
              transparent => $$w{transparent},
            }, $s);
    $pos += length($$p[1]);
  }
  if ($$w{showdow}) {
    dotext(+{ id          => $$w{id} . "_dow",
              x           => $$w{x} + 1 + (($clen > length($dt->day_name()))
                                           ? (int(($clen - length($dt->day_name())) / 2)) : 0),
              y           => $$w{y} + 2,
              fg          => $$w{dowfg}  || $$w{datefg} || $$w{fg} || $option{clockfg} || $option{fg} || "green",
              bg          => $$w{dowbg}  || $$w{datebg} || $$w{bg} || $option{bg} || "black",
              text        => $dt->day_name(),
              transparent => $$w{transparent},
            }, $s);
  }
  if ($$w{showdate}) {
    dotext(+{ id          => $$w{id} . "_date",
              x           => $$w{x} + 1 + (($clen > length($date))
                                           ? (int(($clen - length($date)) / 2)) : 0),
              y           => $$w{y} + 2 + ($$w{showdow} ? 1 : 0),
              fg          => $$w{datefg} || $$w{fg} || $option{clockfg} || $option{fg} || "green",
              bg          => $$w{datebg} || $$w{bg} || $option{bg} || "black",
              text        => $date,
              transparent => $$w{transparent},
            }, $s);
  }
}

#sub doborder {
#  my ($w, $s) = @_;
#  my $fg = widgetfg($w, "borderfg");
#  $$s[$$w{x}][$$w{y}] = +{ char => "╔", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y}]) };
#  $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y}] = +{ char => "╗", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y}]) };
#  $$s[$$w{x}][$$w{y} + $$w{contentsizey} + 1] = +{ char => "╚", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y} + $$w{contentsizey} + 1]) };
#  $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $$w{contentsizey} + 1]
#    = +{ char => "╝", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $$w{contentsizey} + 1]) };
#  for my $x (1 .. $$w{contentsizex}) {
#    $$s[$$w{x} + $x][$$w{y}] = +{ char => "═", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $x][$$w{y}]) };
#    $$s[$$w{x} + $x][$$w{y} + $$w{contentsizey} + 1] = +{ char => "═", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $x][$$w{y} + $$w{contentsizey} + 1]) };
#  }
#  for my $y (1 .. $$w{contentsizey}) {
#    $$s[$$w{x}][$$w{y} + $y] = +{ char => "║", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y} + $y]) };
#    $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $y] = +{ char => "║", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $y]) };
#  }
#  if ($$w{title}) {
#    dotext(+{ id          => $$w{id} . "_title",
#              x           => $$w{x} + 1 + (($$w{contentsizex} > length($$w{title}))
#                                           ? (int(($$w{contentsizex} - length($$w{title})) / 2)) : 0),
#              y           => $$w{y},
#              fg          => $$w{titlefg} || $$w{borderfg} || $$w{fg},
#              bg          => $$w{titlebg} || $$w{borderbg} || $$w{bg},
#              text        => $$w{title},
#              transparent => $$w{transparent},
#            }, $s);
#  }
#}

sub uptime {
  my $ut = `uptime`;
  chomp $ut;
  $ut =~ /(up (?:\d+ \w+,?\s*\d*[:]?\d*(?: min)?)|\d+[:]?\d*(?: min)?),\s+(\d+\s+users?),\s+load average[:]\s+([0-9.]+),\s+([0-9.]+),\s+([0-9.]+)/;
  my ($up, $users, $loadone, $loadtwo, $loadthree) = ($1, $2, $3, $4, $5);
  if (wantarray) { return ($up, $users, $loadone, $loadtwo, $loadthree); }
  else { return $ut }
}

#sub dotext {
#  my ($t, $s) = @_;
#  my ($ut, $users) = uptime();
#  my %magictext = ( __UPTIME__ => $ut,
#                    __USERS__  => $users );
#  if (not $$t{__DONE__}) {
#    my $text = (defined $$t{text}) ? $$t{text} : $$t{title} || $$t{type} || "t_$$t{id}";
#    $text = $magictext{$text} || $text;
#    $$t{rows} = 1;
#    $$t{cols} = length $text;
#    my $x = ($$t{x} >= 0) ? $$t{x} : ($arg{xmax} + $$t{x} - $$t{cols});
#    for my $c (split //, $text) {
#      $$s[$x][$$t{y}] = +{ bg   => widgetbg($t, "bg", $$s[$x][$$t{y}]),
#                           fg   => widgetfg($t),
#                           char => $c };
#      $x++;
#    }
#  }
#}

#sub widgetfg {
#  my ($w, $fgfield) = @_;
#  $fgfield ||= "fg"; $fgfield = "fg" if not $$w{$fgfield};
#  return (ref $$w{$fgfield}) ? rgb(@{$$w{$fgfield}}) : "";
#}
#
#sub widgetbg {
#  my ($w, $bgfield, $old) = @_;
#  $bgfield ||= "bg"; $bgfield = "bg" if not $$w{$bgfield};
#  if ($$w{transparent}) {
#    return $$old{bg} if $$old{bg};
#  }
#  return (ref $$w{$bgfield}) ? rgb(@{$$w{$bgfield}},"bg") : "";
#}

sub logit {
  monitorlog(@_);
}
#sub logit {
#  my ($msg) = @_;
#  open LOG, ">>", $monitorlogfile or die "Cannot write $monitorlogfile: $!";
#  my $now = DateTime->now( time_zone => "America/New_York" );
#  print LOG $now->hms() . " " . $msg . "\n";
#  close LOG;
#}

## sub eightbitcolor {
##   my ($red, $green, $blue, $isbg) = @_;
##   my $delimiter = ";";
##   # This calcuation is inline, rather than being a function called
##   # three times, for perf reasons.  Yes, it has a significant impact,
##   # perhaps because it's called for every character cell every iter.
##   my $r = int(($red   + ($isbg ? 0 : 21)) * 6 / 256); $r = 5 if $r > 5;
##   my $g = int(($green + ($isbg ? 0 : 21)) * 6 / 256); $g = 5 if $g > 5;
##   my $b = int(($blue  + ($isbg ? 0 : 21)) * 6 / 256); $b = 5 if $b > 5;
##   if (($r == $g) and ($r == $b)) {
##     my $gray = int(($red + ($isbg ? 0 : 5)) * 24 / 256);
##     $gray = 23 if $gray > 23;
##     return "\x1b[" . ($isbg ? "48" : "38") . $delimiter . "5" . $delimiter . (232 + $gray) . "m";
##   } else {
##     my $cubeval = 16 + (36 * $r) + (6 * $g) + $b;
##     return "\x1b[" . ($isbg ? "48" : "38") . $delimiter . "5" . $delimiter . $cubeval . "m";
##   }
## }
## 
## sub rgb { # Return terminal code for a 24-bit color.
##   my ($red, $green, $blue, $isbg) = @_;
##   return eightbitcolor($red, $green, $blue, $isbg) if $eightbit;
##   my $fgbg = ($isbg) ? 48 : 38;
##   my $delimiter = ";";
##   return "\x1b[$fgbg$ {delimiter}2$ {delimiter}$ {red}"
##     . "$ {delimiter}$ {green}$ {delimiter}$ {blue}m";
## }

# If you see this source code instead of a log file you wanted in your logfile monitor,
# check whether the user running this script, has read permission on that log file.
