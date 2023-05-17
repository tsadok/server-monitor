#!/usr/bin/perl
# -*- cperl -*-

# TODO: standardize color handling between monitor.pl, eggs.pl, tsvdb.pl, and utm.pl

use strict;
use utf8;
use Term::Size;
use Carp;

our (%option, %magictext, %bigtextfont, $screen);
our (@widget, $wfocus, $wcount);
our ($widgetlogfile, $screenlogfile, $colorlogfile);
our @namedcolor = named_colors();
our %namedcolor = map { $$_{name} => $_,
                        regularize($$_{name}) => $_
                      } @namedcolor;
our $wfocus;
our $reset = chr(27) . qq{[0m};

# TODO: consider whether we need access to %state
require "./bigtextfonts.pl";

our %standard_widget_handler =
  (
   clock      => sub { doclock(@_) },
   label      => sub { dotext(@_) },
   bigtext    => sub { dobigtext(@_) },
   notepad    => sub { donotepad(@_) },
   logtail    => sub { dologtail(@_) },
   messagelog => sub { domessagelog(@_) },
   ordkey     => sub { doordkey(@_) },
   table      => sub { dotablewidget(@_) },
   diffuse    => sub { dodiffuse(@_); },
  );

our ($xmax, $ymax) = Term::Size::chars *STDOUT{IO};

sub setfocus {
  my ($widget) = @_;
  $$wfocus{bg} = undef;
  $wfocus = $widget;
  $$wfocus{bg} = $option{focusbgcolor};
}

sub rotate_focus { # The user is tabbing from pane to pane.
  my $fid = (ref $wfocus) ? $$wfocus{id} : 0;
  $fid++;
  if ($fid > $wcount) { $fid = 0; }
  while (not grep { $$_{id} eq $fid } @widget) {
    $fid++;
    if ($fid > $wcount) { $fid = 0; }
  }
  setfocus(grep { $$_{id} eq $fid } @widget);
  if ($$wfocus{skipfocus}) {
    # Do not, I repeat, do NOT make all widgets skipfocus.
    rotate_focus();
  }
}

sub redraw_widget {
  my ($w, $s) = @_;
  widgetlog("Using redraw-only mode for widget $$w{id}");
  dowidget($w, $s, "redrawonly" => 1);
}

sub is_standard_widget {
  my ($w) = @_;
  if ($standard_widget_handler{$$w{type}}) {
    return $standard_widget_handler{$$w{type}};
  }
  return;
}

sub dostandardwidget {
  my ($w, $s, @more) = @_;
  my $handler = $standard_widget_handler{$$w{type}};
  if (ref $handler) {
    $handler->($w, $s, @more);
  } else {
    widgetlog("dostandardwidget(): fallthrough, type=$$w{type}, id=$$w{id}");
    dotext($w, $s, @more);
  }
}

sub dotablewidget {
  my ($w, $s, %arg) = @_;
  $$w{table}     ||= [];
  $$w{fieldlist} ||= [];
  doborder($w, $s, %arg);
  dotable($w, $s, $$w{fieldlist}, $$w{table}, %arg);
}

sub dotable {
  my ($w, $s, $fieldlist, $records, %arg) = @_;
  widgetlog("dotable(widget " . $$w{id} . ")");
  $$w{xmax}         ||= $xmax;
  $$w{ymax}         ||= $ymax;
  $$w{contentsizey} ||= $$w{ymax} - ($$w{y} + 2);
  $$w{contentsizex} ||= $$w{xmax} - ($$w{x} + 2);
  open DEGUG, ">", "monitor-debug-table.txt";
  my @field  = @$fieldlist;
  my @record = @$records;
  widgetlog(" fields: @field");
  widgetlog(" record count: " . @record);
  if ($$w{showheader}) {
    my $r = +{ map { $_        => $_,
                       $_ . "fg" => $$w{headerfg} || $$w{fg} || $arg{fg} || $option{fg} || "yellow",
                     } @field };
    unshift @record, $r;
  }
  my %len  = ();
  my @f    = @field;
  my $tlen = 0;
  my $lpad = 0;
  while (@f) { # Use while instead of foreach so we know if we're on the last one or not.
    my $f = shift @f;
    $len{$f} = 0;
    for my $r (@record) {
      my $l = length($$r{$f});
      $len{$f} = $l if $len{$f} < $l;
    }
    $len{$f}++ if (($len{$f} > 0) and (scalar @f)); # Spacing between fields.
    $tlen += $len{$f};
  }
  if ($tlen < $$w{contentsizex}) {
    $lpad = 1;
  }
  my %xoffset = ();
  my $xo = $lpad;
  for my $f (@field) {
    $xoffset{$f} = $xo;
    $xo += $len{$f};
  }
  print DEBUG Dumper( +{ w         => $w,
                         fieldlist => $fieldlist,
                         records   => $records,
                         tlen      => $tlen,
                         lpad      => $lpad,
                         len       => \%len,
                         xoffset   => \%xoffset,
                         %arg,
                       });
  my $yoffset = 0;
  for my $r (@record) {
    $yoffset++;
    if ($yoffset <= $$w{contentsizey}) {
      for my $f (@field) {
        my $x  = $$w{x} + 1 + $xoffset{$f};
        my $y  = $$w{y} + $yoffset;
        my $id = $$w{id} . "_r" . $yoffset . "_" . $f;
        widgetlog(" Table record $yoffset field $f  \tat row $y col $x: \t'$$r{$f}'   \t<<$id>>");
        if (($x <= $xmax) and (($x - $$w{x}) < $$w{contentsizex})) {
          my $text = $$r{$f} || "";
          if (length($text) >= ($xmax - $x)) {
            $text = substr($text, 0, $xmax - $x - 1);
          }
          if (length($text) + $x > ($$w{contentsizex} - $$w{x})) {
            $text = substr($text, 0, $$w{x} + $$w{contentsizex} + 1 - $x);
          }
          my $fg = $$r{$f . "fg"} || $$w{$f . "fg"} || $$w{fg} || $arg{fg} || $option{fg} || "grey";
          # Debugging only; comment out for backward compatibility with old monitor.cfg files
          ## croak "dotable: color depth $depth not supported.  " . Dumper(+{ w  => $w,
          ##                                                                  r  => $r,
          ##                                                                  f  => $f,
          ##                                                                  fg => $fg,
          ##                                                                  id => $id,
          ##                                                                  x  => $x,
          ##                                                                  y  => $y,
          ##                                                                  t  => $text,
          ##                                                                })
          ##   if ((ref $fg) and not (("HASH" eq ref $fg) and $$fg{$depth}));
          dotext(+{ id          => $id,
                    text        => $text,
                    x           => $x,
                    y           => $y,
                    fg          => $fg,
                    bg          => $$w{bg},
                    transparent => $$w{transparent},
                  }, $s);
        }
      }}
  }
}

sub dobigtext {
  my ($w, $s, @more) = @_;
  my $font   = site_bigtext_font($$w{font} || "default") || bigtext_font($$w{font} || "default") || bigtext_font("default");
  my @char   = split //, $$w{text};
  my $height = $$font{height} || 0;
  if (not $height) {
    $height = 1;
    for my $c (@char) {
      my $glyph = $$font{glyph}{$c} || $$font{glyph}{lc $c} || $$font{glyph}{uc $c} || $$font{glyph}{default} || $$font{glyph}{" "};
      $height = $$glyph{height} if $height < $$glyph{height};
    }}
  my $x = $$w{x};
  my $n = 0;
  for my $c (@char) {
    my $glyph = $$font{glyph}{$c} || $$font{glyph}{lc $c} || $$font{glyph}{uc $c} || $$font{glyph}{default} || $$font{glyph}{" "};
    $x += bigtext_glyph($w, $n++, $glyph, $x, $height, $s, @more);
    $x += $$font{spacewidth} || 1;
  }
}
sub bigtext_glyph {
  my ($w, $n, $glyph, $x, $h, $s, @more) = @_;
  my $width = 0;
  for my $dy (0 .. ($h - 1)) {
    my $y = $$w{y} + $dy;
    my $line = $$glyph{content}[$dy];
    $width = length($line) if $width < length{$line};
    dotext(+{ id    => $$w{id} . "_" . $n . "_" . $y,
              text  => $line,
              x     => $x,
              y     => $y,
              fg    => $$w{fg} || $option{fg} || "wheat",
              bg    => $$w{bg} || $option{bg} || "slate",
              transparent => $$w{transparent},
            },
           $s, @more);
  }
  #dotext(+{ id      => $$w{id} . "_" . "glyph" . $n . "_idx",
  #          x       => $x,
  #          text    => chr($$glyph{decimal}),
  #          y       => $$w{y},
  #          fg      => $$w{fg},
  #          bg      => $$w{bg},
  #          transparent => $$w{transparent},
  #        }, $s, @more);
  # To allow for combining diacritics, we have to take the font's word for width.
  return ((defined $$glyph{width}) ? $$glyph{width} : $width);
}
sub bigtext_font {
  my ($fontname) = @_;
  return $bigtextfont{$fontname};
}

sub dologtail {
  my ($w, $s, %more)  = @_;
  $$w{__LINES__}    ||= +[];
  if (not $more{redrawonly}) {
    $$w{logfile}      ||= $0;
    $$w{logfile}        = $0 if not -e $$w{logfile};
    $$w{title}        ||= $$w{logfile};
    $$w{xmax}         ||= $xmax;
    $$w{ymax}         ||= $ymax;
    $$w{contentsizex} ||= $$w{xmax} - $$w{x} - 2;
    $$w{contentsizey} ||= $$w{ymax} - $$w{y} - 2;
    $$w{__PADDING__}  ||= " " x $$w{contentsizex};
    widgetlog(qq[dologtail($$w{id}): attempting to show last $$w{contentsizey} lines of $$w{logfile}]);
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
    $$w{__LINES__} = +[ map { chomp $_; $_ } split /\r?\n/, $tail ];
    widgetlog(qq[dologtail($$w{id}): got ] . @{$$w{__LINES__}} . qq[ lines]);
  }
  if (not $$w{__DIDBORDER__}) {
    widgetlog(qq[dologtail($$w{id}): drawing border]);
    doborder($w, $s, %more);
  }
  $$w{__DIDBORDER__} = $$w{redraw} ? 0 : 1;
  for my $n (1 .. $$w{contentsizey}) {
    my $line = $$w{__LINES__}[$n - 1] || "";
    widgetlog(qq[dologtail($$w{id}): $n:$line]);
    dotext(+{ id          => $$w{id} . "_line" . $n,
              text        => substr(($line . $$w{__PADDING__}), 0, $$w{contentsizex}),
              x           => $$w{x} + 1,
              y           => $$w{y} + $n,
              transparent => $$w{transparent},
              bg          => $$w{bg},
              fg          => $$w{fg} || $option{fg} || "white",
            }, $s, %more);
  }
}

sub rewrap_message_log {
  my ($w) = @_;
  $$w{msgpos}   = 0;
  $$w{linepos}  = 0;
  $$w{xpos}     = 0;
  $$w{lines}[0] = undef; # The others get cleared when wrapping down
                         # onto them, but the first line can be a work
                         # in progress as far as the wrapping code knows.
  wrap_message_log($w);
}

sub wrap_message_log {
  my ($w) = @_;
  my $msgcount = scalar @{$$w{messages} || +[]};
  my $first = $$w{msgpos};
  for my $msgidx ($first .. ($msgcount - 1)) {
    message_log_wrap_message($w, $msgidx);
    $$w{msgpos}++;
  }
}

sub message_log_wrap_message {
  my ($w, $idx) = @_;
  my ($text, $channel) = @{$$w{messages}[$idx]};
  my $color = $option{"channelcolor_" . $channel};
  if (not $color) {
    # In some special cases, a color is specified directly (e.g., the
    # color test that is sent when the color depth setting changes).
    my ($cdef) = grep { $$_{name} eq $channel } @namedcolor;
    $color = $$cdef{name} if ref $cdef;
    # Last resort, go with the most generic color we've got:
    $color ||= "grey";
  }
  my $width = $$w{xmax} - $$w{x} - 2;
  if (not exists $$w{lines}[$$w{linepos}]) {
    $$w{lines}[$$w{linepos}] = [ map { [" ", ""] } 1 .. $width ];
  }
  for my $word (split /\s+/, $text) {
    if (($width < $$w{xpos} + length($word)) and
        ($width >= length($word))) {
      $$w{linepos}++; $$w{xpos} = 0;
      $$w{lines}[$$w{linepos}] = [ map { [" ", ""] } 1 .. $width ];
    }
    for my $char (split //, $word) {
      $$w{lines}[$$w{linepos}][$$w{xpos}] = [$char, $color];
      $$w{xpos}++;
      if ($$w{xpos} >= $width) { # Unavoidably split long word:
        $$w{linepos}++; $$w{xpos} = 0;
        $$w{lines}[$$w{linepos}] = [ map { [" ", ""] } 1 .. $width ];
      }
    }
    if (($$w{xpos} > 0) and ($$w{xpos} < $width)) {
      $$w{lines}[$$w{linepos}][$$w{xpos}] = [" ", $color];
      $$w{xpos}++;
    }
  }
  if ($$w{onemessageperline}) {
    $$w{linepos}++; $$w{xpos} = 0;
  }
}

sub domessagelog {
  my ($w, $s, %more) = @_;
  if ($$w{redraw}) {
    doborder($w, $s);
  }
  wrap_message_log($w); # Adds any new messages.
  my $width = $$w{xmax} - $$w{x} - 2;
  my $height = $$w{ymax} - $$w{y} - 2;
  my @line = @{$$w{lines}};
  # TODO: scrollpos
  while ($height < scalar @line) {
    shift @line;
  }
  my $y = 0;
  for my $l (@line) {
    $y++;
    for my $x (1 .. $width) {
      $$s[$$w{x} + $x][$$w{y} + $y] = +{ bg   => widgetbg($w, "bg", $$s[$$w{x} + $x][$$w{y} + $y]),
                                         fg   => $$l[$x - 1][1] || $more{fg} || $$w{fg} || $option{fg} || "grey",
                                         char => $$l[$x - 1][0],
                                       },
    }
  }
}

sub format_help_info {
  my ($w, $info) = @_;
  my $width = $$w{xmax} - $$w{x} - 2;
  my @line;
  my ($y, $x) = (0, 0);
  for my $word (split /\s+/, $info) {
    if (not exists $line[$y]) {
      $line[$y] = join("", map { " " } 1 .. $width);
    }
    if (($width < $x + length($word)) and
        ($width >= length($word))) {
      $y++; $x = 0;
      $line[$y] = join("", map { " " } 1 .. $width);
    }
    for my $char (split //, $word) {
      substr($line[$y], $x, 1, $char);
      $x++;
      if ($x >= $width) { # Unavoidably split long words:
        $y++; $x = 0;
        $line[$y] = join("", map { " " } 1 .. $width);
      }
    }
    if (($x > 0) and ($x < $width)) {
      substr($line[$y], $x, 1, " ");
      $x++;
    }
  }
  return @line;
}

sub donotepad {
  my ($w, $s, %more) = @_;
  if ((not $$w{xmax}) or (not $$w{ymax})) {
    $$w{cxmax} = $$w{cymax} = 0;
    for my $k (grep { /^line/ } keys %$w) {
      my ($n) = $k =~ /(\d+)/;
      $$w{cymax} = $n if $$w{cymax} < $n;
      if (not $$w{__DID_UNESCAPES__}) {
        $$w{$k} = notepad_unescape($$w{$k});
      }
      $$w{cxmax} = 1 + length($$w{$k}) if $$w{cxmax} <= length($$w{$k});
    }
    $$w{xmax} = $$w{x} + $$w{cxmax} + 2;
    $$w{ymax} = $$w{y} + $$w{cymax} + 2;
    $$w{__DID_UNESCAPES__} = 1;
  }
  $$w{contentsizex} ||= $$w{xmax} - $$w{x} - 2;
  $$w{contentsizey} ||= $$w{ymax} - $$w{y} - 2;
  doborder($w, $s, %more);
  for my $n (1 .. ($$w{ymax} - $$w{y} - 2)) {
    my $id = $$w{id} . "_line" . $n;
    dotext(+{ id          => $id,
              text        => substr((($$w{"line" . $n} || "") .
                                     (" " x ($$w{xmax} - $$w{x} - 2)), 0, ($$w{xmax} - $$w{x} - 2))),
              x           => $$w{x} + 1,
              y           => $$w{y} + $n,
              bg          => $$w{bg},
              fg          => $$w{fg} || $more{fg} || $option{fg} || "grey",
              transparent => $$w{transparent},
            }, $s);
  }
}

sub input_ordkey {
  my ($w, $k) = @_;
  my $n = 1;
  while ($$w{"line" . $n}) { $n++; }
  $$w{"line" . $n} = ord $k;
  # If we hit the bottom of the available space, scroll:
  while ($n + 2 > ($$w{ymax} - $$w{y})) {
    for my $p (1 .. $n) {
      $$w{"line" . $p} = $$w{"line" . ($p + 1)} || undef;
    }
    $n--;
  }
  #draweggscreen($screen, %option);
}

sub doordkey {
  # The content gets changed on keyboard input in a different way,
  # but display is the same as a note pad:
  donotepad(@_);
}

sub doclock {
  my ($w, $s, %arg) = @_;
  my %tzalias = ( localtime => $option{localtimezone} );
  my $dt = $$w{faketime}
    || DateTime->now(
                     time_zone => ($tzalias{lc $$w{tz}} || $$w{tz}
                                   || $tzalias{localtime}
                                   || $option{localtimezone}
                                   || "UTC" ),
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
              sub { my ($x,$y) = @_;
                    ($$w{transparent}
                     ? "__TRANSPARENT__"
                     : widgetbg($w, "bg", $$s[$x][$y],
                                # TODO
                               )) },
              " ", #"░",
              widgetfg($w, "clockfg"));
  }
  $$w{contentsizey} = 1 + ($$w{showdate} ? 1 : 0) + ($$w{showdow} ? 1 : 0);
  $$w{contentsizex} = $clen;
  blankrect($s, $$w{x}, $$w{y}, $$w{x} + $$w{contentsizex} + 1, $$w{y} + $$w{contentsizey} + 1,
            sub { my ($x,$y) = @_;
                  return widgetbg($w, undef, $$s[$x][$y])
                }, " ", # TODO
           );
  doborder($w,$s);
  my $pos = $$w{x} + 1 + (($clen > length($time))
                          ? (int(($clen - length($time)) / 2)) : 0);
  for my $p (@tpart) {
    dotext(+{ id          => $$w{id} . "_" . $$p[0],
              x           => $pos,
              y           => $$w{y} + 1,
              fg          => $$w{$$p[2]} || $$w{fg} || $arg{fg} || $option{fg} || "green",
              bg          => $$w{$$p[3]} || $$w{bg} || $arg{bg} || $option{bg} || "black",
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
              fg          => $$w{dowfg}  || $$w{datefg} || $$w{fg} || $arg{fg} || $option{fg} || "green",
              bg          => $$w{dowbg}  || $$w{datebg} || $$w{bg} || $arg{bg} || $option{bg} || "black",
              text        => $dt->day_name(),
              transparent => $$w{transparent},
            }, $s);
  }
  if ($$w{showdate}) {
    dotext(+{ id          => $$w{id} . "_date",
              x           => $$w{x} + 1 + (($clen > length($date))
                                           ? (int(($clen - length($date)) / 2)) : 0),
              y           => $$w{y} + 2 + ($$w{showdow} ? 1 : 0),
              fg          => $$w{datefg} || $$w{fg} || $arg{fg} || $option{fg} || "green",
              bg          => $$w{datebg} || $$w{bg} || $arg{bg} || $option{bg} || "black",
              text        => $date,
              transparent => $$w{transparent},
            }, $s);
  }
}

sub blankrect {
  my ($s, $minx, $miny, $maxx, $maxy, $bgspec, $c, $fgspec) = @_;
  #croak "blankrect(): called with invalid bg, " . Dumper(+{ minx => $minx, miny => $miny, maxx => $maxx, maxy => $maxy })
  #  if not $bgspec;
  for my $x ($minx .. $maxx) {
    for my $y ($miny .. $maxy) {
      my $fg = $fgspec;
      my $bg = $bgspec;
      if ("CODE" eq ref $bg) {
        $bg = $bg->($x,$y);
        #croak "blankrect(): coderef returned invalid bg"
        #  if not $bg;
        colorlog("blankrect: bg[$x][$y]=$bg");
      }
      if ("CODE" eq ref $fg) {
        $fg = $fg->($x,$y);
      }
      if ($bg eq "__TRANSPARENT__") {
        $bg = $$s[$x][$y]{bg};
        #croak "blankrect(): transparent blanking with nothing behind.  "
        #  . Dumper(+{ x => $x, y => $y, cell => $$s[$x][$y] })
        #  if not $bg;
      }
      $$s[$x][$y] = +{ bg   => $bg,
                       fg   => $fg || $option{fg} || "magenta",
                       char => ((defined $c) ? $c : " "),
                     };
    }}}

sub doborder {
  my ($w, $s) = @_;
  my $fg = widgetfg($w, "borderfg");
  #croak("Undefined bg " . Dumper(+{ x => $$w{x}, y => $$w{y}, cell => $$s[$$w{x}][$$w{y}]})
  #      . Dumper(+{ screen => $s })) if not defined $$s[$$w{x}][$$w{y}]{bg};
  $$s[$$w{x}][$$w{y}] = +{ char => "╔", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y}]) };
  $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y}] = +{ char => "╗", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y}]) };
  $$s[$$w{x}][$$w{y} + $$w{contentsizey} + 1] = +{ char => "╚", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y} + $$w{contentsizey} + 1]) };
  $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $$w{contentsizey} + 1]
    = +{ char => "╝", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $$w{contentsizey} + 1]) };
  for my $x (1 .. $$w{contentsizex}) {
    $$s[$$w{x} + $x][$$w{y}] = +{ char => "═", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $x][$$w{y}]) };
    $$s[$$w{x} + $x][$$w{y} + $$w{contentsizey} + 1] = +{ char => "═", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x} + $x][$$w{y} + $$w{contentsizey} + 1]) };
  }
  for my $y (1 .. $$w{contentsizey}) {
    $$s[$$w{x}][$$w{y} + $y] = +{ char => "║", fg => $fg, bg => widgetbg($w, "borderbg", $$s[$$w{x}][$$w{y} + $y]) };
    $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $y] = +{ char => "║", fg => $fg, bg => $reset . widgetbg($w, "borderbg", $$s[$$w{x} + $$w{contentsizex} + 1][$$w{y} + $y]) };
  }
  if ($$w{title}) {
    dotext(+{ id          => $$w{id} . "_title",
              x           => $$w{x} + 1 + (($$w{contentsizex} > length($$w{title}))
                                           ? (int(($$w{contentsizex} - length($$w{title})) / 2)) : 0),
              y           => $$w{y},
              fg          => (($$w{id} eq $$wfocus{id})
                              ? ( $option{focusfg} || $option{fg} || "white")
                              : $$w{titlefg} || $$w{borderfg} || $$w{fg} || $option{fg} || "white"),
              bg          => $$w{titlebg} || $$w{borderbg} || $$w{bg} || $option{bg} || "black",
              text        => $$w{title},
              transparent => $$w{transparent},
            }, $s);
  }
}

sub dotext {
  my ($t, $s) = @_;
  if (not $$t{__DONE__}) {
    my $text = (defined $$t{text}) ? $$t{text} : $$t{title} || $$t{type} || "t_$$t{id}";
    $text = $magictext{$text} || $text;
    $$t{rows} = 1;
    $$t{cols} = length $text;
    my $x = ($$t{x} >= 0) ? $$t{x} : ($xmax + $$t{x} - $$t{cols});
    for my $c (split //, $text) {
      if ($x < $xmax) {
        $c = " " if $c =~ /\s/;
        $$s[$x][$$t{y}] = +{ bg   => widgetbg($t, undef, $$s[$x][$$t{y}]),
                             fg   => widgetfg($t),
                             char => $c };
        $x++;
      }}
  }
}

sub dodiffuse {
  my ($w, $s, %more) = @_;
  widgetlog(qq[dodiffuse($$w{id})]);
  if (not $more{redrawonly}) {
    $$w{x}            ||= 0;
    $$w{y}            ||= 0;
    $$w{xmax}         ||= $xmax;
    $$w{ymax}         ||= $ymax;
    $$w{contentsizex} ||= 1 + $$w{xmax} - $$w{x};
    $$w{contentsizey} ||= 1 + $$w{ymax} - $$w{y};
    $$w{title}        ||= "Diffuse";
    $$w{fade}         ||= (0.05 + rand(0.5));
    $$w{paintprob}    ||= 5 + int rand 10;
    $$w{fudge}        ||= 65535 + int rand 65535;
    $$w{offset}         = (defined $$w{offset}) ? $$w{offset} : 2; # Allow edges to behave as middle
    $$w{c} ||= 0;
    widgetlog(qq[dodiffuse: ($$w{x},$$w{y})/($$w{xmax},$$w{ymax}); offset=$$w{offset}; fade=$$w{fade}; prob=$$w{paintprob}; fudge=$$w{fudge}; c=$$w{c}]);
    $$w{map} ||= +[ map {
      [ map {
        +{ r => 0, g => 0, b => 0 };
      } 0 .. ($$w{ymax} + 2 * $$w{offset}) ]
    } 0 .. ($$w{xmax} + 2 * $$w{offset}) ];
    if ($$w{preseed} and not $$w{__DID_PRESEED__}) {
      $$w{__DID_PRESEED__}++;
      for (1 .. $$w{preseed}) {
        diffuse_addpaint($w);
      }
    }
  }
  diffuse_draw($w, $s);
  if (not $more{redrawonly}) {
    diffuse_diffuse($w);
    diffuse_addpaint($w) if ($$w{paintprob} > rand 100);
  }
}

sub diffuse_draw {
  my ($w, $s) = @_;
  widgetlog(qq[diffuse_draw($$w{id})]);
  for my $y (0 .. $$w{ymax}) {
    for my $x (0 .. $$w{xmax}) {
      my $mx = $x + $$w{offset};
      my $my = $y + $$w{offset};
      my $r = diffuse_scale($$w{map}[$mx][$my]{r});
      my $g = diffuse_scale($$w{map}[$mx][$my]{g});
      my $b = diffuse_scale($$w{map}[$mx][$my]{b});
      my $colorname = "diffuse($r,$g,$b)";
      $namedcolor{$colorname} ||= +{ name => $colorname,
                                     bg   => +{ 3  => "black",
                                                4  => "black",
                                                8  => rgbto8bit($r,$g,$b),
                                                24 => +{ r => $r,
                                                         g => $g,
                                                         b => $b,
                                                       }},
                                     fg   => +{ 3  => "__default__",
                                                4  => "__default__",
                                                8  => "__default__",
                                                24 => "__default__",
                                              }};
      $$s[$$w{x} + $x][$$w{y} + $y] = +{ bg   => $colorname,
                                         fg   => $option{diffusefg} || $$w{diffusefg} || "black",
                                         char => $option{diffusechar} || $$w{char} || "░", };
    }}
}

sub diffuse_diffuse {
  my ($w) = @_;
  widgetlog(qq[diffuse_diffuse($$w{id})]);
  my @old = map {
    [ map { my $x = $_;
            +{ r => $$x{r}, g => $$x{g}, b => $$x{b}, };
          } @$_ ]
  } @{$$w{map}};
  for my $x (2 .. ($$w{xmax} + (2 * $$w{offset}) - 2)) {
    for my $y (1 .. ($$w{ymax} + (2 * $$w{offset}) - 1)) {
      for my $color (qw(r g b)) {
        $$w{map}[$x][$y]{$color} = abs(((1 * $old[$x - 2][$y - 1]{$color}) +
                                        (2 * $old[$x - 2][$y]{$color})     +
                                        (1 * $old[$x - 2][$y + 1]{$color}) +
                                        (2 * $old[$x - 1][$y - 1]{$color}) +
                                        (3 * $old[$x - 1][$y]{$color})     +
                                        (2 * $old[$x - 1][$y + 1]{$color}) +
                                        (3 * $old[$x][$y - 1]{$color})     +
                                        (5 * $old[$x][$y]{$color})         +
                                        (3 * $old[$x][$y + 1]{$color})     +
                                        (2 * $old[$x + 1][$y - 1]{$color}) +
                                        (3 * $old[$x + 1][$y]{$color})     +
                                        (2 * $old[$x + 1][$y + 1]{$color}) +
                                        (1 * $old[$x + 2][$y - 1]{$color}) +
                                        (2 * $old[$x + 2][$y]{$color})     +
                                        (1 * $old[$x + 2][$y + 1]{$color}))
                                       / (33 + $$w{fade} || 0.1));
        # Fix floating-point underflow:
        $$w{map}[$x][$y]{$color} = int($$w{map}[$x][$y]{$color} * $$w{fudge}) / $$w{fudge};
      }}}
}

sub diffuse_addpaint {
  my ($w) = @_;
  my $c = +{ map { ( $_ => rand 5000 ) } qw(r g b) };
  my $x = $$w{offset} + 1 + int rand($$w{xmax} - 2);
  my $y = $$w{offset} + 1 + int rand($$w{ymax} - 2);
  for (1 .. 1 + int rand 3) {
    widgetlog(qq[diffuse_addpaint($$w{id}): c=<$$c{r},$$c{g},$$c{b}> at ($x,$y)]);
    for my $z (qw(r g b)) {
      $$w{map}[$x][$y]{$z} += $$c{$z};
    }
    $x += (($x > ($$w{xmax} / 2)) ? -1 : 1) * (1 + int rand ($$w{xsplatter} || $$w{splatter} || 5));
    $y += (($y > ($$w{ymax} / 2)) ? -1 : 1) * (1 + int rand ($$w{ysplatter} || $$w{splatter} || 3));
  }
}

sub diffuse_scale {
  my ($v) = @_;
  return 0 if $v <= 0;
  my $ln = log($v * 1000);
  my $sqr = $ln * $ln / 2;
  if ($sqr > 127) {
    return 127;
  } else {
    return int $sqr;
  }
}

sub widgetfg {
  my ($w, $fgfield) = @_;
  #croak(Dumper(+{ option => \%option })) if not $option{colordepth};
  return "default" if $option{colordepth} < 3;
  $fgfield ||= "fg"; $fgfield = "fg" if not $$w{$fgfield};
  colorlog("widgetfg($$w{id}, '$fgfield'): "
           . (("ARRAY" eq ref $$w{$fgfield}) ? "<" . @{$$w{$fgfield}} . ">" : $$w{$fgfield}));
  # Debugging only.  Comment out for backward-compatibility with old monitor.cfg files:
  ## my $depth = $option{colordepth} || 8;
  ## croak "widgetfg: color depth $depth not supported.  " . Dumper(+{ w => $w, fgfield => $fgfield })
  ##   if ((ref $$w{$fgfield}) and not (("HASH" eq ref $$w{$fgfield}) and $$w{$fgfield}{$depth}));
  return $$w{$fgfield} || $option{fg} || "white";
}

sub widgetbg {
  my ($w, $bgfield, $old) = @_;
  #croak "widgetbg(): no third argument" if not defined $old;
  #croak(Dumper(+{ option => \%option })) if not $option{colordepth};
  $old ||= +{ char => "E", fg => 'yellow', bg => 'red' };
  return "default" if $option{colordepth} < 3;
  $bgfield ||= ($$w{id} eq $$wfocus{id}) ? "focusbg" : "bg";
  $bgfield = "bg" if not $$w{$bgfield};
  if ($$w{transparent}
      and ($$w{id} ne ((ref $wfocus) ? ($$wfocus{id} || "__DO_NOT_MATCH__") : "__NO_MATCH__"))) {
    #croak "widgetbg(): improper old cell: $old  " . Dumper(+{ screen => $screen }) . "\nrepeat, widgetbg(): improper old cell: $old  "
    #  if $old and not ref $old;
    if ($$old{bg}) {
      colorlog("widgetbg($$w{id}, '$bgfield'): Transparent widget, keeping bg, "
               . (("ARRAY" eq ref $$old{bg}) ? ("[" . (join ",", @{$$old{bg}}) . "]") : $$old{bg} || "bg"));
      return $$old{bg};
    } else {
      # croak "widgetbg($$w{id}, '$bgfield', " . Dumper($old) . "): Transparent widget with nothing behind it.";
      colorlog("widgetbg($$w{id}, '$bgfield', $old): Transparent widget with nothing behind it.");
      return "magenta";
    }
  }
  colorlog("widgetbg($$w{id}, '$bgfield'): "
           . (("ARRAY" eq ref $$w{$bgfield}) ? "<" . @{$$w{$bgfield}} . ">" : $$w{$bgfield}));
  # Debugging only.  For backward compatibility with old monitor.cfg
  # that specifies colors as rgb-triplets, comment out the next three
  # lines:
  ##my $depth = $option{colordepth} || 8;
  ##croak "widgetbg: color depth $depth not supported.  " . Dumper(+{ w => $w, bgfield => $bgfield })
  ##  if ((ref $$w{$bgfield}) and not (("HASH" eq ref $$w{$bgfield}) and $$w{$bgfield}{$depth}));
  return $$w{$bgfield}  || $$w{bg} || $option{bg} || "black";
}

sub drawscreen {
  my ($s, %arg) = @_;
  if ($arg{debug}) {
    overwritelogfile($screenlogfile,
                     Dumper(+{ __screen__ => $s,
                               __arg__    => \%arg,
                               __named__  => \%namedcolor,
                             }));
  }
  my $depth = $arg{colordepth} || 8;
  if ($arg{nohome}) {
    print $reset . "\n\n";
  } else {
    print chr(27) . "[H" . $reset;
  }
  if ((not $arg{xmax}) or (not $arg{ymax})) {
    ($arg{xmax}, $arg{ymax}) = Term::Size::chars *STDOUT{IO};
    $arg{xmax} ||= 80;
    $arg{ymax} ||= 24;
    $arg{xmax}--; $arg{ymax}--; # Term::Size::chars returns counts, not zero-indexed maxima.
  }
  for my $y (0 .. $arg{ymax}) {
    my $lastbg = "";
    my $lastfg = "";
    for my $x (0 .. $arg{xmax}) {
      my $nbg = $namedcolor{$$s[$x][$y]{bg}} || $namedcolor{bg} || $namedcolor{black};
      my $bgcode = (("ARRAY" eq ref $$s[$x][$y]{bg})
                    ? rgb(@{$$s[$x][$y]{bg}}, "bg")
                    : colorcode($nbg, $depth, "bg") || "");
      my $nfg = $namedcolor{$$s[$x][$y]{fg}} || $namedcolor{fg} || $namedcolor{white};
      my $fgcode = (("ARRAY" eq ref $$s[$x][$y]{fg})
                    ? rgb(@{$$s[$x][$y]{fg}})
                    : colorcode($nfg, $depth) || "");
      my $char   = (length($$s[$x][$y]{char}) ? $$s[$x][$y]{char} : " ");
      drawscreenlog(Dumper(+{ nbg    => $nbg,
                              bgcode => $bgcode,
                              nfg    => $nfg,
                              fgcode => $fgcode,
                              cell   => $$s[$x][$y],
                              char   => $char,
                              x      => $x,
                              xmax   => $arg{xmax},
                              y      => $y,
                              ymax   => $arg{ymax},
                              fr     => $arg{fullrect},
                            })) if $arg{debug};
      print ""
        . ((($$s[$x][$y]{bg} eq $lastbg) and
            ($$s[$x][$y]{fg} eq $lastfg))
           ? "" : $reset . $bgcode . $fgcode)
        . (length($$s[$x][$y]{char}) ? $$s[$x][$y]{char} : " ")
        unless (($x == $arg{xmax}) and
                ($y == $arg{ymax}) and
                (not $arg{fullrect}));
      $lastbg = $$s[$x][$y]{bg};
      $lastfg = $$s[$x][$y]{fg};
    }
    print $reset . "\n" unless (($y >= $arg{ymax}) or not $y);
  }
}

sub colorcode {
  my ($clrdef, $depth, $fgbg) = @_;
  my $ground = $fgbg ? "bg" : "fg";
  if ($$clrdef{$ground}{$depth} eq "__default__") {
    return "";
  } elsif ($depth >= 24) {
    return rgb($$clrdef{$ground}{24}{r},
               $$clrdef{$ground}{24}{g},
               $$clrdef{$ground}{24}{b},
               $fgbg);
  } elsif ($depth >= 8) {
    return eightbitcolorcode($$clrdef{$ground}{8}, $fgbg);
  } elsif ($depth >= 3) {
    use Term::ANSIColor;
    return color $$clrdef{$ground}{$depth};
  } else {
    return ""; # I only bother to support 1-bit color because it is ridiculously easy.
  }
}

sub rgb { # Return terminal code for a 24-bit color.
  my ($red, $green, $blue, $isbg) = @_;
  #colorlog("BUG: rgb($red,$green,$blue,$isbg) called at inappropriate color depth ($option{colordepth} < 24).")
  #  if $option{colordepth} < 24;
  #croak "BUG: rgb($red,$green,$blue,$isbg) called at inappropriate color depth ($option{colordepth} < 24)."
  #  if $option{colordepth} < 24;
  my $fgbg = ($isbg) ? 48 : 38;
  my $delimiter = ";";
  return "\x1b[$fgbg$ {delimiter}2$ {delimiter}$ {red}"
    . "$ {delimiter}$ {green}$ {delimiter}$ {blue}m";
}

sub rgbto8bit {
  # TODO: it might be better to do an hsv-based closest match, which
  #       would be more likely to get the hue closer.
  my ($r, $g, $b) = @_;
  # The r, g, and b parts we've been passed, are intended for 24-bit
  # color, so they have 8 bits each.  We need to downgrade them to
  # just 3 bits each, i.e., instead of ranging 0-255, they need to
  # range only from 0-5.
  my $threebitr = int($r / 42.6);
  my $threebitg = int($g / 42.6);
  my $threebitb = int($b / 42.6);
  return 16 + (36 * $threebitr) + (6 * $threebitg) + ($threebitb);
}
sub bg8 {
  my ($cnum) = @_;
  return eightbitcolorcode($cnum, "bg");
}
sub fg8 {
  my ($cnum) = @_;
  return eightbitcolorcode($cnum);
}
sub eightbitcolorcode {
  my ($cnum, $isbg) = @_;
  my $fgbg = $isbg ? 48 : 38;
  return chr(27) . qq([$fgbg;5;${cnum}m);
}

sub regularize {
  my ($name) = @_;
  my $reg = lc $name;
  $reg =~ s/^[a-z0-9-]+/_/g;
  $reg;
}

sub named_colors {
  # Alphabetical list of keys:
  # a - default (terminal default)
  # c - cyan
  # d - gold
  # e - purple-red
  # g - green
  # h - hot pink
  # i - indigo
  # k - black
  # l - blue
  # m - magenta
  # n - brown
  # o - orange
  # p - pink
  # q - red-orange
  # r - red
  # s - grey (mnemonic: silver)
  # t - teal
  # u - purple
  # v - various (widget-defined)
  # w - white
  # x - spring green
  # y - yellow
  # z - azure
  return map {
    my $c = $_;
    $$c{bg}{3} ||= $$c{bg}{4};
    $$c{fg}{3} ||= $$c{fg}{4};
    $$c{fg}{3} =~ s/bold\s+//;
    $$c{bg}{8} ||= rgbto8bit($$c{bg}{24}{r},$$c{bg}{24}{g},$$c{bg}{24}{b})
      if ref $$c{bg}{24};
    $$c{fg}{8} ||= rgbto8bit($$c{fg}{24}{r},$$c{fg}{24}{g},$$c{fg}{24}{b})
      if ref $$c{fg}{24};
    $c;
  } (# In 4-bit (ANSI) mode, all background colors must be in the
     #             0-7 range, and all foreground colors in 8-15.
     # In 8-bit mode, the rules are not as clearly defined yet.
     #             I should probably do something about that (TODO).
     # In 24-bit mode, no foreground may be darker than 144,144,144
     #             and no background may be lighter than 112,112,112
     #             thus guaranteeing a minimum contrast of 32,32,32,
     #             which happens if someone specifies black on white.
     +{ name => "default",
        key  => "a",
        bg   => +{ 4  => "__default__",
                   8  => "__default__",
                   24 => "__default__",
                 },
        fg   => +{ 4  => "__default__",
                   8  => "__default__",
                   24 => "__default__",
                 },
      },
     +{ name => "white",
        key  => "w",
        bg   => +{ 4  => "on_white",
                   8  => 240,
                   24 => +{ r => 112,
                            g => 112,
                            b => 112, },
                 },
        fg   => +{ 4  => "bold white",
                   8  => 255,
                   24 => +{ r => 255,
                            g => 255,
                            b => 255, },
                 }},
     +{ name   => "grey",
        key    => "s", # "silver"
        bg     => +{ 4  => "on_white",
                     8  => 236,
                     24 => +{ r => 64,
                              g => 64,
                              b => 64, },
                   },
        fg     => +{ 4  => "bold white",
                     8  => 251,
                     24 => +{ r => 200,
                              g => 200,
                              b => 200, },
                   },
      },
     +{ name   => "black",
        key    => "k",
        bg     => +{ 4  => "on_black",
                     8  => 16,
                     24 => +{ r => 0,
                              g => 0,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold black",
                     8  => 243,
                     24 => +{ r => 144,
                              g => 144,
                              b => 144, },
                   },
      },
     +{ name => "red",
        key    => "r",
        bg     => +{ 4  => "on_red",
                     8  => 88,
                     24 => +{ r => 112,
                              g => 0,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold red",
                     8  => 196,
                     24 => +{ r => 255,
                              g => 144,
                              b => 144, },
                   },
      },
     +{ name   => "red-orange",
        key    => "q",
        bg     => +{ 4  => "on_red",
                     8  => 88,
                     24 => +{ r => 112,
                              g => 48,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold red",
                     8  => 202,
                     24 => +{ r => 255,
                              g => 208,
                              b => 144, },
                   },
      },
     +{ name   => "orange",
        key    => "o",
        bg     => +{ 4  => "on_red",
                     8  => 130,
                     24 => +{ r => 112,
                              g => 64,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold red",
                     8  => 208,
                     24 => +{ r => 255,
                              g => 192,
                              b => 144, },
                   },
      },
     +{ name   => "gold",
        key    => "d",
        bg     => +{ 4  => "on_yellow",
                     8  => 94,
                     24 => +{ r => 112,
                              g => 88,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold yellow",
                     8  => 214,
                     24 => +{ r => 255,
                              g => 212,
                              b => 144, },
                   },
      },
     +{ name   => "yellow",
        key    => "y",
        bg     => +{ 4  => "on_yellow",
                     8  => 94,
                     24 => +{ r => 112,
                              g => 112,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold yellow",
                     8  => 11,
                     24 => +{ r => 255,
                              g => 255,
                              b => 144, },
                   }},
     +{ name   => "spring-green",
        key    => "x",
        bg     => +{ 4  => "on_yellow",
                     8  => 58, # or 100,
                     24 => +{ r => 88,
                              g => 96,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold yellow",
                     8  => 190,
                     24 => +{ r => 214,
                              g => 255,
                              b => 144, },
                   }},
     +{ name   => "green",
        key    => "g",
        bg     => +{ 4  => "on_green",
                     8  => 22, # or 28?
                     24 => +{ r => 0,
                              g => 112,
                              b => 0, },
                   },
        fg     => +{ 4  => "bold green",
                     8  => 120, # 190,
                     24 => +{ r => 144,
                              g => 255,
                              b => 144, },
                   }},
     +{ name   => "teal",
        key    => "t",
        bg     => +{ 4  => "on_cyan",
                     8  => 23,
                     24 => +{ r => 0,
                              g => 112,
                              b => 96, },
                   },
        fg     => +{ 4  => "bold cyan",
                     8  => 158,
                     24 => +{ r => 144,
                              g => 255,
                              b => 196, },
                   }},
     +{ name   => "cyan",
        key    => "c",
        bg     => +{ 4  => "on_cyan",
                     8  => 23,
                     24 => +{ r => 0,
                              g => 112,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold cyan",
                     8  => 123,#51,
                     24 => +{ r => 144,
                              g => 255,
                              b => 255, },
                   }},
     +{ name   => "azure",
        key    => "z",
        bg     => +{ 4  => "on_blue",
                     8  => 24,
                     24 => +{ r => 0,
                              g => 85,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold cyan",
                     8  => 45,
                     24 => +{ r => 144,
                              g => 192,
                              b => 255, },
                   }},
     +{ name   => "blue",
        key    => "l",
        bg     => +{ 4  => "on_blue",
                     8  => 18,
                     24 => +{ r => 0,
                              g => 0,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold blue",
                     8  => 21,
                     24 => +{ r => 144,
                              g => 144,
                              b => 255, },
                   }},
     +{ name   => "indigo",
        key    => "i",
        bg     => +{ 4  => "on_blue",
                     8  => 17, # or 54
                     24 => +{ r => 64,
                              g => 0,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold blue",
                     8  => 147,
                     24 => +{ r => 176,
                              g => 144,
                              b => 255, },
                   }},
     +{ name   => "purple",
        key    => "u",
        bg     => +{ 4  => "on_magenta",
                     8  => 53,
                     24 => +{ r => 88,
                              g => 0,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 177,
                     24 => +{ r => 216,
                              g => 144,
                              b => 255, },
                   }},
     +{ name   => "magenta",
        key    => "m",
        bg     => +{ 4  => "on_magenta",
                     8  => 53,
                     24 => +{ r => 112,
                              g => 0,
                              b => 112, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 201,
                     24 => +{ r => 255,
                              g => 144,
                              b => 255, },
                   }},
     +{ name   => "hot pink",
        key    => "h",
        bg     => +{ 4  => "on_magenta",
                     8  => 89,
                     24 => +{ r => 112,
                              g => 0,
                              b => 96, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 199,
                     24 => +{ r => 255,
                              g => 144,
                              b => 236, },
                   }},
     +{ name   => "pink",
        key    => "p",
        bg     => +{ 4  => "on_magenta",
                     8  => 138,
                     24 => +{ r => 112,
                              g => 72,
                              b => 88, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 218,
                     24 => +{ r => 255,
                              g => 160,
                              b => 235, },
                   }},
     +{ name   => "purple-red",
        key    => "e",
        bg     => +{ 4  => "on_magenta",
                     8  => 89,#52,
                     24 => +{ r => 112,
                              g => 32,
                              b => 80, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 197,#162,
                     24 => +{ r => 245,
                              g => 144,
                              b => 180, },
                   }},
     +{ name   => "brown",
        key    => "n",
        bg     => +{ 4  => "on_magenta",
                     8  => 94,#95,
                     24 => +{ r => 88,
                              g => 48,
                              b => 16, },
                   },
        fg     => +{ 4  => "bold magenta",
                     8  => 178,
                     24 => +{ r => 235,
                              g => 172,
                              b => 144, },
                   }},
     +{ name => "slate",
        bg   => +{ 4  => "on_cyan",
                   8  => 23,#59,
                   24 => +{ r => 27,
                            g => 51,
                            b => 49,
                          }},
        fg   => +{ 4  => "bold cyan",
                   8  => 151,
                   24 => +{ r => 146,
                            g => 173,
                            b => 171, }},
      },
     +{ name => "wheat",
        bg   => +{ 4  => "on_white",
                   8  => 101,#179,
                   24 => +{ r => 112,
                            g => 93,
                            b => 56,
                          }},
        fg   => +{ 4  => "bold yellow",
                   8  => 221, # or 229 or 230
                   24 => +{ r => 255,
                            g => 230,
                            b => 188, }}},
    );
}

sub gotoxy {
  my ($x, $y) = @_;
  return "\033[${y};${x}H";
}

42;
