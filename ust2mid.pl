#!/usr/bin/perl
#
#   eCantorix - singing speech synthesis using eSpeak
#   Copyright (C) 2012  Rudolf Polzer
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

use FindBin;
use lib $FindBin::Bin;

use strict;
use warnings;
use MIDI;
use Getopt::Long;
use eCantorix::Util;
use Config::Tiny;

my $UTAU_TICKS = 480;

sub mapnote($$$$)
{
	my ($start, $ev, $channel, $options) = @_;
	my @events = ();
	my $len = $ev->{'Length'};
	my $note = $ev->{'NoteNum'};
	my $velocity = $ev->{'Intensity'};
	my $text = $ev->{'Lyric'};
	if($text ne "R")
	{
		push @events, ['lyric', $start, $text];
		push @events, ['note_on', $start, $channel, $note, $velocity];
		push @events, ['note_off', $start + $len, $channel, $note, $velocity];
	}
	return @events;
}

sub mapusthash($$$)
{
	my ($ini, $channel, $options) = @_;
	my @events = ();
	die "Multitrack UST not supported yet"
		if $ini->{"#SETTING"}->{Tracks} != 1;
	my $tick = 0;
	for my $k(sort { $a <=> $b } map { /^#(\d+)$/ ? ($1) : () } keys %$ini)
	{
		my $ev = $ini->{"#$k"};
		push @events, mapnote $tick, $ev, $channel, $options;
		$tick += $ev->{Length};
	}
	return @events;
}

my $infile = "-";
my $outfile = "-";
my $options = {
};
Getopt::Long::Configure("gnu_getopt", "auto_help", "auto_version");
GetOptions(
	'input|i=s' => \$infile,
	'output|o=s' => \$outfile,
);
my $ini = Config::Tiny->read($infile);
my $utautempo = $ini->{"#SETTING"}->{Tempo};
my $miditempo = int(60000000 / $utautempo + 0.5);
my $ttrack = MIDI::Track->new();
$ttrack->events(['set_tempo', 0, $miditempo]);
my $ntrack = MIDI::Track->new();
$ntrack->events(reltime mapusthash $ini, 1, $options);
my $opus = MIDI::Opus->new({ format => 1, ticks => $UTAU_TICKS, tracks => [$ttrack, $ntrack] });
$opus->write_to_file($outfile)
	or die "MIDI::Opus: could not write $outfile";
